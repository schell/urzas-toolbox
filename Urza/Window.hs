module Urza.Window (
    module GLFW,
    initUrza,
    loopUrza,
    processInputEnv
) where

import           Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL
import           Control.Concurrent
import           Control.Lens
import           Control.Applicative
import qualified Control.Monad as M
import qualified Data.Set as S
import           Data.Maybe
import           System.IO
import           System.Exit
import           Urza.Types
import           Urza.Wire.Core


loopUrza :: WindowVar -> Iteration InputEnv InputEvent () a -> IO ()
loopUrza wvar i = do
    -- Execute Urza callbacks and load up events.
    pollEvents

    -- Pop off the oldest event for processing.
    mEvent <- popOldestInputEvent wvar
    --M.when (isJust mEvent) $ print $ fromJust mEvent
    window <- snd <$> readMVar wvar

    -- Pre render setup
    makeContextCurrent $ Just window

    -- Process, update and render our app iteration.
    i' <- stepAndRender i mEvent

    swapBuffers window
    shouldClose <- windowShouldClose window
    M.when shouldClose exitSuccess
    loopUrza wvar i'


initUrza :: (Int, Int) -> (Int, Int) -> String -> IO WindowVar
initUrza pos size title = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- GLFW.init
    defaultWindowHints
    windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    windowHint $ GLFW.WindowHint'DepthBits 16
    makeNewWindow pos size title


-- | Creates a new window. Fails and crashes if no window can be created.
makeNewWindow :: (Int,Int) -> (Int,Int) -> String -> IO WindowVar
makeNewWindow pos size title = do
    Just win <- uncurry createWindow size title Nothing Nothing
    makeContextCurrent $ Just win
    (uncurry $ setWindowPos win) pos

    let (w, h) = over both fromIntegral size
    mvar <- newMVar ([WindowSizeEvent w h, WindowSizeEvent w h], win)

    setCharCallback win $ Just $ \_ c ->
        input mvar $ CharEvent c

    setWindowSizeCallback win $ Just $ \_ w h -> do
        input mvar $ WindowSizeEvent w h

    setKeyCallback win $ Just $ \_ k i ks modi ->
        input mvar $ KeyEvent k i ks modi

    setMouseButtonCallback win $ Just $ \_ mb mbs modi ->
        input mvar $ MouseButtonEvent mb mbs modi

    setCursorPosCallback win $ Just $ \_ x y ->
        input mvar $ CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \_ cs ->
        input mvar $ CursorEnterEvent cs

    setScrollCallback win $ Just $ \_ x y ->
        input mvar $ ScrollEvent x y

    return mvar


-- | Inject some input into a WindowVar.
input :: WindowVar -> InputEvent -> IO ()
input mvar e@(WindowSizeEvent _ _) = do
    (es, w) <- takeMVar mvar
    let es' = filter noWindowEvs es
        noWindowEvs (WindowSizeEvent _ _) = False 
        noWindowEvs _                     = True
    putMVar mvar (es' ++ [e], w)
input mvar e = do
    (es, w) <- takeMVar mvar
    putMVar mvar (es ++ [e], w)


-- | If possible, pops the oldest InputEvent off a WindowVar's events and
-- returns it.
popOldestInputEvent :: WindowVar -> IO (Maybe InputEvent)
popOldestInputEvent wvar = do
    (events, window) <- takeMVar wvar
    let (mEvent, events') = if null events
                              then (Nothing, [])
                              else (Just $ head events, drop 1 events)
    -- Put the rest back for later.
    putMVar wvar (events', window)
    return mEvent


-- | Processes individual events into an input ienvironment.
processInputEnv :: Maybe InputEvent -> InputEnv -> InputEnv
processInputEnv mE@(Just (CursorMoveEvent x y)) ienv =
    ienv & ienvLastCursorPos .~ (x,y) & ienvEvent .~ mE
processInputEnv mE@(Just (CursorEnterEvent cs)) ienv =
    ienv & ienvCursorOnScreen .~ (cs == CursorState'InWindow) & ienvEvent .~ mE
processInputEnv mE@(Just (MouseButtonEvent mb MouseButtonState'Pressed _)) ienv =
    ienv & (ienvMouseButtonsDown %~ S.insert mb) & ienvEvent .~ mE
processInputEnv mE@(Just (MouseButtonEvent mb MouseButtonState'Released _)) ienv =
    ienv & (ienvMouseButtonsDown %~ S.delete mb) & ienvEvent .~ mE
processInputEnv mE ienv = ienv & ienvEvent .~ mE
