module Urza.Window (
    module GLFW,
    initUrza,
    --loopUrza,
    foldInput
) where

import           Graphics.UI.GLFW as GLFW
import           Control.Concurrent
import           Control.Lens
import           Linear
import qualified Data.Set as S
import           System.IO
import           Urza.Types
import           Urza.Input.Types


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

    setWindowSizeCallback win $ Just $ \_ w' h' -> do
        input mvar $ WindowSizeEvent w' h'

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

foldInput :: InputEnv -> InputEvent -> InputEnv
foldInput ienv e@(CursorMoveEvent x y) =
    ienv & ienvLastCursorPos .~ (x,y) & ienvEvents %~ (++ [e])
foldInput ienv e@(CursorEnterEvent cs) =
    ienv & ienvCursorOnScreen .~ (cs == CursorState'InWindow) & ienvEvents %~ (++ [e])
foldInput ienv e@(MouseButtonEvent mb MouseButtonState'Pressed _) =
    ienv & (ienvMouseButtonsDown %~ S.insert mb) & ienvEvents %~ (++ [e])
foldInput ienv e@(MouseButtonEvent mb MouseButtonState'Released _) =
    ienv & (ienvMouseButtonsDown %~ S.delete mb) & ienvEvents %~ (++ [e])
foldInput ienv e@(WindowSizeEvent w h) =
    ienv & (ienvWindowSize .~ V2 w h) & ienvEvents %~ (++ [e])
foldInput ienv e = ienv & ienvEvents %~ (++ [e])

