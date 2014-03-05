module Urza.Window (
    module GLFW,
    InputEvent(..),
    getMouseMoveEvent,
    getMouseUpEvent,
    getKeyUpOf,
    getCharEvent,
    WindowVar,
    initUrza
) where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent
import System.IO

data InputEvent = CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)


getMouseUpEvent :: [InputEvent] -> Maybe InputEvent
getMouseUpEvent = foldl isMouseUpEvent Nothing
    where isMouseUpEvent (Just e) _ = Just e
          isMouseUpEvent _ e@(MouseButtonEvent _ MouseButtonState'Released _) = Just e
          isMouseUpEvent _ _ = Nothing


getMouseMoveEvent :: [InputEvent] -> Maybe InputEvent
getMouseMoveEvent = foldl isMouseMoveEvent Nothing
    where isMouseMoveEvent (Just e) _ = Just e
          isMouseMoveEvent _ e@(CursorMoveEvent _ _) = Just e
          isMouseMoveEvent _ _ = Nothing


getKeyUpOf :: Key -> [InputEvent] -> Maybe InputEvent
getKeyUpOf key = foldl isKeyUpEvent Nothing
    where isKeyUpEvent (Just e) _ = Just e
          isKeyUpEvent Nothing e@(KeyEvent k _ KeyState'Released _) = if key == k then Just e else Nothing
          isKeyUpEvent _ _ = Nothing


getCharEvent :: Char -> [InputEvent] -> Maybe InputEvent
getCharEvent char = foldl isCharEvent Nothing
    where isCharEvent (Just e) _  = Just e
          isCharEvent Nothing e@(CharEvent c) = if char == c then Just e else Nothing
          isCharEvent _ _ = Nothing


type WindowVar = MVar ([InputEvent], Window)


-- | Inject some input into a WindowVar.
input :: WindowVar -> InputEvent -> IO ()
input mvar e = do
    (es, w) <- takeMVar mvar
    putMVar mvar (e:es, w)


-- | Creates a new window. Fails and crashes if no window can be created.
makeNewWindow :: (Int,Int) -> (Int,Int) -> String -> IO WindowVar
makeNewWindow pos size title = do
    Just win <- uncurry createWindow size title Nothing Nothing
    makeContextCurrent $ Just win
    (uncurry $ setWindowPos win) pos

    mvar <- newMVar ([], win)

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


initUrza :: (Int, Int) -> (Int, Int) -> String -> IO WindowVar
initUrza pos size title = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- GLFW.init
    defaultWindowHints
    windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    windowHint $ GLFW.WindowHint'DepthBits 16
    makeNewWindow pos size title

