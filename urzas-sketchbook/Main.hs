{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Rendering.OpenGL hiding (Matrix)
import Graphics.Urza.Sketch.Math
import Graphics.Urza.Sketch.Shader.Shape as S
import Graphics.Urza.Sketch.Shader.Text as T
import Graphics.Urza.Sketch.Text.Types
import Graphics.Urza.Sketch.Text.Renderer
import Graphics.Urza.Sketch
import Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Control.Lens
import Control.Monad
import System.Exit
import Data.Maybe


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


data App = App { _appCursor :: Point2d
               , _appPoints :: [Point2d]
               }


newApp :: App
newApp = App (Point2d 0 0) []


main :: IO ()
main = do
    True <- GLFW.init
    defaultWindowHints
    wvar <- makeNewWindow (100,100) (500,500) "Draw API"

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    let load tr = loadCharMap tr $ map toEnum [33..126]
        font = "/Users/schell/Library/Fonts/Proxima Nova Alt Black.otf"
    sshader <- makeShapeShaderProgram
    textR  <- makeTextRenderer font 32 >>= load
    appVar <- newMVar newApp

    forever $ do
        pollEvents
        (events, window) <- takeMVar wvar
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        case getMouseMoveEvent events of
            Just (CursorMoveEvent x y) -> do
                (App _ ps) <- takeMVar appVar
                putMVar appVar $ App (Point2d x y) ps
            _ -> return ()

        case getMouseUpEvent events of
            Just e@(MouseButtonEvent _ _ _) -> do
                print e
                (App c ps) <- takeMVar appVar
                putMVar appVar $ App c $ ps ++ [c]
            _ -> return ()

        when (isJust $ getCharEvent 'r' events) $ do
            (App c _) <- takeMVar appVar
            putMVar appVar (App c [])

        -- Drawing a red square.
        makeContextCurrent $ Just window

        drawShapes sshader (Size winW winH) $ do
            -- Draw the curve.
            stroke $ execNewPath $ do
                setColor $ Color4 0 1 0 1
                curveAlong cachedCurve 40

            -- Draw the control points of the curve.
            forM_ cachedCurve $ \(x, y) ->
                fill $ execNewPath $ do
                    setColor $ Color4 0.5 0.5 0.5 1
                    rectangleAt (x-2) (y-2) 4 4

        let pj = concat $ orthoMatrix 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
            mv = concat $ identityN 4

        currentProgram $= Just (textR^.shader.T.program)
        textR^.shader.T.setProjection $ pj
        textR^.shader.T.setModelview $ mv
        --textR^.shader.setTextColor $ Color4 1 0 0 1
        _ <- drawTextAt' textR (Position 0 0) "Blah blah blah!"


        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess


drawShapes sshader s@(Size w h) m = do
    let pj = concat $ orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv = concat $ identityN 4

    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer, DepthBuffer]
    viewport $= (Position 0 0, s)
    currentProgram $= (Just $ sshader^.S.program)
    sshader^.S.setProjection $ pj
    sshader^.S.setModelview $ mv
    sshader^.setIsTextured $ False
    m



cachedCurve :: [(Double,Double)]
cachedCurve = [ (40.71484375,459.15234375)
              , (74.421875,26.58984375)
              , (89.15234375,28.67578125)
              , (113.5703125,47.74609375)
              , (129.90234375,472.23046875)
              , (146.328125,470.53515625)
              , (174.0,462.62890625)
              , (230.55859375,269.625)
              , (265.7421875,39.74609375)
              , (281.64453125,37.66015625)
              , (295.41796875,48.65234375)
              , (318.046875,293.0859375)
              , (266.3515625,264.58984375)
              , (268.78515625,217.67578125)
              ]


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

