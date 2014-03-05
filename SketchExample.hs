{-# LANGUAGE TemplateHaskell #-}
module Main where

import Urza
import Graphics.Rendering.OpenGL hiding (Matrix)
import Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Control.Lens hiding ((#))
import Control.Monad
import System.Exit
import Data.Maybe
import Diagrams.Prelude as D hiding (blend, stroke)
import Diagrams.Backend.OpenGL
import Diagrams.Backend.OpenGL.CmdLine


data App = App { _appCursor :: Point2d
               , _appPoints :: [Point2d]
               }


newApp :: App
newApp = App (Point2d 0 0) []


main :: IO ()
main = do
    True <- GLFW.init
    defaultWindowHints
    wvar <- makeNewWindow (100,100) (500,500) "Urza's Sketchbook"

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    let load tr = loadCharMap tr $ map toEnum [33..126]
        font = "/Users/schell/Library/Fonts/Proxima Nova Alt Black.otf"
    r  <- makeRenderer font 16 >>= load
    appVar <- newMVar newApp

    dTex <- diagramToTexture (Size 100 100) defaultOptions $
        circle 0.8 # lc blue # lw 0.1

    let shade = r^.shader
        renderDiagram w h =
            do let vs  = quad 0 0 100 100
                   uvs = quad 0 0 1 1
               currentProgram $= Just (shade^.program)
               (i,j) <- bindAndBufferVertsUVs vs uvs
               texture Texture2D $= Enabled
               textureBinding Texture2D $= Just dTex
               shade^.setSampler $ Index1 0
               shade^.setModelview $ concat $ identityN 4
               shade^.setProjection $ concat $ orthoMatrix 0 w 0 h 0 1
               shade^.setIsTextured $ True
               drawArrays Triangles 0 6
               bindBuffer ArrayBuffer $= Nothing
               deleteObjectNames [i,j]
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

        makeContextCurrent $ Just window
        clearColor $= Color4 0.13 0.13 0.13 1
        clear [ColorBuffer, DepthBuffer]

        renderDiagram (fromIntegral winW) (fromIntegral winH)

        --Rectangle x y w h <- drawShapes shade (Size winW winH) $ do
        --    -- Draw the cached curve.
        --    stroke $ execNewPath $ do
        --        setColor $ Color4 0.76 0.8 0.76 1
        --        curveAlong cachedCurve 40

        --let pj = concat $ orthoMatrix 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        --    mv = concat $ identityN 4

        --currentProgram $= Just (textR^.shade.T.program)
        --textR^.shade.T.setProjection $ pj
        --textR^.shade.T.setModelview $ mv
        --textR^.shade.setTextColor $ Color4 0.76 0.76 0.76 1
        --let drawText = drawTextAt' textR (Position 10 10) $
        --        concat [ "Drawing operations return\n"
        --               , "the bounding box that contains the draw.\n"
        --               , "So you can measure things!"
        --               ]
        --Rectangle tx ty tw th <- drawText

        --drawShapes shade (Size winW winH) $ do
        --    fillPath_ $ do
        --        setColor $ Color4 0.33 0.33 0.33 1
        --        rectangleAt tx ty tw th
        --    strokePath_ $ do
        --        setColor $ Color4 0.76 0.76 0.76 1
        --        rectangleAt tx ty tw th

        --    fillPath_ $ do
        --        setColor $ Color4 0.33 0.33 1 0.3
        --        rectangleAt x y w h

        --currentProgram $= Just (textR^.shade.T.program)
        --_ <- drawText

        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess


drawShapes :: ShaderProgram -> Size -> IO b -> IO b
drawShapes shade s@(Size w h) m = do
    let pj = concat $ orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv = concat $ identityN 4

    viewport $= (Position 0 0, s)
    currentProgram $= (Just $ shade^.program)
    shade^.setProjection $ pj
    shade^.setModelview $ mv
    shade^.setIsTextured $ False
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


