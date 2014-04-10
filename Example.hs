{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Urza
import qualified Urza.Color as Color
--import           Graphics.Urza.UI
--import           Graphics.Urza.Sketch
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#))
import           Control.Monad
import           System.Exit
import           System.Directory
import           System.FilePath ((</>))


--data App = App { _appCursor :: Position }


--newApp :: App
--newApp = App (Position 0 0)


--gui :: SceneList
--gui = node $ do
--    nodeRotation .= Rotation (pi/4)
--    nodePosition .= Position 100 0
--    drawText $ do
--        textWidth .= 32
--        textColor .= Color4 1 0 0 0.5
--        textFont .= "Deutsch.ttf"
--        textString .= "Scene graphs are serious business."
--    return $ node $ do
--        -- Set some initial goodies.
--        nodeSize .= Size 150 150
--        nodePosition .= Position 50 50
--        -- Do some drawing into the node.
--        drawShapes $ strokePath $ do
--            setColor $ Color4 1 0 0 1
--            moveTo 0 0
--            lineTo 150 150
--            rectangleAt 0 0 150 150
--        -- Return the child nodes.
--        return $ (node $ do
--                 nodePosition .= Position 50 50
--                 drawShapes $ fillPath $ do
--                     setColor $ Color4 0 1 0 1
--                     rectangleAt 0 0 50 50
--                 return [])
--             ++
--             (node $ do
--                 nodePosition .= Position 50 100
--                 nodeRotation .= (Rotation (-1))
--                 drawShapes $ fillPath $ do
--                     setColor $ Color4 0 0 1 1
--                     rectangleAt 0 0 50 50
--                 return $ node $ do
--                              nodePosition .= Position 50 50
--                              drawShapes $ fillPath $ do
--                                  setColor $ Color4 1 1 0 1
--                                  rectangleAt 0 0 50 50
--                              drawShapes $ strokePath $ do
--                                  setColor $ Color4 0 1 1 1
--                                  rectangleAt 0 0 50 50
--                              drawText $ do
--                                  textFont .= "Deutsch.ttf"
--                                  textColor .= Color4 0 1 1 1
--                                  textWidth .= 16
--                                  textString .= "Serious business."
--                              return [])



main :: IO ()
main = do
    let txt = "Hey there!\nHello."
    wvar <- initUrza (100,100) (800,600) "Purely Functional User Interface"

    fontDir <- fmap (</> "assets" </> "font") getCurrentDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory

    r <- loadText txt =<< makeRenderer (fontDir </> "Deutsch.ttf") 128
    Just (bmp@(Bitmap t size)) <- loadBitmap (imgDir </> "oryx_roguelike_16x24.png")

--    scene <- newScene (Size 800 600) fontDir gui
--    sceneVar <- newMVar scene

    forever $ do
        pollEvents
        (_, window) <- takeMVar wvar
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        clearColor $= Color.black
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setProjection $ concat $ orthoMatrix 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        drawTexture (_shader r) t $ Rectangle 0 0 100 100

        -- Render the display list.
--        modifyMVar_ sceneVar $ renderScene (Size winW winH)
        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess
        --threadDelay 10


