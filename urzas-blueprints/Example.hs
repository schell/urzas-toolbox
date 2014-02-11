{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Graphics.Urza.Window as Urza
import           Graphics.Urza.UI
import           Graphics.Urza.Sketch
import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Shader.Text as T
import           Graphics.Urza.Sketch.Shader.Shape as S
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get)
import           Control.Lens
import           Control.Monad
import           System.Exit
import           System.Directory
import           Data.Monoid


data App = App { _appCursor :: Position }


newApp :: App
newApp = App (Position 0 0)


gui :: SceneList
gui = node $ do
    nodeRotation .= Rotation (pi/4)
    nodePosition .= Position 100 0
    drawText $ do
        textWidth .= 32
        textColor .= Color4 1 0 0 0.5
        textFont .= "Deutsch.ttf"
        textString .= "Scene graphs are serious business."
    return $ node $ do
        -- Set some initial goodies.
        nodeSize .= Size 150 150
        nodePosition .= Position 50 50
        -- Do some drawing into the node.
        drawShapes $ strokePath $ do
            setColor $ Color4 1 0 0 1
            moveTo 0 0
            lineTo 150 150
            rectangleAt 0 0 150 150
        -- Return the child nodes.
        return $ (node $ do
                 nodePosition .= Position 50 50
                 drawShapes $ fillPath $ do
                     setColor $ Color4 0 1 0 1
                     rectangleAt 0 0 50 50
                 return [])
             ++
             (node $ do
                 nodePosition .= Position 50 100
                 nodeRotation .= (Rotation (-1))
                 drawShapes $ fillPath $ do
                     setColor $ Color4 0 0 1 1
                     rectangleAt 0 0 50 50
                 return $ node $ do
                              nodePosition .= Position 50 50
                              drawShapes $ fillPath $ do
                                  setColor $ Color4 1 1 0 1
                                  rectangleAt 0 0 50 50
                              drawShapes $ strokePath $ do
                                  setColor $ Color4 0 1 1 1
                                  rectangleAt 0 0 50 50
                              drawText $ do
                                  textFont .= "Deutsch.ttf"
                                  textColor .= Color4 0 1 1 1
                                  textWidth .= 16
                                  textString .= "Serious business."
                              return [])


main :: IO ()
main = do
    wvar <- initUrza (100,100) (800,600) "Purely Functional Scene Graph"
    fontDir <- fmap (++ "/assets/font/") getCurrentDirectory

    tshader <- makeTextShaderProgram
    sshader <- makeShapeShaderProgram

    let renderer = NodeRenderer { _rendererWindowSize  = Size 800 600
                                , _rendererModelview   = identityN 4
                                , _rendererShapeShader = sshader
                                , _rendererTextShader  = tshader
                                , _rendererFontAtlas   = mempty
                                , _rendererFontDir     = fontDir
                                }

    sceneVar <- newMVar $ Scene { _sceneNodeRenderer = renderer
                                , _sceneList = gui
                                }

    forever $ do
        pollEvents
        (_, window) <- takeMVar wvar
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        clearColor $= Color4 0.13 0.13 0.13 1
        clear [ColorBuffer, DepthBuffer]

        -- Render the display list.
        modifyMVar_ sceneVar $ \(Scene r ns) -> do
            -- Render the scene after updating the renderer's window size.
            r' <- renderList (r & rendererWindowSize .~ Size winW winH) ns
            return $ Scene r' ns
            --return $ Scene r ns

        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess


