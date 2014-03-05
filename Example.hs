{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Urza
--import           Graphics.Urza.UI
--import           Graphics.Urza.Sketch
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get)
import           Control.Lens hiding ((#))
import           Control.Monad
import           System.Exit
import           System.Directory


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
drawTexture :: (Integral a, Num a, Show a) => Renderer -> TextureObject -> a -> a -> a -> a -> IO ()
drawTexture rnd tex x y w h = do
    let [x',y',w',h'] = map fromIntegral [x,y,w,h] :: [GLfloat] 
        mv = foldl multiply (identityN 4 :: Matrix GLfloat) [scaleMatrix3d w' h' 1, translationMatrix3d x' y' 0]
        unit = quad 0 0 1 1 
        unit'= texQuad 0 0 1 1
    currentProgram $= Just (rnd^.shader.program)
    rnd^.shader.setModelview $ concat mv
    rnd^.shader.setIsTextured $ True
    rnd^.shader.setColorIsReplaced $ False
    rnd^.shader.setSampler $ Index1 0
    (i,j) <- bindAndBufferVertsUVs unit unit' 
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]


main :: IO ()
main = do
    let txt = "Hey there!\nHello."
    wvar <- initUrza (100,100) (800,600) "Purely Functional User Interface"

    fontDir <- fmap (++ "/assets/font/") getCurrentDirectory
    r <- makeRenderer (fontDir ++ "/" ++ "Deutsch.ttf") 128 >>=
        flip loadCharMap txt
    let bounds = boundsOfRenderedText r txt (Position 0 0)

        
    currentProgram $= Just (r^.shader.program)
    r^.shader.setProjection $ concat $ orthoMatrix 0 800 (-1) 600 0 1
    r^.shader.setModelview $ concat $ identityN 4

    r^.shader.setIsTextured $ False
    r^.shader.setColorIsReplaced $ False

    stex <- unsizeRectangle renderToTexture bounds RGBA' $ do
        fillPath_ $ do
            setColor $ Color4 1 0 0 1
            uncurryRectangle rectangleAt bounds
        strokePath_ $ do
            setColor $ Color4 1 1 1 1
            uncurryRectangle rectangleAt bounds

    r^.shader.setIsTextured $ True
    r^.shader.setColorIsReplaced $ True
    r^.shader.setTextColor $ Color4 1 1 1 1

    ttex <- unsizeRectangle renderToTexture bounds RGBA' $ do 
        drawTextAt r (Position 0 0) txt

--    scene <- newScene (Size 800 600) fontDir gui
--    sceneVar <- newMVar scene

    forever $ do
        pollEvents
        (_, window) <- takeMVar wvar
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        clearColor $= Color4 0.13 0.13 0.13 1
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setProjection $ concat $ orthoMatrix 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        
        drawTexture r stex 0 0 800 600  
        drawTexture r ttex 0 0 800 600  
        
        -- Render the display list.
--        modifyMVar_ sceneVar $ renderScene (Size winW winH)
        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess
        --threadDelay 10


