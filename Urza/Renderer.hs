{-# LANGUAGE OverloadedStrings #-}
module Urza.Renderer where

import           Urza.Text
import           Urza.Types
import           Urza.Shader
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Control.Monad
import           Control.Lens
import           Data.Monoid
import           System.Directory
import           System.Exit


makeAsciiRenderer :: FilePath -> GLsizei -> IO Renderer
makeAsciiRenderer fp px = loadText (map toEnum [32..126]) =<< makeRenderer fp px


loadText :: String -> Renderer -> IO Renderer
loadText = flip loadCharMap


drawTextAt :: Renderer -> PenPosition -> String -> IO ()
drawTextAt r (Position x y) s = do
    r^.shader.setIs3d $ False
    foldM_ foldCharacter (Position x y) s
      where foldCharacter (Position _ y') '\n' = return (Position x (y' + r^.atlas.atlasPxSize))
            foldCharacter p c                  = drawChar r p c


drawTextAt' :: Renderer -> PenPosition -> String -> IO BoundingBox
drawTextAt' r pen s = do
    let (BufferAcc _ (vs,uvs) _ (l,rt) (t,bm)) = geometryForString emptyAcc s
        emptyAcc     = BufferAcc (r^.atlas) mempty pen (fromIntegral x, -1/0) (fromIntegral y, -1/0)
        Position x y = pen
        numIndices   = floor $ ((fromIntegral $ length vs) / 2.0 :: Double)
    (i,j) <- bindAndBufferVertsUVs vs uvs
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (r^.atlas.atlasTextureObject)
    r^.shader.setIs3d $ False
    r^.shader.setIsTextured $ True
    r^.shader.setColorIsReplaced $ True
    r^.shader.setSampler $ Index1 0
    drawArrays Triangles 0 numIndices
    bindBuffer ArrayBuffer $= Nothing
    deleteObjectNames [i,j]
    return $ Rectangle l t (rt - l) (bm - t)


makeRenderer :: FilePath -> GLsizei -> IO Renderer
makeRenderer font px = do
    fontExists <- doesFileExist font
    unless fontExists $ do
        putStrLn $ show font ++ " does not exist."
        exitSuccess

    -- TODO: Does this blend stuff have to be here?
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing

    s <- makeShaderProgram
    a <- makeAtlas font px

    return Renderer { _shader = s
                    , _atlas = a
                    }
