{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urza.Sketch.Text.Renderer (
    makeTextRenderer,
    makeAtlas,
    drawTextAt,
    drawTextAt',
    loadCharMap
) where

import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Text.Character
import           Graphics.Urza.Sketch.Text.Font
import           Graphics.Urza.Sketch.Text.Types
import           Graphics.Urza.Sketch.Types
import           Graphics.Urza.Sketch.Shader.Text
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Control.Monad
import           Control.Lens
import           Data.Monoid
import           System.Directory
import           System.Exit
import qualified Data.IntMap as IM


makeAtlas :: FilePath -> GLsizei -> IO Atlas
makeAtlas fp px = do
    -- Get the missing glyph as a texture and FontChar.
    (tex, fChar) <- texturizeGlyphOfEnum fp px '\NUL'

    -- Store that in our atlas.
    return Atlas { _atlasFontFilePath = fp
                 , _atlasTextureObject = tex
                 , _atlasTextureSize = _fcTextureSize fChar
                 , _atlasPxSize = px
                 , _atlasMap = IM.insert 0 fChar IM.empty
                 }


drawTextAt :: TextRenderer -> PenPosition -> String -> IO ()
drawTextAt r (Position x y) = foldM_ foldCharacter (Position x y)
    where foldCharacter (Position _ y') '\n' = return (Position x (y' + r^.atlas.atlasPxSize))
          foldCharacter p c          = drawChar r p c


drawTextAt' :: TextRenderer -> PenPosition -> String -> IO (Rectangle Double)
drawTextAt' r pen s = do
    let (BufferAcc _ (vs,uvs) _ (l,rt) (t,bm)) = geometryForString emptyAcc s
        emptyAcc     = BufferAcc (r^.atlas) mempty pen (fromIntegral x, -1/0) (fromIntegral y, -1/0)
        Position x y = pen
        numIndices   = floor $ ((fromIntegral $ length vs) / 2.0 :: Double)
    (i,j) <- bindAndBufferVertsUVs vs uvs
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (r^.atlas.atlasTextureObject)
    r^.shader.setSampler $ Index1 0
    drawArrays Triangles 0 numIndices
    bindBuffer ArrayBuffer $= Nothing
    deleteObjectNames [i,j]
    return $ Rectangle l t (rt - l) (bm - t)


makeTextRenderer :: FilePath -> GLsizei -> IO TextRenderer
makeTextRenderer font px = do
    fontExists <- doesFileExist font
    unless fontExists $ do
        putStrLn $ show font ++ " does not exist."
        exitSuccess

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing

    s <- makeTextShaderProgram
    a <- makeAtlas font px

    return TextRenderer { _shader = s
                        , _atlas = a
                        }

