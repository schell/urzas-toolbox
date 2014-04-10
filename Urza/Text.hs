{-# LANGUAGE OverloadedStrings #-}
module Urza.Text (
    module T,
    makeAtlas,
    boundsOfRenderedText,
    sizeOfRenderedText
) where

import           Urza.Text.Character as T
import           Urza.Text.Font as T
import           Urza.Types
import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Control.Lens
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


sizeOfRenderedText :: Atlas -> String -> Size
sizeOfRenderedText atls str =
    let BufferAcc _ _ _ xRange yRange = geometryForString acc str
        acc   = emptyBufferAccumulator atls
        bbw   = xRange^._2 - xRange^._1
        bbh   = yRange^._2 - yRange^._1
    in Size (floor bbw) (floor bbh)


boundsOfRenderedText :: Atlas -> String -> PenPosition -> BoundingBox
boundsOfRenderedText atls str (Position x y) =
    let Size w h = sizeOfRenderedText atls str
        [x',y',w',h'] = map fromIntegral [x,y,w,h]
    in Rectangle x' y' w' h'


