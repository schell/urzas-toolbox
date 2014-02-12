{-# LANGUAGE OverloadedStrings #-}
module Graphics.Urza.Sketch.Text.Character (
    charIsLoaded,
    geometryForString,
    loadChar,
    loadCharMap,
    loadCharacter,
    drawChar
) where

import           Graphics.Urza.Sketch.Types
import           Graphics.Urza.Sketch.Utils
import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Text.Types
import           Graphics.Urza.Sketch.Text.Font
import           Graphics.Urza.Sketch.Shader.Text
import           Graphics.Rendering.OpenGL hiding (Matrix, Bitmap)
import           Control.Monad.State (execState)
import           Control.Monad
import           Data.Monoid
import           Control.Lens
import qualified Data.IntMap as IM


loadCharMap :: TextRenderer -> String -> IO TextRenderer
loadCharMap r str = foldM loadChar r str


charIsLoaded :: TextRenderer -> Char -> Bool
charIsLoaded r c = let i = fromEnum c in
    case (IM.lookup i $ r^.atlas.atlasMap) of
                         Nothing -> False
                         Just _  -> True


loadChar :: TextRenderer -> Char -> IO TextRenderer
loadChar r c = if charIsLoaded r c then return r else loadCharacter r c


-- | Loads a character into a text renderer's atlas.
-- It does so by rendering a glyph to a texture then draws the original
-- atlas and the glyph texture into a framebuffer that is used as the new
-- atlas texture.
loadCharacter :: TextRenderer -> Char -> IO TextRenderer
loadCharacter r ' ' = return r

loadCharacter r char = do
    let fp       = r^.atlas.atlasFontFilePath
        px       = r^.atlas.atlasPxSize
        Size aW aH = r^.atlas.atlasTextureSize
        aTex     = r^.atlas.atlasTextureObject

    -- Render the glyph into a seperate texture.
    (charTex, (FontChar (Size gW gH) _ gMtrx)) <- texturizeGlyphOfEnum fp px char

    let w  = aW + gW
        h  = max aH gH
        w' = fromIntegral w
        h' = fromIntegral h
        gW' = fromIntegral gW :: GLint
        gH' = fromIntegral gH :: GLint
        gW'' = fromIntegral gW' :: GLfloat
        gH'' = fromIntegral gH' :: GLfloat


    -- Flip the charTex to be right side up.
    charTex' <- renderToTexture (Size gW' gH') R8 $ do
        let pj = orthoMatrix 0 gW'' gH'' 0 0 1 :: Matrix GLfloat
            mv = identityN 4 `multiply` scaleMatrix3d gW'' gH'' 1 :: Matrix GLfloat
            vs = texQuad 0 0 1 1
            us = quad 0 0 1 1

        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer]
        viewport $= (Position 0 0, Size gW' gH')
        currentProgram $= (Just $ r^.shader.program)
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just charTex
        r^.shader.setSampler $ Index1 0
        r^.shader.setTextColor $ Color4 1 0 0 1
        r^.shader.setProjection $ concat pj
        r^.shader.setModelview $ concat mv
        (i,j) <- bindAndBufferVertsUVs vs us
        drawArrays Triangles 0 6
        bindBuffer ArrayBuffer $= Nothing
        deleteObjectNames [i,j]

    -- Then get rid of the old char texture.
    deleteObjectName charTex

    tex <- renderToTexture (Size w' h') R8 $ do
        -- Now render business as usual.
        let pj  = orthoMatrix 0 (fromIntegral w) (fromIntegral h) 0 0 1 :: Matrix GLfloat
            aW' = fromIntegral aW
            aH' = fromIntegral aH

        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer, DepthBuffer]
        viewport $= (Position 0 0, Size w' h')
        currentProgram $= (Just $ r^.shader.program)
        r^.shader.setProjection $ concat pj
        -- We have to render the atlas upside down.
        renderTex r aTex (Position 0 0) (aW', aH')
        renderTex r charTex' (Position (fromIntegral aW) 0) (gW'', gH'')

    deleteObjectName aTex
    deleteObjectName charTex'

    -- Update our atlas.
    return $ flip execState r $ do
        atlas.atlasMap %= IM.insert (fromEnum char) (FontChar (Size gW gH) (Position aW 0) gMtrx)
        atlas.atlasTextureObject .= tex
        atlas.atlasTextureSize .= Size w h


-- | Draws one character at a given pen position using the given renderer.
-- Returns the next pen position.
drawChar :: TextRenderer -> PenPosition -> Char -> IO PenPosition
drawChar r (Position x y) ' ' =
    case IM.lookup 0 $ r^.atlas.atlasMap of
        -- Worst case scenario we advance by the pixel size.
        Nothing -> return (Position (x + r^.atlas.atlasPxSize) y)
        Just c -> return $ advancePenPosition (Position x y) c

drawChar r pen char =
    let mChar = IM.lookup (fromEnum char) $ r^.atlas.atlasMap
    in
    case mChar of
        Nothing -> return pen
        Just fc -> do
            let Atlas _ tex (Size tSw tSh) pxS _ = r^.atlas

            -- Find the scaled (normalized) glyph metrics and use those to
            -- typeset our character.
            -- TODO: Add kerning.
            let (vs, pen') = charVs fc pen pxS --quad x' y' sW sH
                uvs = charUVs fc (fromIntegral tSw, fromIntegral tSh)

            -- Make our geometry vbos.
            (i,j) <- bindAndBufferVertsUVs vs uvs

            -- Do some standard GL texture stuffs and render our quad with
            -- the char's texture.
            texture Texture2D $= Enabled
            activeTexture $= TextureUnit 0
            textureBinding Texture2D $= Just tex
            r^.shader.setSampler $ Index1 0
            r^.shader.setModelview $ concat $ identityN 4

            drawArrays Triangles 0 6
            bindBuffer ArrayBuffer $= Nothing

            deleteObjectNames [i,j]

            return pen'


updateRange :: Ord a => a -> Range a -> Range a
updateRange n (minr, maxr) = (min n minr, max n maxr)


extractRanges :: [Double] -> (Range Double, Range Double)
extractRanges verts = (xr, yr)
    where xr = (foldl min (1/0) xxs, foldl max (-1/0) xxs)
          yr = (foldl min (1/0) yys, foldl max (-1/0) yys)
          (xxs, yys) = split mempty verts


split :: ([a], [a]) -> [a] -> ([a], [a])
split (xs,ys) (x:y:vs) = let (xs',ys') = split mempty vs
                         in (concat [xs, [x], xs'], concat [ys, [y], ys'])
split xs'ys _          = xs'ys


appendRange :: Ord a => Range a -> Range a -> Range a
appendRange a = updateRange (fst a) . updateRange (snd a)


-- | Produces and accumulates the geometry for rendering a string of characters
-- into a buffer accumulator.
geometryForString :: BufferAccumulator -> String -> BufferAccumulator
geometryForString b@(BufferAcc _ _ (Position ox _) _ _) = foldl foldBuffer b
    where foldBuffer b' '\n' = flip execState b' $ do
              pxS <- fmap fromIntegral $ use $ buffAccAtlas.atlasPxSize
              Position _ py <- use buffAccPos
              -- Reset the pen pos x, also drop the pen pos y down a line
              -- and return that result.
              Position px' py' <- buffAccPos <.= Position ox (py + pxS)
              -- Update the current ranges.
              buffAccXBounds %= updateRange (fromIntegral px')
              buffAccYBounds %= updateRange (fromIntegral $ py' + pxS)

          foldBuffer b' c    = accumulateBuffer b' c


-- | Accumulates the geometry of a character into a buffer accumulator.
accumulateBuffer :: Enum a => BufferAccumulator -> a -> BufferAccumulator
accumulateBuffer b@(BufferAcc atls _ (Position penX penY) _ _) c
    -- In the case of ' '.
    | fromEnum c == 32 =
        let pxS = atls^.atlasPxSize
            (Position penX' penY') =
                case IM.lookup 0 $ atls^.atlasMap of
                    -- Update the x pen pos by the px size.
                    Nothing -> (Position (penX + pxS) penY)
                    Just fc -> advancePenPosition (Position penX penY) fc
        in flip execState b $ do
            buffAccPos .= Position penX' penY'
            buffAccXBounds %= updateRange (fromIntegral penX')
            buffAccYBounds %= updateRange (fromIntegral $ penY' + pxS)

    | otherwise = case IM.lookup (fromEnum c) $ atls^.atlasMap of
        Just fc -> flip execState (loadCharGeomIntoBuffer b fc) $ do
            let pxS = atls^.atlasPxSize
            Position x y <- use buffAccPos
            buffAccXBounds %= updateRange (fromIntegral x)
            buffAccYBounds %= updateRange (fromIntegral $ y + pxS)

        -- If there is no character just move the pen forward.
        Nothing -> flip execState b $ do
            let pxS = atls^.atlasPxSize
            -- Update pen pos x and return that result.
            Position x' y' <- buffAccPos <%= \(Position x y) -> (Position (x + pxS) y)
            buffAccXBounds %= updateRange (fromIntegral x')
            buffAccYBounds %= updateRange (fromIntegral $ y' + pxS)


-- | Loads character vertices and uv coords into a buffer accumulator.
loadCharGeomIntoBuffer :: BufferAccumulator -> FontChar -> BufferAccumulator
loadCharGeomIntoBuffer b fc = loadCharUVs (loadCharVs b fc) fc


-- | Loads a list of character vertices into a buffer accumulator.
loadCharVs :: BufferAccumulator -> FontChar -> BufferAccumulator
loadCharVs b fc =
    let px       = b^.buffAccAtlas.atlasPxSize
        (vs, pp) = charVs fc (b^.buffAccPos) px
        (xr, yr) = extractRanges $ map realToFrac vs
    in flip execState b $ do
        buffAccGeom %= (`mappend` (vs, []))
        buffAccPos .= pp
        buffAccXBounds %= (xr `appendRange`)
        buffAccYBounds %= (yr `appendRange`)


-- | Loads a list of character uv coords into a buffer accumulator.
loadCharUVs :: BufferAccumulator -> FontChar -> BufferAccumulator
loadCharUVs b fc =
    let uvs = charUVs fc (w',h')
        Size w h = b^.buffAccAtlas.atlasTextureSize
        w' = fromIntegral w
        h' = fromIntegral h
    in b & buffAccGeom %~ (`mappend` ([], uvs))


-- | Returns the vertex coords of the given character at a pen position
-- and the next pen position.
-- The vertices are in pixel coordinates assuming (0,0) to be upper left.
charVs :: FontChar -> PenPosition -> GLsizei -> ([GLfloat], PenPosition)
charVs fc@(FontChar (Size w h) _ _) pen pxS =
    let Position x' y' = adjustedPenPosForChar pen fc pxS
        x'' = fromIntegral x'
        y'' = fromIntegral y'
        w'  = fromIntegral w
        h'  = fromIntegral h
    in (quad x'' y'' w' h', advancePenPosition pen fc)


-- | Returns the uv coords of the given character with regard to a texture
-- size. The texture atlas has the glyphs flipped in Y.
charUVs :: FontChar -> (GLfloat, GLfloat) -> [GLfloat]
charUVs (FontChar (Size w h) (Position x y) _) (tW,tH) =
    let x' = fromIntegral x/tW
        y' = fromIntegral y/tH
        w' = fromIntegral w/tW
        h' = fromIntegral h/tH
    in texQuad x' y' w' h'


-- | Adjusts the pen position by the characters horizontal and vertical
-- bearing.
adjustedPenPosForChar :: PenPosition -> FontChar -> GLsizei -> PenPosition
adjustedPenPosForChar (Position x y) (FontChar (Size w h) _ (NormGMetrics (bXp, bYp) _)) pxS =
    let prcntY = realToFrac bYp :: Double
        prcntX = realToFrac bXp :: Double
        incX = (fromIntegral w) * prcntX
        incY = (fromIntegral pxS) - (fromIntegral h) * prcntY
    in Position (x + floor incX) (y + floor incY)


-- | Advances the pen position horizontally past the given character.
advancePenPosition :: PenPosition -> FontChar -> PenPosition
advancePenPosition (Position x y) (FontChar (Size w _) _ (NormGMetrics _ advp)) =
    let w'  = fromIntegral w
        adv = advp * w'
    in Position (x + floor adv) y


-- | Renders a texture object at a pen position using the program in the
-- given text renderer.
renderTex :: TextRenderer -> TextureObject -> PenPosition -> (GLfloat, GLfloat) -> IO ()
renderTex r t (Position x y) (w,h) = do
    let scl  = scaleMatrix3d w h 1 :: Matrix GLfloat
        x'  = fromIntegral x
        y'  = fromIntegral y
        tns = translationMatrix3d x' y' 0 :: Matrix GLfloat
        mv  = identityN 4 `multiply` tns `multiply` scl
        vts = texQuad 0 0 1 1
        uvs = texQuad 0 0 1 1
    r^.shader.setModelview $ concat mv
    r^.shader.setSampler $ Index1 0
    r^.shader.setTextColor $ Color4 1 0 0 1
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just t
    (i,j) <- bindAndBufferVertsUVs vts uvs
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]
    bindBuffer ArrayBuffer $= Nothing


