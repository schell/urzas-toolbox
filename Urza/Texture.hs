{-# LANGUAGE TypeFamilies #-}
module Urza.Texture where

import Urza.Types
import Urza.Shader
import Urza.Error
import Urza.Math
import Codec.Picture
import Graphics.Rendering.OpenGL hiding (Matrix)
import Control.Monad
import Control.Lens
import Foreign.Ptr
import System.Exit (exitFailure)
import Data.Vector.Storable         (unsafeWith, Storable)
import Data.Maybe                   (isNothing)
import Linear


makeTextureRenderAndRelease :: ShaderProgram -> TextureObject -> IO (Transform2d Double -> IO (), IO ())
makeTextureRenderAndRelease shdr tex = do
    (vb@(i,_,_),uvb@(j,_,_)) <- bindAndBufferVertsUVs vs uvs
    return (renderTexture vb uvb, deleteObjectNames [i, j])
    where uvs = quad 0 0 1 1
          vs  = uvs
          renderTexture vb uvb tfrm = do bindVertexBuffer vb
                                         bindVertexBuffer uvb
                                         texture Texture2D $= Enabled
                                         activeTexture $= TextureUnit 0
                                         textureBinding Texture2D $= Just tex
                                         shdr^.setIs3d $ False
                                         shdr^.setIsTextured $ True
                                         shdr^.setColorIsReplaced $ False
                                         shdr^.setSampler $ Index1 0
                                         shdr^.setModelview $ toMatrix tfrm
                                         drawArrays Triangles 0 6


-- | Load a temporary texture and do some post processing on it if possible.
loadTempTextureAnd :: FilePath -> (TextureObject -> IO a) -> IO (Maybe a)
loadTempTextureAnd file f = do
    mTex <- loadTexture file
    case mTex of
        Nothing  -> return Nothing
        Just tex -> do
            result <- f tex
            deleteObjectName tex
            return $ Just result


loadTexture :: FilePath -- ^ The texture to load.
            -> IO (Maybe TextureObject)
loadTexture file = do
    texture Texture2D $= Enabled
    -- Load our texture or die.
    mTex <- readTexture file
    unless (isNothing mTex) $ do
        -- Set the texture params on our bound texture.
        setCommonTextureParams
    when (isNothing mTex) $ putStrLn $ "Could not initialize "++ file
    return mTex

setCommonTextureParams :: IO ()
setCommonTextureParams = do
    textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)

readTexture :: FilePath -> IO (Maybe TextureObject)
readTexture f = do
    eDynImg <- readImage f
    case eDynImg of
        Left note  -> do
            putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
            return Nothing

        Right img -> do
            -- Get our texture object.
            tex  <- newBoundTexUnit 0
            -- Buffer our texture data.
            success <- bufferDataIntoBoundTexture img
            unless success $ putStrLn $ "    ("++f++")"
            return $ Just tex


bufferImage :: Image PixelRGBA8 -> IO (Maybe TextureObject)
bufferImage img = do
    t  <- newBoundTexUnit 0
    ok <- unsafeTexImage2D RGBA8 RGBA img
    setCommonTextureParams
    return $ if ok then Just t else Nothing


newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit (fromIntegral u)
    textureBinding Texture2D $= Just tex
    return tex


bufferDataIntoBoundTexture :: DynamicImage -> IO Bool
bufferDataIntoBoundTexture dynImg = case dynImg of
    (ImageRGB8 img)  -> unsafeTexImage2D RGB8 RGB img
    (ImageRGBA8 img) -> unsafeTexImage2D RGBA8 RGBA img
    _                -> do
        putStrLn "Texture is not an expected format (expecting RGB8 or RGBA8)."
        return False


unsafeTexImage2D :: (Storable t1, PixelBaseComponent t ~ t1)
                 => PixelInternalFormat
                 -> PixelFormat
                 -> Image t
                 -> IO Bool
unsafeTexImage2D rb r (Image w h dat) = do
    unsafeWith dat $ \ptr ->
        texImage2D
          Texture2D
          -- No proxy
          NoProxy
          -- No mipmaps
          0
          -- Internal storage @ rgba8
          rb
          -- Size of the image
          (TextureSize2D (fromIntegral w) (fromIntegral h))
          -- No borders
          0
          -- Pixel data in unsigned bytes, rgba order
          (PixelData r UnsignedByte ptr)
    printError
    return True


-- | Renders the IO () action into a framebuffer texture.
renderToTexture :: Size -> PixelInternalFormat -> IO () -> IO TextureObject
renderToTexture (Size w h) fmt ioF = do
    fb <- genObjectName
    bindFramebuffer Framebuffer $= fb

    tex <- genObjectName
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texImage2D
        Texture2D
        NoProxy
        0
        fmt
        (TextureSize2D w h)
        0
        (PixelData RGBA UnsignedByte nullPtr)
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0

    status <- get $ framebufferStatus Framebuffer
    unless (status == Complete) $ do
        print status
        exitFailure

    clearColor $= Color4 0 0 0 0
    clear [ColorBuffer]
    viewport $= (Position 0 0, Size w h)

    ioF
    bindFramebuffer Framebuffer $= defaultFramebufferObject
    deleteObjectName fb
    return tex


drawTexture :: ShaderProgram -> TextureObject -> Transform2d Double -> IO ()
drawTexture shdr tex = drawTextureWithUVs shdr tex uvs
    where uvs = quad 0 0 1 1


drawTextureWithUVs :: ShaderProgram -> TextureObject -> [GLfloat] -> Transform2d Double -> IO ()
drawTextureWithUVs shdr tex uvs tfrm = do
    let --mat = toMatrix tfrm
        vs  = quad 0 0 1 1
    ((i,_,_),(j,_,_)) <- bindAndBufferVertsUVs vs uvs
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    shdr^.setIs3d $ False
    shdr^.setIsTextured $ True
    shdr^.setColorIsReplaced $ False
    shdr^.setSampler $ Index1 0
    shdr^.setModelview $ toMatrix tfrm
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]


sizeOfTexture :: TextureObject -> IO Size
sizeOfTexture tex = do
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    TextureSize2D w h <- get $ textureSize2D Texture2D 0
    return $ Size w h


flipTexture :: ShaderProgram -> TextureObject -> IO TextureObject
flipTexture shdr tex = do
    Size w h <- sizeOfTexture tex

    renderToTexture (Size w h) RGBA' $ do
        let [w',h'] = map fromIntegral [w,h]
            pj = orthoM44 0 w' h' 0 0 1
            mv = scaleM44 w' h' 1
            vs = texQuad 0 0 1 1
            us = quad 0 0 1 1

        clearColor $= Color4 0 0 0 0
        clear [ColorBuffer]
        viewport $= (Position 0 0, Size w h)
        currentProgram $= (Just $ shdr^.program)
        shdr^.setProjection $ pj
        shdr^.setModelview $ mv
        shdr^.setSampler $ Index1 0
        shdr^.setColorIsReplaced $ False
        shdr^.setIsTextured $ True
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just tex
        ((i,_,_),(j,_,_)) <- bindAndBufferVertsUVs vs us
        drawArrays Triangles 0 6
        bindBuffer ArrayBuffer $= Nothing
        deleteObjectNames [i,j]

-- | Draws a source rectangle portion of a texture into a destination rectangle
-- in the currently bound framebuffer.
drawPixels :: ShaderProgram
           -- ^ The shader to use for rendering.
           -> TextureObject
           -- ^ The texture to render.
           -> BoundingBox
           -- ^ The source rectangle. Keep in mind opengl's
           -- texture coordinate space is flipped, with
           -- (0,0) set in the lower left, y increasing upward.
           -> Transform2d Double
           -- ^ The transform to apply.
           -> IO ()
drawPixels shdr t frm = drawPixelsWithM44 shdr t frm . toMatrix

-- | Draws a source rectangle portion of a texture into a destination rectangle
-- in the currently bound framebuffer.
drawPixelsFromTo :: ShaderProgram
                 -- ^ The shader to use for rendering.
                 -> TextureObject
                 -- ^ The texture to render.
                 -> BoundingBox
                 -- ^ The source rectangle. Keep in mind opengl's
                 -- texture coordinate space is flipped, with
                 -- (0,0) set in the lower left, y increasing upward.
                 -> BoundingBox
                 -- ^ The destination rectangle to draw into. (0,0)
                 -- is the upper left, y increasing downward.
                 -> IO ()
drawPixelsFromTo shd tex from' (Rectangle x2 y2 w2 h2) =
    drawPixelsWithM44 shd tex from' mv
        where mv = transM44 x2 y2 0 !*! scaleM44 w2 h2 1

drawPixelsWithM44 :: ShaderProgram
                  -- ^ The shader to use for rendering.
                  -> TextureObject
                  -- ^ The texture to render.
                  -> BoundingBox
                  -- ^ The source rectangle. Keep in mind opengl's
                  -- texture coordinate space is flipped, with
                  -- (0,0) set in the lower left, y increasing upward.
                  -> M44 Double
                  -- ^ The modelview matrix.
                  -> IO ()
drawPixelsWithM44 shd tex from' mv = do
    let u = quad 0 0 1 1
        u'= map realToFrac $ uncurryRectangle texQuad from'
    currentProgram $= Just (shd^.program)
    shd^.setModelview $ mv
    shd^.setIsTextured $ True
    shd^.setColorIsReplaced $ False
    shd^.setSampler $ Index1 0
    ((i,_,_),(j,_,_)) <- bindAndBufferVertsUVs u u'
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]

