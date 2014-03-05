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


initTexture :: FilePath -- ^ The texture to load.
            -> Int      -- ^ The index of the texture unit to hold the texture in.
            -> IO (Maybe TextureObject)
initTexture file u = do
    texture Texture2D $= Enabled
    -- Load our texture or die.
    mTex <- loadTexture file u
    unless (isNothing mTex) $ do
        -- Set the texture params on our bound texture.
        textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
        textureWrapMode Texture2D S $= (Repeated, Clamp)
        textureWrapMode Texture2D T $= (Repeated, Clamp)
    when (isNothing mTex) $ putStrLn $ "Could not initialize "++ file
    return mTex


loadTexture :: FilePath -> Int -> IO (Maybe TextureObject)
loadTexture f u = do
    putStrLn $ "Loading texture "++f
    eDynImg <- readImage f
    case eDynImg of
        Left note  -> do
            putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
            return Nothing

        Right img -> do
            -- Get our texture object.
            tex  <- newBoundTexUnit u
            -- Buffer our texture data.
            success <- bufferDataIntoBoundTexture img
            unless success $ putStrLn $ "    ("++f++")"
            return $ Just tex


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


drawTexture :: (Integral a, Num a, Show a) => ShaderProgram -> TextureObject -> a -> a -> a -> a -> IO ()
drawTexture shd tex x y w h = do
    let [x',y',w',h'] = map fromIntegral [x,y,w,h] :: [GLfloat]
        mv = foldl multiply (identityN 4 :: Matrix GLfloat) [scaleMatrix3d w' h' 1, translationMatrix3d x' y' 0]
        unit = quad 0 0 1 1
        unit'= texQuad 0 0 1 1
    currentProgram $= Just (shd^.program)
    shd^.setModelview $ concat mv
    shd^.setIsTextured $ True
    shd^.setColorIsReplaced $ False
    shd^.setSampler $ Index1 0
    (i,j) <- bindAndBufferVertsUVs unit unit'
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]


drawPixels :: Renderer -> TextureObject -> Rectangle a -> Rectangle a -> IO ()
drawPixels = undefined
