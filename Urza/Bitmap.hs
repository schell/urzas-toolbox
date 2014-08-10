module Urza.Bitmap where

import Urza.Types
import Urza.Texture
import Urza.Math
--import Urza.Shader
--import Data.Maybe
--import Linear
--import Control.Lens
import Codec.Picture
import Graphics.Rendering.OpenGL hiding (Bitmap)


withBitmap :: (GLsizei, GLsizei) -> IO () -> IO Bitmap
withBitmap (w,h) ioF = do
    t <- renderToTexture (Size w h) RGBA8 ioF
    return $ Bitmap t (fromIntegral w, fromIntegral h)


imageToBitmap :: Image PixelRGBA8 -> IO (Maybe Bitmap)
imageToBitmap img@(Image w h _) = bufferImage img >>= return . fmap (`Bitmap` (w,h))


makeImageRenderAndRelease :: ShaderProgram -> Image PixelRGBA8 -> IO (Maybe (Transform2d Double -> IO (), IO ()))
makeImageRenderAndRelease shdr img = do
    mBmp <- imageToBitmap img
    case mBmp of
        Nothing  -> return Nothing
        Just (Bitmap t _) -> makeTextureRenderAndRelease shdr t >>= return . Just

loadBitmap :: FilePath -> IO (Maybe Bitmap)
loadBitmap file = do
    mTex <- loadTexture file
    case mTex of
        Nothing -> return Nothing
        Just t  -> do
            let toTuple (Size w h) = (fromIntegral w, fromIntegral h)
            sizeOfTexture t >>= return . Just . Bitmap t . toTuple

emptyBitmap :: IO Bitmap
emptyBitmap = do
    t <- genObjectName
    return $ Bitmap t (0, 0)

drawBitmapPixels :: Integral a => ShaderProgram -> Bitmap -> Rectangle a -> Transform2d Double -> IO ()
drawBitmapPixels shdr (Bitmap t (bw, bh)) (Rectangle x y w h) = drawTextureWithUVs shdr t uvs
    where [bw',bh'] = map fromIntegral [bw,bh]
          [x',y',w',h'] = map fromIntegral [x,y,w,h]
          (xs,ys) = splitxys (quad x' y' w' h') ([],[])
          xs' = map (/bw') xs
          ys' = map (/bh') ys
          uvs = concat $ zipWith (\ex ey -> [ex,ey]) xs' ys' :: [GLfloat]

splitxys :: [a] -> ([a],[a]) -> ([a],[a])
splitxys [] xys = xys
splitxys xys (xs,ys) = let x:y:[] = take 2 xys
                       in splitxys (drop 2 xys) (xs ++ [x], ys ++ [y])

