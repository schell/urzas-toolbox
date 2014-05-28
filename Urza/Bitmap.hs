module Urza.Bitmap where

import Urza.Types
import Urza.Texture
import Urza.Math
import Urza.Shader
import Linear
import Control.Lens
import Graphics.Rendering.OpenGL hiding (Bitmap)


loadBitmap :: FilePath -> IO (Maybe Bitmap)
loadBitmap file = do
    mTex <- loadTexture file
    case mTex of
        Nothing -> return Nothing
        Just t  -> do
           sizeOfTexture t >>= return . Just . Bitmap t

emptyBitmap :: IO Bitmap
emptyBitmap = do
    t <- genObjectName
    return $ Bitmap t (Size 0 0)

drawBitmap :: Renderer -> Bitmap -> Position -> Size -> Scale -> Rotation -> IO ()
drawBitmap r bmp = drawBitmapWithUVs r bmp uvs
    where uvs = quad 0 0 1 1


drawBitmapPixels :: Integral a => Renderer -> Bitmap -> Rectangle a -> Position -> Size -> Scale -> Rotation -> IO ()
drawBitmapPixels r b@(Bitmap _ (Size bw bh)) (Rectangle x y w h) = drawBitmapWithUVs r b uvs
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


drawBitmapWithUVs :: Renderer -> Bitmap -> [GLfloat] -> Position -> Size -> Scale -> Rotation -> IO ()
drawBitmapWithUVs r bmp uvs (Position tx ty) (Size w h) (Scale sx sy) (Rotation phi) = do
    let [tx',ty',w',h'] = map fromIntegral [tx,ty,w,h]
        vs  = quad 0 0 1 1
        qtr = axisAngle (V3 0 0 1) phi
        rot = mkTransformationMat (fromQuaternion qtr) $ V3 0 0 0
        trn = mkTransformationMat eye3 $ V3 tx' ty' 0
        siz = m4 w' h'
        scl = m4 sx sy
        mat = trn !*! siz !*! scl !*! rot
        m4 x y = (V4 (V4 x 0 0 0)
                     (V4 0 y 0 0)
                     (V4 0 0 0 0)
                     (V4 0 0 0 1))
    (i,j) <- bindAndBufferVertsUVs vs uvs
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (bmp^.bitmapTexture)
    r^.shader.setIs3d $ False
    r^.shader.setIsTextured $ True
    r^.shader.setColorIsReplaced $ False
    r^.shader.setSampler $ Index1 0
    r^.shader.setModelview $ mat
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]
