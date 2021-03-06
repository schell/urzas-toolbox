module Urza.Math.Matrix (
    M44,
    V3(..),
    toList,
    orthoM44,
    frustumM44,
    perspectiveM44,
    perspectM44,
    scaleM44,
    transM44,
    vUp,
    vOut,
    vPos,
    vRight
) where

import           Linear
import qualified Data.Foldable as F

-- Linear Helpers

toList :: M44 a -> [a]
toList = F.foldl (\vs (V4 a b c d) -> vs ++ [a,b,c,d]) []


orthoM44 :: (Num a, Fractional a) => a -> a -> a -> a -> a -> a -> M44 a
orthoM44 left right top bottom near far =
    V4 (V4 (2/(right-left)) 0 0 (-(right+left)/(right-left)) )
       (V4 0 (2/(top-bottom)) 0 (-(top+bottom)/(top-bottom)) )
       (V4 0 0 (-2/(far-near)) (-(far+near)/(far-near)) )
       (V4 0 0 0 1)


frustumM44 :: (Floating t, Fractional t) => t -> t -> t -> t -> t -> t -> M44 t
frustumM44 left right top bottom znear zfar =
    let x = (2*znear)       / (right-left)
        y = (2*znear)       / (top-bottom)
        a = (right+left)    / (right-left)
        b = (top+bottom)    / (top-bottom)
        c = -(zfar+znear)   / (zfar-znear)
        d = -(2*zfar*znear) / (zfar-znear)
    in V4 (V4 x 0 a 0)
          (V4 0 y b 0)
          (V4 0 0 c d)
          (V4 0 0 (-1) 0)


perspectiveM44 :: (Floating t, Fractional t) => t -> t -> t -> t -> M44 t
perspectiveM44 fovy aspect near far =
    let top     = near * tan (fovy * (pi / 360.0))
        bottom  = top * (-1)
        left    = bottom * aspect
        right   = top * aspect
    in frustumM44 left right top bottom near far


perspectM44 :: (Floating t, Fractional t) => t -> t -> t -> t -> M44 t
perspectM44 fovy aspect znear zfar =
    let xymax = znear * tan (fovy * pi / 360.0)
        ymin = -xymax
        xmin = -xymax

        width = xymax - xmin
        height = xymax - ymin

        depth = zfar - znear
        q = -(zfar + znear) / depth
        qn = -2 * (zfar * znear) / depth

        w = (2 * znear / width) / aspect
        h = 2 * znear / height
    in V4 (V4 w 0 0 0)
          (V4 0 h 0 0)
          (V4 0 0 q qn)
          (V4 0 0 (-1) 0)


scaleM44 :: Num t => t -> t -> t -> M44 t
scaleM44 x y z = V4 (V4 x 0 0 0)
                    (V4 0 y 0 0)
                    (V4 0 0 z 0)
                    (V4 0 0 0 1)


transM44 :: Num t => t -> t -> t -> M44 t
transM44 x y z = V4 (V4 1 0 0 x)
                    (V4 0 1 0 y)
                    (V4 0 0 1 z)
                    (V4 0 0 0 1)


---- Relative Vectors

vRight :: M44 a -> V3 a
vRight (V4 (V4 x y z _) _ _ _) = V3 x y z


vUp :: M44 a -> V3 a
vUp (V4 _ (V4 x y z _) _ _) = V3 x y z


vOut :: M44 a -> V3 a
vOut (V4 _ _ (V4 x y z _) _) = V3 x y z


vPos :: M44 a -> V3 a
vPos (V4 (V4 _ _ _ x)
         (V4 _ _ _ y)
         (V4 _ _ _ z)
         _) = V3 x y z


