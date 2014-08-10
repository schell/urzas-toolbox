module Urza.Math (
    module R,
    module M,
    quad,
    texQuad,
    tau,
    toMatrix
) where

import Urza.Math.Rectangle as R
import Urza.Math.Matrix as M
import Urza.Types
import Linear

-- | Returns vertices for a two-tri quad.
-- Assumes (0,0) is the upper left, y increasing downward.
quad :: Num a => a -> a -> a -> a -> [a]
quad x y w h = [x, y, x + w, y, x + w, y + h, x, y, x + w, y + h, x, y + h]


-- | Returns uvs for a two-tri quad.
-- Assumes (0,0) is the lower left, y incresing upward.
texQuad :: Num a => a -> a -> a -> a -> [a]
texQuad x y w h =
    [ x, y + h
    , x + w , y + h
    , x + w, y
    , x, y + h
    , x + w, y
    , x, y
    ]

-- Geometry

tau :: Floating a => a
tau = 2.0 * pi

-- Transform2d
toMatrix :: (Epsilon a, Floating a, Num a) => Transform2d a -> M44 a
toMatrix (Transform2d (V2 tx ty) (V2 w h) (V2 sx sy) phi) =
    trn !*! siz !*! scl !*! rot
    where --vs  = quad 0 0 1 1
          qtr = axisAngle (V3 0 0 1) phi
          rot = mkTransformationMat (fromQuaternion qtr) $ V3 0 0 0
          trn = mkTransformationMat eye3 $ V3 tx ty 0
          siz = m4 w h
          scl = m4 sx sy
          m4 x y = (V4 (V4 x 0 0 0)
                       (V4 0 y 0 0)
                       (V4 0 0 0 0)
                       (V4 0 0 0 1))
