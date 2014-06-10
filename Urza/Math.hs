module Urza.Math (
    module R,
    module M,
    quad,
    texQuad,
    tau
) where

import Urza.Math.Rectangle as R
import Urza.Math.Matrix as M

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
