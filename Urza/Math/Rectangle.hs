module Urza.Math.Rectangle
(
  Rectangle(..),
  uncurryRectangle,
  unsizeRectangle,
  unposRectangle,
  left,
  right,
  top,
  bottom,
  width,
  height,
  containsRect,
  intersectsRect,
  quadrants,
  union,
  zeroRect,
  areaOf
) where

import Urza.Types
import Graphics.Rendering.OpenGL hiding (Matrix, normalize)

uncurryRectangle :: (a -> a -> a -> a -> b) -> Rectangle a -> b
uncurryRectangle f (Rectangle x y w h) = f x y w h


unsizeRectangle :: RealFrac a => (Size -> b) -> Rectangle a -> b
unsizeRectangle f (Rectangle _ _ w h) = f $ Size (floor w) (floor h)


unposRectangle :: RealFrac a => (Size -> b) -> Rectangle a -> b
unposRectangle f (Rectangle _ _ w h) = f $ Size (floor w) (floor h)


left :: Rectangle a -> a
left (Rectangle x _ _ _) = x

right :: Num a => Rectangle a -> a
right (Rectangle x _ w _) = x + w

top :: Rectangle a -> a
top (Rectangle _ y _ _) = y

bottom :: Num a => Rectangle a -> a
bottom (Rectangle _ y _ h) = y + h

width :: Rectangle a -> a
width (Rectangle _ _ w _) = w

height :: Rectangle a -> a
height (Rectangle _ _ _ h) = h

containsRect :: (Ord a, Num a) => Rectangle a -> Rectangle a -> Bool
containsRect r1 r2 = horizontal && vertical
    where horizontal = left r1 <= left r2 && right r1 > right r2
          vertical = top r1 <= top r2 && bottom r1 > bottom r2

intersectsRect :: (Ord a, Num a) => Rectangle a -> Rectangle a -> Bool
intersectsRect r1 r2 = not separated
    where separated = left r1 >= right r2
                      || left r2 >= right r1
                      || top r1 >= bottom r2
                      || top r2 >= bottom r1


quadrants :: (Num a, Fractional a) => Rectangle a -> (Rectangle a, Rectangle a, Rectangle a, Rectangle a)
quadrants r = (q1, q2, q3, q4)
    where q1 = Rectangle (left r) (top r) w h
          q2 = Rectangle (left r + w) (top r) w h
          q3 = Rectangle (left r) (top r + h) w h
          q4 = Rectangle (left r + w) (top r + h) w h
          w = (width r) / 2
          h = (height r) / 2

union :: (Num a, Ord a) => Rectangle a -> Rectangle a -> Rectangle a
union (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) = Rectangle x y w h
    where x  = (min x1 x2)
          y  = (min y1 y2)
          l1 = x1 + w1
          l2 = x2 + w2
          b1 = y1 + h1
          b2 = y2 + h2
          w  = (max l1 l2) - x
          h  = (max b1 b2) - y

zeroRect :: Num a => Rectangle a
zeroRect = Rectangle 0 0 0 0


areaOf :: (Num a) => Rectangle a -> a
areaOf r = (width r) * (height r)
