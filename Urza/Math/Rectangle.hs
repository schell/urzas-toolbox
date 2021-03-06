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
  pointOnRect,
  pointInRect,
  expandRect,
  quadrants,
  union,
  zeroRect,
  areaOf,
  absRect,
  untupleXYRectangle,
  untupleWHRectangle
) where

import Urza.Types
import Linear
import Graphics.Rendering.OpenGL hiding (Matrix, normalize)

uncurryRectangle :: (a -> a -> a -> a -> b) -> Rectangle a -> b
uncurryRectangle f (Rectangle x y w h) = f x y w h


unsizeRectangle :: RealFrac a => (Size -> b) -> Rectangle a -> b
unsizeRectangle f (Rectangle _ _ w h) = f $ Size (floor w) (floor h)


unposRectangle :: RealFrac a => (Size -> b) -> Rectangle a -> b
unposRectangle f (Rectangle _ _ w h) = f $ Size (floor w) (floor h)

untupleXYRectangle :: ((a,a) -> b) -> Rectangle a -> b
untupleXYRectangle f (Rectangle x y _ _) = f (x, y)


untupleWHRectangle :: ((a,a) -> b) -> Rectangle a -> b
untupleWHRectangle f (Rectangle _ _ w h) = f (w, h)


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

pointOnRect :: (Ord a, Num a) => V2 a -> Rectangle a -> Bool
pointOnRect (V2 x y) (Rectangle x1 y1 w h) =
    let (x2,y2) = (x1+w, y1+h)
    in x >= x1 && x <= x2 && y >= y1 && y <= y2

pointInRect :: (Ord a, Num a) => V2 a -> Rectangle a -> Bool
pointInRect (V2 x y) (Rectangle x1 y1 w h) =
    let (x2,y2) = (x1+w, y1+h)
    in x > x1 && x < x2 && y > y1 && y < y2

expandRect :: Num a => Rectangle a -> V2 a -> Rectangle a
expandRect (Rectangle x y w h) (V2 vx vy) = Rectangle (x-vx) (y-vy) (w+2*vx) (h+2*vy) 

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

-- | Returns a rectangle with a non-negative width and height that occupies
-- the same space as the original.
absRect :: (Num a, Ord a) => Rectangle a -> Rectangle a
absRect (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = if w < 0 then x + w else x
          y' = if h < 0 then y + h else y
          w' = abs w
          h' = abs h
