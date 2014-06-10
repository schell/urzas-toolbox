module Urza.Color where

import Graphics.Rendering.OpenGL


black :: (Num a, Fractional a) => Color4 a
black = Color4 0 0 0 1


white :: (Num a, Fractional a) => Color4 a
white = Color4 1 1 1 1


red :: (Num a, Fractional a) => Color4 a
red = Color4 1 0 0 1


green :: (Num a, Fractional a) => Color4 a
green = Color4 0 1 0 1


blue :: (Num a, Fractional a) => Color4 a
blue = Color4 0 0 1 1


yellow :: (Num a, Fractional a) => Color4 a
yellow = Color4 1 1 0 1


transparent :: (Num a, Fractional a) => Color4 a
transparent = Color4 0 0 0 0


alpha :: Fractional a => Color4 a -> a -> Color4 a
alpha (Color4 r g b _) a = Color4 r g b a

