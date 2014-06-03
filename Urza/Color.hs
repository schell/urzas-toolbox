module Urza.Color where

import Graphics.Rendering.OpenGL


black :: Color4 GLfloat
black = Color4 0 0 0 1


white :: Color4 GLfloat
white = Color4 1 1 1 1


red :: Color4 GLfloat
red = Color4 1 0 0 1


yellow :: Color4 GLfloat
yellow = Color4 1 1 0 1


transparent :: Color4 GLfloat
transparent = Color4 0 0 0 0


alpha :: Fractional a => Color4 a -> a -> Color4 a
alpha (Color4 r g b _) a = Color4 r g b a

