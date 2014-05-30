{-# LANGUAGE TemplateHaskell #-}
module Urza.Types where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent
import System.IO
import           Linear
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW as GLFW
import           Control.Lens
import           Data.Monoid
import qualified Data.IntMap as IM


-- | A rectangle of x y width and height.
data Rectangle a = Rectangle a a a a deriving (Show, Ord, Eq)


instance (Num a, Ord a) => Monoid (Rectangle a) where
    mempty = Rectangle 0 0 0 0
    (Rectangle x1 y1 w1 h1) `mappend` (Rectangle x2 y2 w2 h2) = Rectangle x y w h
        where x  = (min x1 x2)
              y  = (min y1 y2)
              l1 = x1 + w1
              l2 = x2 + w2
              b1 = y1 + h1
              b2 = y2 + h2
              w  = (max l1 l2) - x
              h  = (max b1 b2) - y


type BoundingBox = Rectangle Double


data Point2d = Point2d Double Double deriving (Show, Eq, Ord)


type PathColor = Color4 Double


type Range a = (a, a)


data Path = Path { _pathPoints :: [Point2d]
                 , _pathColors :: [PathColor]
                 , _pathLength :: NumArrayIndices
                 , _pathColor  :: PathColor
                 , _pathPoint  :: Point2d
                 , _pathXBounds:: Range Double
                 , _pathYBounds:: Range Double
                 }
makeLenses ''Path


type BezierCurve = [Point2d]


-- | A function that updates a 4x4 matrix uniform.
type SetUniformMatrix4fv = M44 GLfloat -> IO ()


-- | A function that updates an int uniform.
type SetUniform1i = Index1 GLint -> IO ()


-- | A function that updates a bool uniform.
type SetUniformBool = Bool -> IO ()


-- | A function that updates a color uniform.
type SetUniformColor4f = Color4 GLfloat -> IO ()


data ShaderProgram = ShaderProgram { _program            :: Program
                                   , _setProjection      :: SetUniformMatrix4fv
                                   , _setModelview       :: SetUniformMatrix4fv
                                   , _setSampler         :: SetUniform1i
                                   , _setTextColor       :: SetUniformColor4f
                                   , _setIsTextured      :: SetUniformBool
                                   , _setColorIsReplaced :: SetUniformBool
                                   , _setIs3d            :: SetUniformBool
                                   }
makeLenses ''ShaderProgram


-- | The position of the current character in a text rendering.
type PenPosition = Position


-- | The geometrical vertices and texture mapping uvs of a text rendering.
type BufferGeom = ([GLfloat], [GLfloat])


-- | Glyph metrics normalized 0 <= n <= 1.
data NormalizedGlyphMetrics =
    NormGMetrics { _ngmBearing :: (Rational, Rational) -- ^ The inset in x and y.
                 , _ngmAdvance :: Rational -- ^ The horizontal distance the pen
                                           -- position should travel before
                                           -- drawing the next character.
                 } deriving (Show, Eq)


-- | Metrics and texture sizing for a character.
data FontChar = FontChar { _fcTextureSize   :: Size
                         , _fcTextureOffset :: Position
                         , _fcNormMetrics   :: NormalizedGlyphMetrics
                         } deriving (Show, Eq)
makeLenses ''FontChar


-- | A store of character metrics and texture data.
data Atlas = Atlas { _atlasFontFilePath  :: FilePath
                   , _atlasTextureObject :: TextureObject
                   , _atlasTextureSize   :: Size
                   , _atlasPxSize        :: GLsizei
                   , _atlasMap           :: IM.IntMap FontChar
                   }
makeLenses ''Atlas


-- | Accumulates text geometry.
data BufferAccumulator = BufferAcc { _buffAccAtlas   :: Atlas
                                   , _buffAccGeom    :: BufferGeom
                                   , _buffAccPos     :: Position
                                   , _buffAccXBounds :: Range Double
                                   , _buffAccYBounds :: Range Double
                                   }
makeLenses ''BufferAccumulator


data Renderer = Renderer { _shader :: ShaderProgram
                         , _atlas  :: Atlas
                         }
makeLenses ''Renderer


data Scale = Scale GLfloat GLfloat deriving (Show)


data Rotation = Rotation GLfloat deriving (Show)


data Bitmap = Bitmap { _bitmapTexture :: TextureObject
                     , _bitmapSize    :: Size
                     } deriving (Show)
makeLenses ''Bitmap


data InputEvent = NoInputEvent
                | CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys -- Key, scancode, pressed/released, mods
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)


type WindowVar = MVar ([InputEvent], Window)


data Env = Env { _envEvent :: Maybe InputEvent
               , _envCursorOnScreen :: Bool
               , _envLastCursorPos :: (Double, Double)
               } deriving (Show)
makeLenses ''Env




