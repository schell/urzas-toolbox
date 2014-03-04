{-# LANGUAGE TemplateHaskell #-}
module Urza.Types where

import           Graphics.Rendering.OpenGL
import           Control.Lens
import qualified Data.IntMap as IM

-- | A rectangle of x y width and height.
data Rectangle a = Rectangle a a a a deriving (Show, Ord, Eq)


uncurryRectangle :: (a -> a -> a -> a -> b) -> Rectangle a -> b
uncurryRectangle f (Rectangle x y w h) = f x y w h


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
type SetUniformMatrix4fv = [GLfloat] -> IO ()


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


