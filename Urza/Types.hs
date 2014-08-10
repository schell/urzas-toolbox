{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Urza.Types where

import           Prelude hiding ((.), id)
import           Graphics.UI.GLFW as GLFW
--import           Control.Wire
import           Control.Concurrent
--import           Control.Monad.Reader
import           Control.Monad.State
--import           System.IO
import           Linear
import           Graphics.Rendering.OpenGL
--import           Graphics.UI.GLFW as GLFW
import           Control.Lens
import           Data.Monoid
import qualified Data.IntMap as IM
--import qualified Data.Set as S
import           Urza.Input.Types


class Default a where
    def :: a

-- | A rectangle of x y width and height.
data Rectangle a = Rectangle a a a a deriving (Show, Ord, Eq)


type BoundingBox = Rectangle Double


type PathColor = Color4 Double


type Range a = (a, a)


data Path a = Path { _pathPoints :: [V2 a]
                   , _pathColors :: [PathColor]
                   , _pathLength :: NumArrayIndices
                   , _pathColor  :: PathColor
                   , _pathPoint  :: V2 a
                   , _pathXBounds:: Range a
                   , _pathYBounds:: Range a
                   }
makeLenses ''Path

type PathState a = State (Path a) ()


type BezierCurve a = [V2 a]


-- | A function that updates a 4x4 matrix uniform.
type SetUniformMatrix4fv = M44 Double -> IO ()


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


data Bitmap = Bitmap { _bitmapTexture :: TextureObject
                     , _bitmapSize    :: (Int, Int)
                     } deriving (Show, Eq)
makeLenses ''Bitmap




type InputEvents_Window = ([InputEvent], Window)


type WindowVar = MVar InputEvents_Window



makeLenses ''InputEnv


instance Default InputEnv where
    def = InputEnv { _ienvEvents = []
                   , _ienvCursorOnScreen = False
                   , _ienvLastCursorPos = (0,0)
                   , _ienvKeysDown = mempty
                   , _ienvMouseButtonsDown = mempty
                   }


data Transform a = Transform { _tPosition :: V3 a
                             , _tSize     :: V3 a
                             , _tScale    :: V3 a
                             , _tRotation :: Quaternion a
                             } deriving (Show, Eq)


data Transform2d a = Transform2d { _t2Position :: V2 a
                                 , _t2Size     :: V2 a
                                 , _t2Scale    :: V2 a
                                 , _t2Rotation :: a
                                 } deriving (Show, Eq)
makeLenses ''Transform2d


instance Num a => Default (Transform2d a) where
    def = Transform2d { _t2Position = V2 0 0
                      , _t2Size = V2 0 0
                      , _t2Scale = V2 1 1
                      , _t2Rotation = 0
                      }


type Bitmap_Transform2d a = (Bitmap, Transform2d a)



