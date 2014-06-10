{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Urza.Types where

import           Prelude hiding ((.), id)
import           Graphics.UI.GLFW as GLFW
import           FRP.Netwire
import           Control.Wire
import           Control.Concurrent
import           Control.Monad.Reader
import           System.IO
import           Linear
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW as GLFW
import           Control.Lens
import           Data.Monoid
import qualified Data.IntMap as IM
import qualified Data.Set as S


class Default a where
    def :: a

-- | A rectangle of x y width and height.
data Rectangle a = Rectangle a a a a deriving (Show, Ord, Eq)


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


type InputEvents_Window = ([InputEvent], Window)


type WindowVar = MVar InputEvents_Window


data InputEnv = InputEnv { _ienvEvent            :: Maybe InputEvent
                         , _ienvCursorOnScreen   :: Bool
                         , _ienvLastCursorPos    :: (Double, Double)
                         , _ienvKeysDown         :: S.Set Key
                         , _ienvMouseButtonsDown :: S.Set MouseButton
                         } deriving (Show)
makeLenses ''InputEnv


instance Default InputEnv where
    def = InputEnv { _ienvEvent = Nothing
                   , _ienvCursorOnScreen = False
                   , _ienvLastCursorPos = (0,0)
                   , _ienvKeysDown = mempty
                   , _ienvMouseButtonsDown = mempty
                   }


data Transform2d = Transform2d { _t2Position :: Position
                               , _t2Size     :: Size
                               , _t2Scale    :: Scale
                               , _t2Rotation :: Rotation
                               } deriving (Show)
makeLenses ''Transform2d


instance Default Transform2d where
    def = Transform2d { _t2Position = Position 0 0
                      , _t2Size = Size 0 0
                      , _t2Scale = Scale 1 1
                      , _t2Rotation = Rotation 0
                      }


type Bitmap_Transform2d = (Bitmap, Transform2d)


type TimeDelta = Timed NominalDiffTime ()


type Timer = Session IO TimeDelta


type UrzaWire en ex a = Wire TimeDelta ex (ReaderT en Identity) a a


type InputReader = ReaderT InputEnv Identity


type InputWire a b = Wire TimeDelta () InputReader a b


-- | An iteration is all the data you need for one frame.
data Iteration env event xcept a =
    Iteration { _iEnv        :: env
              -- ^ The environment wires use to trigger events.
              , _iSession    :: Timer
              -- ^ The current timer.
              , _iData       :: Either xcept a
              -- ^ Our user data (some display object).
              , _iWire       :: UrzaWire env xcept a
              -- ^ A control wire for changing user data over time and
              -- responding to events.
              , _iProcessEv  :: Maybe event -> env -> env
              -- ^ Processes an event into the environment.
              , _iRender     :: Either xcept a -> IO (Either xcept a)
              -- ^ Function for rendering user data and performing IO.
              }
makeLenses ''Iteration


type WindowIteration a = Iteration InputEnv InputEvent () a


instance Default (WindowIteration ()) where
    def = Iteration { _iEnv = def
                    , _iSession = clockSession_
                    , _iData = Left mempty
                    , _iWire = arr id
                    , _iProcessEv = const id
                    , _iRender = return
                    }
