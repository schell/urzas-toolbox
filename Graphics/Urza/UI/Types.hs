{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urza.UI.Types where

import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get)
import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Shader.Text as T
import           Graphics.Urza.Sketch.Shader.Shape as S
import           Graphics.Urza.Sketch.Text.Types
import           Graphics.Urza.Sketch.Types
import           Control.Lens
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as I

type SizeAtlas = I.IntMap Atlas


type FontAtlas = M.Map FilePath SizeAtlas


data NodeRenderer = NodeRenderer { _rendererWindowSize  :: Size
                                 , _rendererModelview   :: Matrix GLfloat
                                 , _rendererShapeShader :: ShapeShaderProgram
                                 , _rendererTextShader  :: ShaderProgram
                                 , _rendererFontAtlas   :: FontAtlas
                                 , _rendererFontDir     :: FilePath
                                 }
makeLenses ''NodeRenderer


type BoundingBox = Rectangle Double





-- | The result of a rendering. All rendering happens to a texture first,
-- then can be combined later on.
data RenderResult = RenderResult { _resultNodeRenderer :: NodeRenderer
                                 , _resultTexture      :: TextureObject
                                 , _resultLocalBB      :: BoundingBox
                                 , _resultGlobalBB     :: BoundingBox
                                 }
makeLenses ''RenderResult


-- | A draw takes a NodeRenderer, draws some stuff and returns a new renderer
-- as well as the bounding box that contains the drawing it did.
type Draw = NodeRenderer -> IO RenderResult


data Scale = Scale GLfloat GLfloat


data Rotation = Rotation GLfloat


data SceneText = SceneText { _textFont   :: FilePath
                           , _textWidth  :: Int
                           , _textString :: String
                           , _textColor  :: Color4 GLfloat
                           }
makeLenses ''SceneText


data SceneNodeCache = SceneNodeCache { _nodeLocalBounds     :: BoundingBox
                                     , _nodeGlobalBounds    :: BoundingBox
                                     , _nodeTexture         :: TextureObject
                                     , _nodeGlobalTransform :: Matrix GLfloat
                                     }


data SceneNode = SceneNode { _nodeName         :: String
                           , _nodePosition     :: Position
                           , _nodeSize         :: Size
                           , _nodeScale        :: Scale
                           , _nodeRotation     :: Rotation
                           , _nodeDraws        :: [Draw]
                           , _nodeChildren     :: Int
                           , _nodeCache        :: Maybe SceneNodeCache
                           }
makeLenses ''SceneNode


type SceneList = [SceneNode]


data Scene = Scene { _sceneNodeRenderer :: NodeRenderer
                   , _sceneList         :: SceneList
                   }
makeLenses ''Scene


