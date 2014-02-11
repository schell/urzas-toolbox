{-# LANGUAGE TemplateHaskell #-}
module Graphics.Urza.UI where

import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get)
import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Shader.Text as T
import           Graphics.Urza.Sketch.Shader.Shape as S
import           Graphics.Urza.Sketch.Text.Renderer
import           Graphics.Urza.Sketch.Text.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as I


type SizeAtlas = I.IntMap Atlas


type FontAtlas = M.Map FilePath SizeAtlas


lookupAtlas :: FilePath -> Int -> FontAtlas -> Maybe Atlas
lookupAtlas font size fAtlas = do
    sizeAtlas <- M.lookup font fAtlas
    I.lookup size sizeAtlas


insertAtlas :: FilePath -> Int -> Atlas -> FontAtlas -> FontAtlas
insertAtlas font size atls fAtls = M.insert font sizeAtls' fAtls
    where  mSizeAtls = M.lookup font fAtls
           sizeAtls  = case mSizeAtls of
                           Nothing -> mempty
                           Just a  -> a
           sizeAtls' = I.insert size atls sizeAtls


data NodeRenderer = NodeRenderer { _rendererWindowSize  :: Size
                                 , _rendererModelview   :: Matrix GLfloat
                                 , _rendererShapeShader :: ShapeShaderProgram
                                 , _rendererTextShader  :: TextShaderProgram
                                 , _rendererFontAtlas   :: FontAtlas
                                 , _rendererFontDir     :: FilePath
                                 }
makeLenses ''NodeRenderer


type Draw = NodeRenderer -> IO NodeRenderer


data Scale = Scale GLfloat GLfloat


data Rotation = Rotation GLfloat


data SceneText = SceneText { _textFont   :: FilePath
                               , _textWidth  :: Int
                               , _textString :: String
                               , _textColor  :: Color4 GLfloat
                               }
makeLenses ''SceneText


emptySceneText :: SceneText
emptySceneText = SceneText { _textFont = ""
                               , _textWidth = 12
                               , _textString = ""
                               , _textColor = Color4 0 0 0 1
                               }


data SceneNode = SceneNode { _nodeName      :: String
                           , _nodeTexture   :: Maybe TextureObject
                           , _nodePosition  :: Position
                           , _nodeSize      :: Size
                           , _nodeScale     :: Scale
                           , _nodeRotation  :: Rotation
                           , _nodeDraws     :: [Draw]
                           , _nodeChildren  :: Int
                           }
makeLenses ''SceneNode


emptyNode :: SceneNode
emptyNode = SceneNode { _nodeName = ""
                      , _nodeTexture = Nothing
                      , _nodePosition = Position 0 0
                      , _nodeSize = Size 0 0
                      , _nodeScale = Scale 1 1
                      , _nodeRotation = Rotation 0
                      , _nodeDraws = []
                      , _nodeChildren = 0
                      }


nodeTransform :: SceneNode -> Matrix GLfloat
nodeTransform n =
    let Position tx ty = n^.nodePosition
        Rotation rz    = n^.nodeRotation
        Scale sx sy    = n^.nodeScale
        t = translationMatrix3d (fromIntegral tx) (fromIntegral ty) 0.0
        r = rotationMatrix3d 0 0 rz
        s = scaleMatrix3d sx sy 1
    in foldl multiply (identityN 4) [t,r,s]


type SceneList = [SceneNode]


node :: State SceneNode SceneList -> SceneList
node s = let (list,n) = runState s emptyNode
             n' = n{_nodeChildren = length list}
         in n':list


drawText :: State SceneText () -> State SceneNode ()
drawText s = nodeDraws %= (++ [renderText (execState s emptySceneText)])


data Scene = Scene { _sceneNodeRenderer :: NodeRenderer
                   , _sceneList         :: SceneList
                   }


renderText :: SceneText -> Draw
renderText t r = do
    let Size w h = r^.rendererWindowSize
        font     = r^.rendererFontDir ++ t^.textFont
        size     = t^.textWidth
        pj = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv = r^.rendererModelview
        ts = r^.rendererTextShader
        mA = lookupAtlas font size (r^.rendererFontAtlas)
    -- Get a text renderer.
    tr <- case mA of
        Just a  -> return $ TextRenderer ts a
        Nothing -> do atls <- makeAtlas font $ fromIntegral size
                      return $ TextRenderer ts atls
    -- Load that text up into the renderer.
    tr' <- loadCharMap tr $ t^.textString
    -- Set up the draw.
    currentProgram $= Just (ts^.T.program)
    ts^.T.setProjection $ concat pj
    ts^.T.setModelview $ concat mv
    ts^.setTextColor $ t^.textColor
    Size w h <- drawTextAt' tr (Position 0 0) (t^.textString)
    print (w,h)
    return $ r & rendererFontAtlas %~ (insertAtlas font size (tr'^.atlas))


drawShapes :: IO () -> State SceneNode ()
drawShapes f = nodeDraws %= (++ [renderShapes f])


renderShapes :: IO () -> Draw
renderShapes f r = do
    let Size w h = r^.rendererWindowSize
        pj  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv  = r^.rendererModelview
        s   = r^.rendererShapeShader
    currentProgram $= Just (s^.S.program)
    s^.setIsTextured $ False
    s^.S.setProjection $ concat pj
    s^.S.setModelview $ concat mv
    f
    return r


renderList :: NodeRenderer -> SceneList -> IO NodeRenderer
renderList r [] = return r
renderList r (n:list) = do
    let nodes = take (n^.nodeChildren) list
        rest  = drop (n^.nodeChildren) list
        mv    = r^.rendererModelview
        r'    = r & rendererModelview %~ (`multiply` nodeTransform n)
    -- Draw this node and update our renderer.
    -- This allows us to load fonts and text as we go along.
    r'' <- foldM (\rndr f -> f rndr) r' (n^.nodeDraws)
    -- Draw subnodes with this modelview.
    r''' <- renderList r'' nodes
    -- Draw the rest of the nodes without this modelview.
    renderList (r''' & rendererModelview .~ mv) rest


