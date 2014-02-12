{-# LANGUAGE TemplateHaskell #-}
module Graphics.Urza.UI (
    -- Scene
    Scene(..),
    renderScene,
    SceneList,
    newScene,
    Rotation(..),
    -- NodeRenderer
    -- SceneText
    textFont,
    textWidth,
    textString,
    textColor,
    -- SceneNode
    node,
    nodeName,
    nodePosition,
    nodeSize,
    nodeScale,
    nodeRotation,
    drawShapes,
    drawText
) where

import           Graphics.Urza.UI.Types
import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Shader.Text as T
import           Graphics.Urza.Sketch.Shader.Shape as S
import           Graphics.Urza.Sketch.Text.Renderer
import           Graphics.Urza.Sketch.Text.Types
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get)
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as I


newScene :: Size -> FilePath -> SceneList -> IO Scene 
newScene windowSize fontDir gui = do
    tshader <- makeTextShaderProgram
    sshader <- makeShapeShaderProgram

    let renderer = NodeRenderer { _rendererWindowSize  = windowSize 
                                , _rendererModelview   = identityN 4
                                , _rendererShapeShader = sshader
                                , _rendererTextShader  = tshader
                                , _rendererFontAtlas   = mempty
                                , _rendererFontDir     = fontDir
                                }

    return $ Scene { _sceneNodeRenderer = renderer
                   , _sceneList = gui
                   }


renderScene :: Size -> Scene -> IO Scene
renderScene (Size w h) scene = do
    let nodes    = scene^.sceneList
        renderer = scene^.sceneNodeRenderer
    (r', bbs) <- renderList (renderer & rendererWindowSize .~ Size w h) nodes 
    print bbs 
    return $ Scene r' nodes 
 

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


emptySceneText :: SceneText
emptySceneText = SceneText { _textFont = ""
                               , _textWidth = 12
                               , _textString = ""
                               , _textColor = Color4 0 0 0 1
                               }


emptyNode :: SceneNode
emptyNode = SceneNode { _nodeName = ""
                      , _nodePosition = Position 0 0
                      , _nodeSize = Size 0 0
                      , _nodeScale = Scale 1 1
                      , _nodeRotation = Rotation 0
                      , _nodeDraws = []
                      , _nodeChildren = 0
                      , _nodeCache = Nothing
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


node :: State SceneNode SceneList -> SceneList
node s = let (list,n) = runState s emptyNode
             n' = n{_nodeChildren = length list}
         in n':list


drawText :: State SceneText () -> State SceneNode ()
drawText s = nodeDraws %= (++ [renderText (execState s emptySceneText)])


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
    rectangle <- drawTextAt' tr (Position 0 0) (t^.textString)
    return $ (r & rendererFontAtlas %~ (insertAtlas font size (tr'^.atlas)), rectangle)


drawShapes :: IO BoundingBox -> State SceneNode ()
drawShapes f = nodeDraws %= (++ [renderShapes f])


renderShapes :: IO BoundingBox -> Draw
renderShapes f r = do
    let Size w h = r^.rendererWindowSize
        pj  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv  = r^.rendererModelview
        s   = r^.rendererShapeShader
    currentProgram $= Just (s^.S.program)
    s^.setIsTextured $ False
    s^.S.setProjection $ concat pj
    s^.S.setModelview $ concat mv
    bb <- f
    return (r, bb)


renderList :: NodeRenderer -> SceneList -> IO (NodeRenderer, [BoundingBox])
renderList r [] = return (r, []) 
renderList r (n:list) = do
    let nodes = take (n^.nodeChildren) list
        rest  = drop (n^.nodeChildren) list
        mv    = r^.rendererModelview
        r'    = r & rendererModelview %~ (`multiply` nodeTransform n)
    -- Draw this node and update our renderer.
    -- This allows us to load fonts and text as we go along.
    (r'', bbs) <- foldM accumRendererAndBoxes (r', []) (n^.nodeDraws)
    -- Fold all the bounding boxes up into one.
    let bb = case bbs of
                 []   -> mempty
                 b:[] -> b
                 b:bs -> foldl mappend b bs  
    -- Draw subnodes with this modelview.
    (r''', nodebbs) <- renderList r'' nodes
    -- Draw the rest of the nodes without this modelview.
    (r'''', restbbs) <- renderList (r''' & rendererModelview .~ mv) rest
    return (r'''', [bb] ++ nodebbs ++ restbbs)

accumRendererAndBoxes :: (NodeRenderer, [BoundingBox]) -> Draw -> IO (NodeRenderer, [BoundingBox]) 
accumRendererAndBoxes (r, bbs) f = do
    (r', bb) <- f r 
    return (r', bbs ++ [bb])
