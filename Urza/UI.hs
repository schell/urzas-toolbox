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
import           Graphics.Urza.Sketch
import           Graphics.Urza.Sketch.Math
import           Graphics.Urza.Sketch.Utils
import           Graphics.Urza.Sketch.Shader.Text as T
import           Graphics.Urza.Sketch.Shader.Shape as S
import           Graphics.Urza.Sketch.Text
import           Graphics.Urza.Sketch.Types
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get)
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as I


newScene :: Size -> FilePath -> SceneList -> IO Scene
newScene windowSize fontDir gui = do
    tshader <- makeShaderProgram
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


renderSceneToRendererAndBounds :: Size -> Scene -> IO (NodeRenderer, [BoundingBox])
renderSceneToRendererAndBounds (Size w h) scene =
    let nodes    = scene^.sceneList
        renderer = scene^.sceneNodeRenderer
    in renderList (renderer & rendererWindowSize .~ Size w h) nodes


renderScene :: Size -> Scene -> IO Scene
renderScene size scene = do
    r <- fmap fst $ renderSceneToRendererAndBounds size scene
    return $ scene & sceneNodeRenderer .~ r

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
        mv = map (map fromIntegral) (r^.rendererModelview) :: Matrix Double
        ts = r^.rendererTextShader
        mA = lookupAtlas font size (r^.rendererFontAtlas)
    -- Get a text renderer.
    tr <- case mA of
        Just a  -> return $ Renderer ts a
        Nothing -> do atls <- makeAtlas font $ fromIntegral size
                      return $ Renderer ts atls
    -- Load that text up into the renderer.
    tr' <- loadCharMap tr $ t^.textString
    -- Get the geometry for the text.
    let BufferAcc _ _ _ xRange yRange = geometryForString (emptyBufferAccumulator $ tr'^.atlas) $ t^.textString
        bbw   = xRange^._2 - xRange^._1
        bbh   = yRange^._2 - yRange^._1
        tsize = Size (fromIntegral bbw) (fromIntegral bbh)

    let renderer = r & rendererFontAtlas %~ (insertAtlas font size $ tr'^.atlas)
        localbb  = Rectangle 0 0 (fromIntegral bbw) (fromIntegral bbh) :: BoundingBox
        globalbb = bbLocalToGlobal localbb mv

    -- Draw this node into a texture.
    tex <- renderToTexture tsize RGBA' $ do
        currentProgram $= Just (ts^.T.program)
        ts^.T.setProjection $ concat pj
        ts^.T.setModelview $ concat $ identityN 4
        drawTextAt tr (Position 0 0) (t^.textString)

    return $ RenderResult { _resultNodeRenderer = renderer
                          , _resultTexture = tex
                          , _resultLocalBB = localbb
                          , _resultGlobalBB = globalbb
                          }


bbLocalToGlobal :: BoundingBox -> Matrix Double -> BoundingBox
bbLocalToGlobal localbb mv = 
    vec2sToRectangle $ map (`multiplyVec2` mv) $ rectangleToVec2s localbb


rectangleToVec2s :: Num a => Rectangle a -> [Vec2 a]
rectangleToVec2s (Rectangle x y w h) = [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]


vec2sToRectangle :: (Num a, Ord a, Fractional a) => [Vec2 a] -> Rectangle a
vec2sToRectangle vs =
    let (lx,rx,ty,by) = foldl acc (1/0, -1/0, 1/0, -1/0) vs
        acc (l,r,t,b) (x,y) = (min x l, max x r, min y t, max y b)
    in Rectangle lx ty (rx - lx) (by - ty)


drawShapes :: BoundingBox -> IO () -> State SceneNode ()
drawShapes bb f = nodeDraws %= (++ [renderShapes bb f])


renderShapes :: BoundingBox -> IO () -> Draw
renderShapes bb@(Rectangle x y w h) f r = do
    let Size w h = r^.rendererWindowSize
        pj  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv  = map (map fromIntegral) $ r^.rendererModelview
        s   = r^.rendererShapeShader
        tsize = Size (fromIntegral w) (fromIntegral h)
    tex <- renderToTexture tsize RGBA' $ do
        currentProgram $= Just (s^.S.program)
        s^.setIsTextured $ False
        s^.S.setProjection $ concat pj
        s^.S.setModelview $ concat $ identityN 4
        f
    return $ RenderResult { _resultNodeRenderer = r
                          , _resultTexture = tex
                          , _resultLocalBB = bb
                          , _resultGlobalBB = bbLocalToGlobal bb mv
                          }

-- | Uses a `NodeRenderer` to render a `SceneList` and returns a new
-- updated `NodeRenderer` and the bounding boxes of each node of the
-- `SceneList`.
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


-- | Similar to renderList but renders into a list of textures and returns
-- the new `NodeRenderer` and the list of `SceneNodeCache`s.
renderAndCacheList :: NodeRenderer -> SceneList -> [BoundingBox] -> IO (NodeRenderer, [SceneNodeCache])
renderAndCacheList r [] _ = return (r, [])
renderAndCacheList _ _ _ = undefined

accumRendererAndBoxes :: (NodeRenderer, [RenderResult]) -> Draw -> IO (NodeRenderer, [RenderResult])
accumRendererAndBoxes (r, results) f = do
    result <- f r
    return (result, results ++ [result])

