{-# LANGUAGE TemplateHaskell #-}
module Urza.Draw (
    module T,
    circleAt,
    circleOfNAt,
    closePath,
    curveAlong,
    curveTo,
    execNewPath,
    fill,
    fillPath,
    fillPath_,
    gradientLineTo,
    intervals,
    lineTo,
    moveTo,
    newPath,
    pushPoint,
    rectangleAt,
    setColor,
    setPoint,
    stroke,
    strokePath,
    strokePath_,
    pointToTuple
) where

import Urza.Types as T
import Urza.Shader
import Graphics.Rendering.OpenGL
import Control.Lens
import Control.Monad
import Control.Monad.State


pointToTuple :: Point2d -> (Double, Double)
pointToTuple (Point2d x y) = (x, y)


-- | Creates a new default path.
newPath :: Path
newPath = Path [] [] 0 (Color4 0 0 0 1) (Point2d 0 0) (1/0,-1/0) (1/0,-1/0)


-- | Runs a path state change over a new path and returns the modified
-- path.
execNewPath :: State Path () -> Path
execNewPath = flip execState newPath


-- | Sets the current color of the path.
setColor :: PathColor -> State Path ()
setColor = (pathColor .=)


-- | Sets the next position of the path.
-- This does not add the point.
setPoint :: Point2d -> State Path ()
setPoint = (pathPoint .=)


-- | Pushes the current point onto the end of the path along with the
-- current color.
pushPoint :: State Path ()
pushPoint = do
    Point2d x y  <- use pathPoint
    (xMin, xMax) <- use pathXBounds
    (yMin, yMax) <- use pathYBounds
    -- Push the point up.
    pathPoints %= (++ [(Point2d x y)])
    -- Update width/height.
    when (x < xMin) $ pathXBounds._1 .= x
    when (x > xMax) $ pathXBounds._2 .= x
    when (y < yMin) $ pathYBounds._1 .= y
    when (y > yMax) $ pathYBounds._2 .= y
    -- Push a color up.
    c <- use pathColor
    pathColors %= (++ [c])
    -- Update length.
    pathLength %= (+1)


-- | Moves the path to a point without altering the path.
moveTo :: Double -> Double -> State Path ()
moveTo x y = setPoint $ Point2d x y


-- | Adds to the path a line from the current point to the given point and
-- updates the current point to the latter.
lineTo :: Double -> Double -> State Path ()
lineTo x y = do
    pushPoint
    moveTo x y
    pushPoint


-- | Adds to the path a bezier curve sampled n times.
curveAlong :: [(Double, Double)] -> Int -> State Path ()
curveAlong curve n =
    case curve of
        []   -> return ()
        _:[] -> return ()
        -- ps' is the control polygon of the curve.
        ps'@((x, y):_) -> do
            moveTo x y
            let ts  = intervals 0 1 n :: [Double]
            forM_ ts $ \t -> do
                -- Do the bezier interpalation.
                let (x', y') = deCasteljau t ps'
                lineTo x' y'


curveTo :: Double -> Double -> [(Double, Double)] -> Int -> State Path ()
curveTo x2 y2 ctrls n = do
    Point2d x1 y1 <- use pathPoint
    curveAlong ((x1,y1):ctrls ++ [(x2,y2)]) n


-- | Adds to the path a line from the current point to the given point and
-- changes the color at the given point.
gradientLineTo :: Double -> Double -> PathColor -> State Path ()
gradientLineTo x y c = do
    pushPoint
    moveTo x y
    setColor c
    pushPoint


-- | Adds a rectangle at the given point with a given width and height.
rectangleAt :: Double -> Double -> Double -> Double -> State Path ()
rectangleAt x y w h = do
    moveTo x y
    lineTo (x+w) y
    lineTo (x+w) (y+h)
    lineTo x (y+h)
    lineTo x y


circleOfNAt :: Double -> Double -> Double -> Int -> State Path ()
circleOfNAt x y r n = do
    when (r > 0) $ do
        let i      = n
            fi     = fromIntegral i
        -- Divide 2pi by i to get the radian increment.
        let rinc   = 2 * pi / fi
            thetas = take i $ iterate (+ rinc) 0
            fxys   = map (\t -> (cos t * r + x, sin t * r + y)) thetas
            (xx,yy):xys = fmap (over both realToFrac) fxys

        -- Move to the first point on the circle
        moveTo xx yy
        unless (null xys) $ do
            forM_ xys $ \(x',y') -> do
                lineTo x' y'
            lineTo xx yy


circleAt :: Double -> Double -> Double -> State Path ()
circleAt x y r =
    -- Find how many segments we will need to keep the straight
    -- line segment unnoticable.
    let arcLen = 10 -- Segments of 10 pixels.
        c      = 2 * pi * r
        -- Use at least 8 points.
        i      = max (ceiling $ c / arcLen) 8
    in circleOfNAt x y r i


-- | Closes a path.
closePath :: State Path ()
closePath = do
    ps <- use pathPoints
    unless (null ps) $ do
        let lp@(Point2d x y) = last ps
        when (head ps /= lp) $ lineTo x y


-- | Draws a path into the current opengl context and returns the bounding
-- box of the result rendering.
-- Assumes a shader is set up with the proper uniforms to handle the
-- drawing.
stroke :: Path -> IO (Rectangle Double)
stroke p = do
    let vs  = map realToFrac $ concat [ [x,y] | Point2d x y <- p^.pathPoints ]
        uvs = map realToFrac $ concat [ [red,g,bl,a] | Color4 red g bl a <- p^.pathColors ]
        (l,r) = p^.pathXBounds
        (t,b) = p^.pathYBounds
    (i,j) <- bindAndBufferVertsColors vs uvs
    drawArrays Lines 0 $ p^.pathLength
    deleteObjectNames [i,j]
    return $ Rectangle l t (r - l) (b - t)


strokePath :: State Path () -> IO (Rectangle Double)
strokePath = stroke . execNewPath


strokePath_ :: State Path () -> IO ()
strokePath_ s = strokePath s >> return ()


-- | Draws a filled path as a polygon into the current opengl context.
-- We should probably be doing earclipping and sending tris to the gpu but
-- drawing in Polygon primitive mode works for now.
fill :: Path -> IO (Rectangle Double)
fill p = do
    -- Make sure
    let p'  = execState closePath p
        vs  = map realToFrac $ concat [ [x,y] | Point2d x y <- p'^.pathPoints ]
        uvs = map realToFrac $ concat [ [r,g,b,a] | Color4 r g b a <- p'^.pathColors ]
        (l,rt) = p^.pathXBounds
        (t,bm) = p^.pathYBounds
    (i,j) <- bindAndBufferVertsColors vs uvs
    drawArrays Polygon 0 $ p'^.pathLength
    deleteObjectNames [i,j]
    return $ Rectangle l t (rt - l) (bm - t)


fillPath :: State Path () -> IO (Rectangle Double)
fillPath = fill . execNewPath


fillPath_ :: State Path () -> IO ()
fillPath_ s = fillPath s >> return ()


deCasteljau :: Double -> [(Double, Double)] -> (Double, Double)
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (lerpP t) coefs (tail coefs)
    lerpP t' (x0, y0) (x1, y1) = (lerp t' x0 x1, lerp t' y0 y1)
    lerp t' a b = t' * b + (1 - t') * a


intervals :: (Fractional a, Num a) => a -> a -> Int -> [a]
intervals n n' i = ns ++ [n']
    where ns  = take (i-1) $ iterate (+ inc) n
          inc = (n' - n) / fromIntegral (i-1)


