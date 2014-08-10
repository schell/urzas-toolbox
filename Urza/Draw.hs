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
    rectangle,
    setColor,
    setPoint,
    stroke,
    strokePath,
    strokePath_
) where

import Urza.Types as T
import Linear hiding (lerp)
import Urza.Shader
import Graphics.Rendering.OpenGL
import Control.Lens
import Control.Monad
import Control.Monad.State


-- | Creates a new default path.
newPath :: (Fractional a) => Path a
newPath = Path [] [] 0 (Color4 0 0 0 1) (V2 0 0) (1/0,-1/0) (1/0,-1/0)


-- | Runs a path state change over a new path and returns the modified
-- path.
execNewPath :: Fractional a => PathState a -> Path a
execNewPath = flip execState newPath


-- | Sets the current color of the path.
setColor :: PathColor -> PathState a
setColor = (pathColor .=)


-- | Sets the next position of the path.
-- This does not add the point.
setPoint :: V2 a -> PathState a
setPoint = (pathPoint .=)


-- | Pushes the current point onto the end of the path along with the
-- current color.
pushPoint :: Ord a => PathState a
pushPoint = do
    V2 x y  <- use pathPoint
    (xMin, xMax) <- use pathXBounds
    (yMin, yMax) <- use pathYBounds
    -- Push the point up.
    pathPoints %= (++ [(V2 x y)])
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
moveTo :: V2 a -> PathState a
moveTo = setPoint


-- | Adds to the path a line from the current point to the given point and
-- updates the current point to the latter.
lineTo :: Ord a => V2 a -> PathState a
lineTo p = do
    pushPoint
    moveTo p
    pushPoint


-- | Adds to the path a bezier curve sampled n times.
curveAlong :: (Fractional a, Num a, Ord a) => [V2 a] -> Int -> PathState a
curveAlong curve n =
    case curve of
        []   -> return ()
        _:[] -> return ()
        -- ps' is the control polygon of the curve.
        ps'@(p:_) -> do
            moveTo p
            let ts  = intervals 0 1 n
            forM_ ts $ \t ->
                -- Do the bezier interpalation.
                lineTo $ deCasteljau t ps'


curveTo :: (Fractional a, Num a, Ord a) => V2 a -> [V2 a] -> Int -> PathState a
curveTo p2 ctrls n = do
    p1 <- use pathPoint
    curveAlong (p1:ctrls ++ [p2]) n


-- | Adds to the path a line from the current point to the given point and
-- changes the color at the given point.
gradientLineTo :: (Num a, Ord a) => V2 a -> PathColor -> PathState a
gradientLineTo p c = do
    pushPoint
    moveTo p
    setColor c
    pushPoint


-- | Adds a rectangle at the given point with a given width and height.
rectangle :: (Num a, Ord a) => Rectangle a -> PathState a
rectangle (Rectangle x y w h) = do
    moveTo $ V2 x y
    lineTo $ V2 (x+w) y
    lineTo $ V2 (x+w) (y+h)
    lineTo $ V2 x (y+h)
    lineTo $ V2 x y


circleOfNAt :: (Real a, Floating a, RealFrac a, Fractional a) => V2 a -> a -> Int -> PathState a
circleOfNAt (V2 x y) r n = do
    when (r > 0) $ do
        let i      = n
            fi     = fromIntegral i
        -- Divide 2pi by i to get the radian increment.
        let rinc   = 2 * pi / fi
            thetas = take i $ iterate (+ rinc) 0
            fxys   = map (\t -> (cos t * r + x, sin t * r + y)) thetas
            (xx,yy):xys = fmap (over both realToFrac) fxys

        -- Move to the first point on the circle
        moveTo $ V2 xx yy
        unless (null xys) $ do
            forM_ xys $ \(x',y') -> do
                lineTo $ V2 x' y'
            lineTo $ V2 xx yy


circleAt :: (Real a, Floating a, RealFrac a, Fractional a) => V2 a -> a -> PathState a
circleAt p r =
    -- Find how many segments we will need to keep the straight
    -- line segment unnoticable.
    let arcLen = 10 -- Segments of 10 pixels.
        c      = 2 * pi * r
        -- Use at least 8 points.
        i      = max (ceiling $ c / arcLen) 8
    in circleOfNAt p r i


-- | Closes a path.
closePath :: (Eq a, Ord a) => PathState a
closePath = do
    ps <- use pathPoints
    unless (null ps) $ do
        let lp = last ps
        when (head ps /= lp) $ lineTo lp


-- | Draws a path into the current opengl context and returns the bounding
-- box of the result rendering.
-- Assumes a shader is set up with the proper uniforms to handle the
-- drawing.
stroke :: (Num a, Real a) => ShaderProgram -> Path a -> IO (Rectangle a)
stroke s p = drawPath s p Lines


strokePath :: (Fractional a, Num a, Real a) => ShaderProgram -> PathState a -> IO (Rectangle a)
strokePath s = stroke s . execNewPath


strokePath_ :: (Fractional a, Num a, Real a) => ShaderProgram -> PathState a -> IO ()
strokePath_ r s = strokePath r s >> return ()


-- | Draws a filled path as a polygon into the current opengl context.
-- We should probably be doing earclipping and sending tris to the gpu but
-- drawing in Polygon primitive mode works for now.
fill :: (Num a, Real a) => ShaderProgram -> Path a -> IO (Rectangle a)
fill s p = drawPath s p' Polygon
    where p' = execState closePath p


drawPath :: (Num a, Real a) => ShaderProgram -> Path a -> PrimitiveMode -> IO (Rectangle a)
drawPath s p mode = do
    let vs  = map realToFrac $ concat [ [x,y] | V2 x y <- p^.pathPoints ]
        uvs = map realToFrac $ concat [ [r,g,b,a] | Color4 r g b a <- p^.pathColors ]
        (l,rt) = p^.pathXBounds
        (t,bm) = p^.pathYBounds
    ((i,_,_),(j,_,_)) <- bindAndBufferVertsColors vs uvs
    texture Texture2D $= Disabled
    s^.setIs3d $ False
    s^.setIsTextured $ False
    s^.setColorIsReplaced $ False
    drawArrays mode 0 $ p^.pathLength
    deleteObjectNames [i,j]
    return $ Rectangle l t (rt - l) (bm - t)



fillPath :: (Fractional a, Num a, Real a) => ShaderProgram -> PathState a -> IO (Rectangle a)
fillPath s = fill s . execNewPath


fillPath_ :: (Fractional a, Num a, Real a) => ShaderProgram -> PathState a -> IO ()
fillPath_ shdr s = fillPath shdr s >> return ()


deCasteljau :: (Fractional a, Num a) => a -> [V2 a] -> V2 a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (lerpP t) coefs (tail coefs)
    lerpP t' (V2 x0 y0) (V2 x1 y1) = V2 (lerp t' x0 x1) (lerp t' y0 y1)
    lerp t' a b = t' * b + (1 - t') * a


intervals :: (Fractional a, Num a) => a -> a -> Int -> [a]
intervals n n' i = ns ++ [n']
    where ns  = take (i-1) $ iterate (+ inc) n
          inc = (n' - n) / fromIntegral (i-1)


