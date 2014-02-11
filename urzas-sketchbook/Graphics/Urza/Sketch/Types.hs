{-# LANGUAGE TemplateHaskell #-}
module Graphics.Urza.Sketch.Types where

import Graphics.Rendering.OpenGL
import Control.Lens


data Rectangle a = Rectangle a a a a


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



