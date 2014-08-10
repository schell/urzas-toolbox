{-# LANGUAGE FlexibleContexts #-}
module Urza.Wire.Core where

import           Prelude hiding ((.), id, until)
import           Urza.Types
import           Urza.Wire.Types
import           Linear hiding (trace)
import           Graphics.UI.GLFW
import           Data.Maybe
--import           Control.Wire
--import           Control.Wire.Core
import           Control.Monad.Reader hiding (when, unless)
import           Control.Monad as M hiding (unless)
import           Control.Concurrent
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)
import           Debug.Trace


-- | A simple pass through wire.
pass :: Monad m => Wire m a a
pass = mkId

traceWire :: (Monad m, Show a) => Wire m a a
traceWire = arr (\a -> trace (show a) a)

traceString :: (Monad m) => String -> Wire m a a
traceString s = arr (\a -> trace s a)

traceWith :: (Monad m) => (a -> String) -> Wire m a a
traceWith f = arr (\a -> trace (f a) a)

holding :: Wire m (Event a) (Maybe a)
holding = hold

holdJustInit :: a -> Wire m (Maybe a) a
holdJustInit a = mkPure $ \_ -> holdJust a
    where holdJust a Nothing   = Output a (holdJustInit a)
          holdJust _ (Just a') = Output a' (holdJustInit a')

toMaybe :: Wire m (Event a) (Maybe a)
toMaybe = mkPureA $ \_ -> toMaybe'
    where toMaybe' (Event a) = Just a
          toMaybe' NoEvent   = Nothing

-- | Produces values from `w1` unless an event occurs on `w2`, in which case it
-- produces the event's value.
unless :: Monad m => Wire m a b -> Wire m a (Event b) -> Wire m a b
unless w1 w2 = mkGen $ \dt a -> do
    Output ev w2' <- stepWire w2 dt a
    case ev of
        Event b -> return $ Output b (w1 `unless` w2')
        NoEvent -> do Output b w1' <- stepWire w1 dt a
                      return $ Output b (w1' `unless` w2')

