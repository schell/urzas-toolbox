module Urza.Wire.Core where

import           Prelude hiding ((.), id, until)
import           Urza.Types
import           Graphics.UI.GLFW
import           FRP.Netwire
import           Data.Maybe
import           Control.Wire
import           Control.Wire.Unsafe.Event
import           Control.Monad.Reader hiding (when)
import           Control.Monad as M
import           Control.Concurrent
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)

-- | A simple pass through wire.
pass :: Monad m => Wire s e m a a
pass = arr id

-- | Iterates and renders an Iteration. Processes the InputEvent into the
-- iteration.
stepAndRender :: Show ev => Iteration en ev ex a -> Maybe ev -> IO (Iteration en ev ex a)
stepAndRender i mEvent = do
    --M.when (isJust mEvent) $ print mEvent
    let i' = i & iEnv %~ (i^.iProcessEv) mEvent
    (dt, session) <- stepSession $ i'^.iSession
    let i'' = stepIteration i' dt & iSession .~ session
    idata' <- i''^.iRender $ i''^.iData
    return $ i'' & iData .~ idata'



-- | Steps an Iteration using a timer.
stepIteration :: Iteration en ev ex a -> TimeDelta -> Iteration en ev ex a
stepIteration i t =
    let (idata, wire) = runReader (stepWire (i^.iWire) t (i^.iData)) (i^.iEnv)
    in i & iData .~ idata & iWire .~ wire
