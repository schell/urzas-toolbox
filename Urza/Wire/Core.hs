module Urza.Wire.Core where

import           Prelude hiding ((.), id, until)
import           Urza.Types
import           Graphics.UI.GLFW
import           FRP.Netwire
import           Control.Wire
import           Control.Wire.Unsafe.Event
import           Control.Monad.Reader hiding (when)
import           Control.Concurrent
import qualified Data.Set as S
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)


-- | Iterates and renders an Iteration2d. Processes the InputEvent into the
-- iteration.
stepAndRender :: MVar (Iteration2d e a) -> Maybe InputEvent -> IO ()
stepAndRender ivar mEvent = stepWithIterationVar ivar mEvent >> renderWithIterationVar ivar


-- | Renders an iteration stored in an MVar. Updates the MVar with the
-- newest value after the rendering (rendering returns an Either e a).
renderWithIterationVar :: MVar (Iteration2d e a) -> IO ()
renderWithIterationVar ivar = do
    iter <- takeMVar ivar
    idata' <- iter^.iRender $ iter^.iData
    putMVar ivar $ iter & iData .~ idata'


-- | Steps an Iteration2d stored in an MVar. Processes the given event into
-- the iteration.
stepWithIterationVar :: MVar (Iteration2d e a) -> Maybe InputEvent -> IO (Iteration2d e a)
stepWithIterationVar ivar mEvent = do
    iter <- (& iEnv %~ processEnv mEvent) <$> takeMVar ivar
    (_, session) <- stepSession $ _iSession iter
    return $ stepIteration iter session


-- | Processes individual events into an input environment.
processEnv :: Maybe InputEvent -> Env -> Env
processEnv mE@(Just (CursorMoveEvent x y)) env =
    env & envLastCursorPos .~ (x,y) & envEvent .~ mE
processEnv mE@(Just (CursorEnterEvent cs)) env =
    env & envCursorOnScreen .~ (cs == CursorState'InWindow) & envEvent .~ mE
processEnv mE@(Just (MouseButtonEvent mb MouseButtonState'Pressed _)) env =
    env & (envMouseButtonsDown %~ S.insert mb) & envEvent .~ mE
processEnv mE@(Just (MouseButtonEvent mb MouseButtonState'Released _)) env =
    env & (envMouseButtonsDown %~ S.delete mb) & envEvent .~ mE
processEnv mE env = env & envEvent .~ mE


-- | Steps an Iteration2d using a timer.
stepIteration :: Iteration2d e a -> Timer -> Iteration2d e a
stepIteration i t =
    let idata = _iData i
        wire  = _iWire i
        env   = _iEnv i
        (idata', wire') = runReader (stepWire wire t idata) env
    in i & iData .~ idata' & iWire .~ wire' & iSession .~ t
