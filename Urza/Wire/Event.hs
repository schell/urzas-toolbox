{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Urza.Wire.Event where

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

useNow :: (Monad m, Monoid e) => Wire s e m (Event a) a
useNow = mkGen_ $ return . switcheroo
    where switcheroo NoEvent = Left mempty
          switcheroo (Event a) = Right a


useNow_ :: (Monad m, Monoid e) => Wire s e m (Event a) ()
useNow_ = arr (const ()) . useNow


keyIsDown :: MonadReader InputEnv m => Key -> Wire s e m a (Event a)
keyIsDown key = mkGen_ $ \a -> do
    isDown <- asks $ S.member key . _ienvKeysDown
    return $ Right $ if isDown then Event a else NoEvent


keyDown :: MonadReader InputEnv m => Key -> Wire s e m a (Event a)
keyDown key = keyEvent key KeyState'Pressed


keyUp :: MonadReader InputEnv m => Key -> Wire s e m a (Event a)
keyUp key = keyEvent key KeyState'Released


keyRepeat :: MonadReader InputEnv m => Key -> Wire s e m a (Event a)
keyRepeat key = keyEvent key KeyState'Repeating


keyEvent :: MonadReader InputEnv m => Key -> KeyState -> Wire s e m a (Event a)
keyEvent key kstate = mkGen_ $ \a -> do
    mEv <- asks _ienvEvent
    return $ Right $ case mEv of
        Just (KeyEvent k _ ks _) -> if k == key && ks == kstate then Event a else NoEvent
        _ -> NoEvent

onCharEvent :: (MonadReader InputEnv m, Monoid e) => Char -> Wire s e m a (Event ())
onCharEvent ch = proc _ -> do
   ch' <- useNow . charEvent -< ()
   returnA -< if ch' == ch then Event () else NoEvent


charEvent :: MonadReader InputEnv m => Wire s e m a (Event Char)
charEvent = mkGen_ $ \_ -> do
    mEv <- asks _ienvEvent
    return $ Right $ case mEv of
        Just (CharEvent ch) -> Event ch
        _ -> NoEvent


cursorEnterEvent :: MonadReader InputEnv m => CursorState -> Wire s e m a (Event a)
cursorEnterEvent cState = mkGen_ $ \a -> do
    mEv <- asks _ienvEvent
    return $ Right $ case mEv of
        Just (CursorEnterEvent s) -> if cState == s then Event a else NoEvent
        _ -> NoEvent


-- | Produces `a` when the cursor is on the screen, otherwise it inhibits.
whenCursorIsOnScreen :: (Monoid e) => MonadReader InputEnv m => Wire s e m a a
whenCursorIsOnScreen = mkGen_ $ \a -> do
    cOs <- asks _ienvCursorOnScreen
    return $ if cOs then Right a else Left mempty


cursorPositionStartingWith :: (MonadReader InputEnv m, HasTime t s, Monoid e, Fractional t) => Position -> Wire s e m a Position
cursorPositionStartingWith pos = cursor2Pos . asSoonAs . cursorMoveEvent <|> (pure pos)
    where cursor2Pos = arr $ \(x, y) -> Position (round x) (round y)


cursorMoveEvent :: MonadReader InputEnv m => Wire s e m a (Event (Double, Double))
cursorMoveEvent = mkGen_ $ \_ -> do
    mEv <- asks _ienvEvent
    return $ Right $ case mEv of
        Just (CursorMoveEvent x y) -> Event (x, y)
        _ -> NoEvent


mouseDownLeft :: InputWire () (Event (Double, Double))
mouseDownLeft = mouseButtonEvent MouseButton'1 MouseButtonState'Pressed


mouseUpLeft :: InputWire () (Event (Double, Double))
mouseUpLeft = mouseButtonEvent MouseButton'1 MouseButtonState'Released


mouseIsDown :: MonadReader InputEnv m => MouseButton -> Wire s e m a (Event a)
mouseIsDown button = mkGen_ $ \a -> do
    isDown <- asks $ S.member button . _ienvMouseButtonsDown
    return $ Right $ if isDown then Event a else NoEvent


mouseIsDraggingWith :: MonadReader InputEnv m => MouseButton -> Wire s e m a (Event (Double, Double))
mouseIsDraggingWith mbutton = mkGen_ $ \_ -> do
   cdown <- asks $ S.member mbutton . _ienvMouseButtonsDown
   pos <- asks _ienvLastCursorPos
   return $ Right $ if cdown then Event pos else NoEvent


mouseButtonEvent :: MonadReader InputEnv m => MouseButton -> MouseButtonState -> Wire s e m a (Event (Double, Double))
mouseButtonEvent mbutton mstate = mkGen_ $ \_ -> do
    mEv   <- asks _ienvEvent
    (x,y) <- asks _ienvLastCursorPos
    return $ Right $ case mEv of
        Just (MouseButtonEvent mbutton' mstate' _) ->
            if mbutton == mbutton' && mstate == mstate'
              then Event (x,y)
              else NoEvent
        _ -> NoEvent


scrollEvent :: MonadReader InputEnv m => Wire s e m a (Event (Double, Double))
scrollEvent = mkGen_ $ \_ -> do
    mEv <- asks _ienvEvent
    return $ Right $ case mEv of
        Just (ScrollEvent x y) -> Event (x,y)
        _ -> NoEvent


windowResizeEvent :: MonadReader InputEnv m => Wire s e m a (Event (Int, Int))
windowResizeEvent = mkGen_ $ \_ -> do
    mEv <- asks _ienvEvent
    return $ Right $ case mEv of
        Just (WindowSizeEvent x y) -> Event (x,y)
        _ -> NoEvent
