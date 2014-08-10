{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Urza.Wire.Event where

import           Prelude hiding ((.), id, until)
import           Urza.Types
import           Urza.Wire.Types
import           Urza.Wire.Core
import           Urza.Input.Event
import           Urza.Input.Types
import           Graphics.UI.GLFW
import           Linear
import           Control.Wire
import           Control.Wire.Core
import           Control.Wire.Session
import           Control.Monad.Reader hiding (when)
import           Control.Concurrent
import qualified Data.Set as S
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)


keyIsDown :: MonadReader InputEnv m => Key -> Wire m a (Event a)
keyIsDown key = mkGen $ \_ a -> do
    isDown <- asks $ S.member key . _ienvKeysDown
    let ev = if isDown then Event a else NoEvent
    return $ Output ev (keyIsDown key)


keyDown :: MonadReader InputEnv m => Key -> Wire m a (Event a)
keyDown key = keyEvent key KeyState'Pressed


keyUp :: MonadReader InputEnv m => Key -> Wire m a (Event a)
keyUp key = keyEvent key KeyState'Released


keyRepeat :: MonadReader InputEnv m => Key -> Wire m a (Event a)
keyRepeat key = keyEvent key KeyState'Repeating


keyEvent :: MonadReader InputEnv m => Key -> KeyState -> Wire m a (Event a)
keyEvent key kstate = mkGen $ \_ a -> do
    mEv <- asks (getKeyEvent . _ienvEvents)
    let ev = case mEv of
                 Just (KeyEvent k _ ks _) -> if k == key && ks == kstate then Event a else NoEvent
                 _ -> NoEvent
    return $ Output ev (keyEvent key kstate)


onCharEvent :: (MonadReader InputEnv m) => Char -> Wire m a (Event a)
onCharEvent ch = proc a -> do
    mCh <- toMaybe . charEvent -< ()
    let ev = case mCh of
                 Nothing  -> NoEvent
                 Just ch' -> if ch == ch' then Event a else NoEvent
    returnA -< ev


charEvent :: MonadReader InputEnv m => Wire m a (Event Char)
charEvent = askInputEvent getCharEvent $ \(CharEvent ch) -> ch
--charEvent = mkGen_ $ \_ -> do
--    mEv <- asks (getCharEvent . _ienvEvents)
--    return $ Right $ case mEv of
--        Just (CharEvent ch) -> Event ch
--        _ -> NoEvent


onCursorEvent :: MonadReader InputEnv m => CursorState -> Wire m a (Event a)
onCursorEvent cState = mkGen $ \_ a -> do
    mEv <- asks (getCursorEnterEvent . _ienvEvents)
    let ev = case mEv of
                 Just (CursorEnterEvent s) -> if cState == s
                                              then Event a
                                              else NoEvent
                 _ -> NoEvent
    return $ Output ev $ onCursorEvent cState


-- | Produces `a` when the cursor is on the screen, otherwise it inhibits.
whenCursorIsOnScreen :: MonadReader InputEnv m => Wire m a (Event a)
whenCursorIsOnScreen = mkGen $ \dt a -> do
    cOs <- asks _ienvCursorOnScreen
    let ev = if cOs then Event a else NoEvent
    return $ Output ev whenCursorIsOnScreen

-- | Produces True when the cursor is on the screen and False when it is
-- not.
isCursorOnScreen :: MonadReader InputEnv m => Wire m a Bool
isCursorOnScreen = effect_ (asks _ienvCursorOnScreen)


cursorPositionStartingWith :: MonadReader InputEnv m => V2 Double -> Wire m a (V2 Double)
cursorPositionStartingWith pos = holdJustInit pos . toMaybe . cursorMoveEvent


cursorMoveEvent :: MonadReader InputEnv m => Wire m a (Event (V2 Double))
cursorMoveEvent =
    askInputEvent getCursorMoveEvent $ \(CursorMoveEvent x y) -> V2 x y


mouseDownLeft :: MonadReader InputEnv m => Wire m a (Event (V2 Double))
mouseDownLeft = mouseButtonEvent MouseButton'1 MouseButtonState'Pressed


mouseUpLeft :: MonadReader InputEnv m => Wire m a (Event (V2 Double))
mouseUpLeft = mouseButtonEvent MouseButton'1 MouseButtonState'Released


mouseIsDown :: MonadReader InputEnv m => MouseButton -> Wire m a (Event a)
mouseIsDown button = mkGen $ \_ a -> do
    isDown <- asks $ S.member button . _ienvMouseButtonsDown
    let ev = if isDown then Event a else NoEvent
    return $ Output ev (mouseIsDown button)


mouseIsDraggingWith :: MonadReader InputEnv m => MouseButton -> Wire m a (Event (V2 Double))
mouseIsDraggingWith mbutton = mkGen $ \_ _ -> do
   cdown <- asks $ S.member mbutton . _ienvMouseButtonsDown
   (x,y) <- asks _ienvLastCursorPos
   let ev = if cdown then Event (V2 x y) else NoEvent
   return $ Output ev $ mouseIsDraggingWith mbutton


mouseButtonEvent :: MonadReader InputEnv m => MouseButton -> MouseButtonState -> Wire m a (Event (V2 Double))
mouseButtonEvent mbutton mstate = mkGen $ \_ _ -> do
    mEv   <- asks (getMouseButtonEvent . _ienvEvents)
    (x,y) <- asks _ienvLastCursorPos
    let ev = case mEv of
                 Just (MouseButtonEvent mbutton' mstate' _) ->
                     if mbutton == mbutton' && mstate == mstate'
                     then Event (V2 x y)
                     else NoEvent
                 _ -> NoEvent
    return $ Output ev (mouseButtonEvent mbutton mstate)


scrollEvent :: MonadReader InputEnv m => Wire m a (Event (V2 Double))
scrollEvent =
    askInputEvent getScrollEvent $ \(ScrollEvent x y) -> V2 x y
--scrollEvent = mkGen $ \_ _ -> do
--    mEv <- asks (getScrollEvent . _ienvEvents)
--    return $ Right $ case mEv of
--        Just (ScrollEvent x y) -> Event (x,y)
--        _ -> NoEvent
--    return $ Output ev scrollEvent


windowSize :: MonadReader InputEnv m => V2 Int -> Wire m a (V2 Int)
windowSize s = holdJustInit s . toMaybe . windowResizeEvent


windowResizeEvent :: MonadReader InputEnv m => Wire m a (Event (V2 Int))
windowResizeEvent =
    askInputEvent getWindowSizeEvent $ \(WindowSizeEvent x y) -> V2 x y


askInputEvent :: MonadReader InputEnv m
              => ([InputEvent] -> Maybe InputEvent)
              -> (InputEvent -> b)
              -> Wire m a (Event b)
askInputEvent f g = mkGen $ \_ _ -> do
    mEv <- asks (f . _ienvEvents)
    let ev = case mEv of
                 Just e -> Event (g e)
                 _      -> NoEvent
    return $ Output ev $ askInputEvent f g

