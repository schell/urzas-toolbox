{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Urza.Input.Event where

import           Prelude hiding ((.), id, until)
import           Urza.Input.Types
import           Linear
import qualified Data.Set as S

emptyInputEnv :: InputEnv
emptyInputEnv = InputEnv [] False (0,0) S.empty S.empty $ V2 0 0

getCharEvent :: [InputEvent] -> Maybe InputEvent
getCharEvent (e@(CharEvent _):_) = Just e
getCharEvent (_:es) = getCharEvent es
getCharEvent [] = Nothing

getWindowSizeEvent :: [InputEvent] -> Maybe InputEvent
getWindowSizeEvent (e@(WindowSizeEvent _ _):_) = Just e
getWindowSizeEvent (_:es) = getWindowSizeEvent es
getWindowSizeEvent [] = Nothing

getKeyEvent :: [InputEvent] -> Maybe InputEvent
getKeyEvent (e@(KeyEvent _ _ _ _):_) = Just e
getKeyEvent (_:es) = getKeyEvent es
getKeyEvent [] = Nothing

getMouseButtonEvent :: [InputEvent] -> Maybe InputEvent
getMouseButtonEvent (e@(MouseButtonEvent _ _ _):_) = Just e
getMouseButtonEvent (_:es) = getMouseButtonEvent es
getMouseButtonEvent [] = Nothing

getCursorMoveEvent :: [InputEvent] -> Maybe InputEvent
getCursorMoveEvent (e@(CursorMoveEvent _ _):_) = Just e
getCursorMoveEvent (_:es) = getCursorMoveEvent es
getCursorMoveEvent [] = Nothing

getCursorEnterEvent :: [InputEvent] -> Maybe InputEvent
getCursorEnterEvent (e@(CursorEnterEvent _):_) = Just e
getCursorEnterEvent (_:es) = getCursorEnterEvent es
getCursorEnterEvent [] = Nothing

getScrollEvent :: [InputEvent] -> Maybe InputEvent
getScrollEvent (e@(ScrollEvent _ _):_) = Just e
getScrollEvent (_:es) = getScrollEvent es
getScrollEvent [] = Nothing

