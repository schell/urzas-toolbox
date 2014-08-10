module Urza.Input.Types (
    module GLFW,
    InputEvent(..),
    InputEnv(..)
) where


import           Graphics.UI.GLFW as GLFW
import           Linear
import qualified Data.Set as S


data InputEvent = NoInputEvent
                | CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys -- Key, scancode, pressed/released, mods
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)

data InputEnv = InputEnv { _ienvEvents           :: [InputEvent]
                         , _ienvCursorOnScreen   :: Bool
                         , _ienvLastCursorPos    :: (Double, Double)
                         , _ienvKeysDown         :: S.Set Key
                         , _ienvMouseButtonsDown :: S.Set MouseButton
                         , _ienvWindowSize       :: V2 Int
                         } deriving (Show)
