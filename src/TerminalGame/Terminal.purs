module TerminalGame.Terminal where

import Prelude
import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Foldable (intercalate)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Process (exit, stdin, stdinIsTTY)
import Node.Stream (Readable)
import TerminalGame.Types (Canvas, KeyState, Size, Vector2, Window(..), projectOnWindow, validateKey)

foreign import getCols :: Effect Int

foreign import getRows :: Effect Int

foreign import getEol :: Effect Char

foreign import write :: String -> Effect Unit

foreign import cursorTo :: Vector2 -> Effect Unit

foreign import moveCursor :: Vector2 -> Effect Unit

resetCursor :: Effect Unit
resetCursor = cursorTo { x: 0, y: 0 }

foreign import clearScreenDown :: Effect Unit -> Effect Unit

hideCursor :: Effect Unit
hideCursor = write (escapeCodeToString HideCursor)

showCursor :: Effect Unit
showCursor = write (escapeCodeToString ShowCursor)

foreign import emitKeypressEvents :: forall r. Readable r -> Effect Unit

foreign import setRawMode :: forall r. Boolean -> Readable r -> Effect Unit

initTerminal :: Ref (List KeyState) -> Effect Unit
initTerminal pressedKeys = do
  emitKeypressEvents stdin
  when stdinIsTTY $ setRawMode true stdin
  flip onKeypress stdin \{ sequence, name, ctrl, meta, shift } -> case validateKey { sequence, name } of
    Just key -> Ref.modify_ (Cons { key, ctrl, meta, shift }) pressedKeys
    Nothing -> pure unit
  hideCursor
  resetCursor
  clearScreenDown $ pure unit
  resetCursor

resetTerminal :: Effect Unit
resetTerminal = do
  showCursor
  exit 0

foreign import _onKeypress :: forall r.
  (forall a. a -> Maybe a) -> (forall a. Maybe a) ->
  ({ sequence :: Maybe String, name :: Maybe String, ctrl :: Boolean, meta :: Boolean, shift :: Boolean } -> Effect Unit) -> Readable r -> Effect Unit

onKeypress :: forall r. 
  ({ sequence :: Maybe String, name :: Maybe String, ctrl :: Boolean, meta :: Boolean, shift :: Boolean } -> Effect Unit) -> Readable r -> Effect Unit
onKeypress = _onKeypress Just Nothing

getWindowSize :: Window -> Effect Size
getWindowSize w =
  case w of
    FullScreen -> identity
    Window s -> min s
    <$> ({ width: _, height: _ } <$> getCols <*> getRows)

safeWrite :: Window -> Canvas -> Effect Unit
safeWrite w canvas = do
  eol <- getEol
  da <- getWindowSize w
  write $ intercalate (SCU.singleton eol) $ projectOnWindow da canvas
  pure unit
