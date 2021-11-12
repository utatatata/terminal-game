module TerminalGame.Types where

import Prelude
import Ansi.Codes (Color)
import Ansi.Output (background, foreground, withGraphics)
import Control.Monad.ST as ST
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Foldable (fold, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Unfoldable (replicate)

data Window
  = FullScreen
  | Window Size

data Key
  = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
  | KeyOne | KeyTwo | KeyThree | KeyFour | KeyFive | KeySix | KeySeven | KeyEight | KeyNine | KeyZero
  | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6 | KeyF7 | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12
  | KeyHyphen | KeyEqual | KeyBackslash | KeyBackquote | KeyBracketOpen | KeyBracketClose | KeySemicolon | KeyQuote | KeyComma | KeyDot | KeySlash
  | KeyEscape | KeyTab | KeyBackspace | KeyDelete
  | KeyUp | KeyDown | KeyLeft | KeyRight
  | KeyHome | KeyEnd | KeyInsert | KeyPageUp | KeyPageDown

instance showKey :: Show Key where
  show KeyA = "A"
  show KeyB = "B"
  show KeyC = "C"
  show KeyD = "D"
  show KeyE = "E"
  show KeyF = "J"
  show KeyG = "G"
  show KeyH = "H"
  show KeyI = "I"
  show KeyJ = "J"
  show KeyK = "K"
  show KeyL = "L"
  show KeyM = "M"
  show KeyN = "N"
  show KeyO = "O"
  show KeyP = "P"
  show KeyQ = "Q"
  show KeyR = "R"
  show KeyS = "S"
  show KeyT = "T"
  show KeyU = "U"
  show KeyV = "V"
  show KeyW = "W"
  show KeyX = "X"
  show KeyY = "Y"
  show KeyZ = "Z"
  show KeyOne = "1"
  show KeyTwo = "2"
  show KeyThree = "3"
  show KeyFour = "4"
  show KeyFive = "5"
  show KeySix = "6"
  show KeySeven = "7"
  show KeyEight = "8"
  show KeyNine = "9"
  show KeyZero = "0"
  show KeyF1 = "F1"
  show KeyF2 = "F2"
  show KeyF3 = "F3"
  show KeyF4 = "F4"
  show KeyF5 = "F5"
  show KeyF6 = "F6"
  show KeyF7 = "F7"
  show KeyF8 = "F8"
  show KeyF9 = "F9"
  show KeyF10 = "F10"
  show KeyF11 = "F11"
  show KeyF12 = "F12"
  show KeyEscape  = "Escape"
  show KeyTab  = "Tab"
  show KeyBackspace  = "Backspace"
  show KeyDelete = "Delete"
  show KeyUp  = "Up"
  show KeyDown  = "Down"
  show KeyLeft  = "Left"
  show KeyRight = "Right"
  show KeyHome  = "Home"
  show KeyEnd  = "End"
  show KeyInsert  = "Insert"
  show KeyPageUp  = "PageUp"
  show KeyPageDown = "PageDown"
  show KeyHyphen  = "-"
  show KeyEqual  = "="
  show KeyBackslash  = "\\"
  show KeyBackquote  = "`"
  show KeyBracketOpen  = "["
  show KeyBracketClose  = "]"
  show KeySemicolon  = ";"
  show KeyQuote  = "'"
  show KeyComma  = ","
  show KeyDot  = "."
  show KeySlash = "/"

derive instance eqKey :: Eq Key

validateKey :: { sequence :: Maybe String, name :: Maybe String } -> Maybe Key
validateKey { sequence, name } =
  case name of
    Just "a" -> Just KeyA
    Just "b" -> Just KeyB
    Just "c" -> Just KeyC
    Just "d" -> Just KeyD
    Just "e" -> Just KeyE
    Just "f" -> Just KeyF
    Just "g" -> Just KeyG
    Just "h" -> Just KeyH
    Just "i" -> Just KeyI
    Just "j" -> Just KeyJ
    Just "k" -> Just KeyK
    Just "l" -> Just KeyL
    Just "m" -> Just KeyM
    Just "n" -> Just KeyN
    Just "o" -> Just KeyO
    Just "p" -> Just KeyP
    Just "q" -> Just KeyQ
    Just "r" -> Just KeyR
    Just "s" -> Just KeyS
    Just "t" -> Just KeyT
    Just "u" -> Just KeyU
    Just "v" -> Just KeyV
    Just "w" -> Just KeyW
    Just "x" -> Just KeyX
    Just "y" -> Just KeyY
    Just "z" -> Just KeyZ
    Just "1" -> Just KeyOne
    Just "2" -> Just KeyTwo
    Just "3" -> Just KeyThree
    Just "4" -> Just KeyFour
    Just "5" -> Just KeyFive
    Just "6" -> Just KeySix
    Just "7" -> Just KeySeven
    Just "8" -> Just KeyEight
    Just "9" -> Just KeyNine
    Just "0" -> Just KeyZero
    Just "f1"  -> Just KeyF1
    Just "f2"  -> Just KeyF2
    Just "f3"  -> Just KeyF3
    Just "f4"  -> Just KeyF4
    Just "f5"  -> Just KeyF5
    Just "f6"  -> Just KeyF6
    Just "f7"  -> Just KeyF7
    Just "f8"  -> Just KeyF8
    Just "f9"  -> Just KeyF9
    Just "f10" -> Just KeyF10
    Just "f11" -> Just KeyF11
    Just "f12" -> Just KeyF12
    Just "escape"    -> Just KeyEscape 
    Just "tab"       -> Just KeyTab 
    Just "backspace" -> Just KeyBackspace 
    Just "delete"    -> Just KeyDelete
    Just "up"        -> Just KeyUp 
    Just "down"      -> Just KeyDown 
    Just "left"      -> Just KeyLeft 
    Just "right"     -> Just KeyRight
    Just "home"      -> Just KeyHome 
    Just "end"       -> Just KeyEnd 
    Just "insert"    -> Just KeyInsert 
    Just "pageup"    -> Just KeyPageUp 
    Just "pagedown"  -> Just KeyPageDown
    _ -> case sequence of
      Just "-"  -> Just KeyHyphen
      Just "="  -> Just KeyEqual
      Just "\\" -> Just KeyBackslash
      Just "`"  -> Just KeyBackquote
      Just "["  -> Just KeyBracketOpen
      Just "]"  -> Just KeyBracketClose
      Just ";"  -> Just KeySemicolon
      Just "'"  -> Just KeyQuote
      Just ","  -> Just KeyComma
      Just "."  -> Just KeyDot
      Just "/"  -> Just KeySlash
      _ -> Nothing

type KeyState =
  { key :: Key
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

type FPS = Int

type Size = { width :: Int, height :: Int }

type Vector2 = { x :: Int, y :: Int }

vec2 :: Int -> Int -> Vector2
vec2 = { x: _, y: _ }

type Vector3 = { x :: Int, y :: Int, z :: Int }

vec3 :: Int -> Int -> Int -> Vector3
vec3 = { x: _, y: _, z: _ }

type Canvas = Map Vector3 { fg :: Color, bg :: Color, char :: Char }

projectOnWindow :: Size -> Canvas -> Array String
projectOnWindow size canvas =
  bufferToStrs $
    runMatrix do
      buffer <- initBuffer size
      forWithIndex_ canvas \{ x, y, z } { fg, bg, char } ->
        STA.peek y buffer >>= case _ of
          Just line -> do
            void $ STA.modify x (flip minDepth { depth: z, str: withGraphics (foreground fg <> background bg) (SCU.singleton char) }) line
            pure unit
          Nothing -> pure unit
      pure buffer
  where
  initBuffer :: forall h. Size -> ST h (STArray h (STArray h { depth :: Int, str :: String }))
  initBuffer { width, height } = do
    buffer <- STA.new
    for_ (replicate height (replicate width { depth: 0, str: " " }) :: Array (Array { depth :: Int, str :: String })) \l -> do
      l' <- STA.thaw l
      void $ STA.push l' buffer
      pure unit
    pure buffer

  runMatrix :: forall a. (forall h. ST h (STArray h (STArray h a))) -> Array (Array a)
  runMatrix m = ST.run (m >>= STA.unsafeFreeze >>= traverse STA.unsafeFreeze)

  bufferToStrs :: Array (Array { depth :: Int, str :: String }) -> Array String
  bufferToStrs = map $ fold <<< map _.str

  minDepth :: { depth :: Int, str :: String } -> { depth :: Int, str :: String } -> { depth :: Int, str :: String }
  minDepth x y = if x.depth < y.depth then x else y
