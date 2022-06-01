module Panepon where

import Prelude

import Ansi.Codes (Color(..))
import Control.Monad.State.Trans (evalStateT, get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime, diff)
import Data.DateTime.Extra (getNow)
import Data.Foldable (any, find, foldl, foldMap, foldr, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (odd)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate, replicateA)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import TerminalGame (Game, Key(..), KeyState, Window(..), dot, flush, hline, local, move, moveTo, popPressedKey, quit, runGame, setBg, setFg, vline)
import Partial.Unsafe (unsafePartial)
import Ansi.Output (background, foreground, withGraphics)
import Data.String.CodeUnits as SCU


playPanepon :: Effect Unit
playPanepon = do
  board <- initBoard
  runGame { fps: 20, window: FullScreen } (initPaneponState board) panepon

type PaneponState =
  { board :: Board
  , mode :: Mode
  , cursorPos :: Pos
  }

initPaneponState :: Board -> PaneponState
initPaneponState board =
  { board
  , mode: Init
  , cursorPos: { col: 3, row: 6 }
  }

initBoard :: Effect Board
initBoard = do
  rows <- replicateA 6 randomRow
  pure $ replicate 6 emptyRow <> rows
  where
  randomRow :: Effect (List (Maybe Block))
  randomRow = replicateA maxRow (pure <$> randomBlock)
  emptyRow :: List (Maybe Block)
  emptyRow = replicate maxRow Nothing

type Board = List (List (Maybe Block))

data Block
  = Heart
  | Circle
  | Triangle
  | Star
  | Diamond

blockToColor :: Block -> Color
blockToColor = case _ of
  Heart -> Red
  Circle -> Green
  Triangle -> Blue
  Star -> Yellow
  Diamond -> Magenta

randomBlock :: Effect Block
randomBlock = randomInt 1 5 <#> case _ of
  1 -> Heart
  2 -> Circle
  3 -> Triangle
  4 -> Star
  _ -> Diamond

maxCol :: Int
maxCol = 6

maxRow :: Int
maxRow = 12

data Mode
  = Init
  | Play
  | Remove RemoveMode { startTime :: DateTime, removingPoss :: List Pos }

data RemoveMode
  = Emphasise 
  | RemoveOneByOne

type Pos = { col :: Int, row :: Int }

panepon :: Game PaneponState Unit
panepon = gets _.mode >>= case _ of
  Init -> do
    { board } <- get
    moveTo 0 0 0
    boardView board
    flush
  Play -> do
    pure unit
  Remove removeMode { startTime, removingPoss } -> case removeMode of
    Emphasise -> do
      -- TODO: emphasise removing blocks
      now <- liftEffect getNow
      if (diff now startTime :: Milliseconds) > Milliseconds 800.0 then do
        modify_ \s -> s { mode = Remove RemoveOneByOne { startTime: now, removingPoss } }
      else
        pure unit
    RemoveOneByOne ->
      case removingPoss of
        Nil -> modify_ \s -> s { mode = Play }
        Cons pos poss -> do
          -- TODO: removing animation
          now <- liftEffect getNow
          if (diff now startTime :: Milliseconds) > Milliseconds 300.0 then
            modify_ \s -> s { mode = Remove RemoveOneByOne { startTime: now, removingPoss: poss } }
          else
            pure unit

boardView :: Board -> Game PaneponState Unit
boardView board = local $
  forWithIndex_ board \row blocks ->
    forWithIndex_ blocks \col block ->
      case blockToColor <$> block of
        Nothing -> pure unit
        Just color -> do
          moveTo (col * 2) row 0
          setBg color
          dot ' '
          move 1 0 0
          setBg color
          dot ' '

