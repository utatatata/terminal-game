module Main where

import Prelude

import Ansi.Codes (Color(..))
import Control.Monad.Rec.Class (forever, whileJust)
import Control.Monad.State.Trans (StateT, evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (find, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import TerminalGame (Key(..), Window(..), dot, flush, moveTo, popPressedKeys, quit, runGame)
import TerminalGame.Terminal (write, initTerminal, onKeypress, resetTerminal)
import Node.Process (stdin)
import Effect.Ref as Ref

-- tetrominoL :: Game Unit
-- tetrominoL = local do
--   moveTo (-1) 0 0
--   dot '#'
--   moveTo 0 0 0
--   dot '#'
--   moveTo 1 0 0
--   dot '#'
--   moveTo 1 (-1) 0
--   dot '#'

-- game :: Game Unit
-- game = do
--   forever
--     do
--       lift do
--         moveTo 0 0 0
--         setFg Blue
--         tetrominoL
--       lift do
--         moveTo 7 7 0
--         setFg Yellow
--         tetrominoL
--       lift do
--         moveTo 9 9 0
--         setFg Red
--         dot 't'
--       lift flush

data Piece
  = King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn

data Mode
  = WhitePlaying
  | WhiteMoving
  | BlackPlaying
  | BlackMoving
  | Judgement

type GameState =
  { board :: List (List (Maybe Piece))
  , end :: Boolean
  , mode :: Mode
  }

initGameState :: GameState
initGameState =
  { board: fromFoldable
      [ fromFoldable [ Just Rook, Just Knight, Just Bishop, Just Queen, Just King, Just Bishop, Just Knight, Just Rook ]
      , fromFoldable [ Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn ]
      , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , fromFoldable [ Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn, Just Pawn ]
      , fromFoldable [ Just Rook, Just Knight, Just Bishop, Just Queen, Just King, Just Bishop, Just Knight, Just Rook ]
      ]
  , end: false
  , mode: WhitePlaying
  }

-- game2 :: Game Unit
-- game2 = flip evalStateT initGameState $ forever do
--   { mode, board } <- get
--   pressedKeys <- lift2 $ popPressedKeys
--   pure unit
--   lift tick
--   -- lift2 flush
-- 
--       -- for_ pressedKeys \{ key } -> lift2 do
--       --   line $ show key
--       --   move $ vec3 0 1 0
--       -- pure $ Just unit
-- 
--       -- if judgeEnd pressedKeys then
--       --   pure Nothing
--       -- else
--       --   case mode of
--       --     WhitePlaying -> do
--       --       lift2 do
--       --         moveTo $ vec3 3 3 0
--       --         local $
--       --           forWithIndex_ board \y line ->
--       --             forWithIndex_ line \x p -> do
--       --               moveTo $ vec3 x y 0
--       --       pure $ Just unit
--       --     _ -> pure $ Just unit
-- 
--     where
--     lift2 :: Game ~> StateT GameState Game
--     lift2 = lift <<< lift
-- 
--     judgeEnd :: List KeyState -> Boolean
--     judgeEnd keys = case find (\{ key, ctrl } -> key == Escape || key == Q || (key == C || key == D) && ctrl) keys of
--       Just _ -> true
--       Nothing -> false
-- 
--     drawPiece :: Piece -> Char
--     drawPiece = case _ of
--       King -> 'K'
--       Queen -> 'Q'
--       Rook -> 'R'
--       Knight -> 'N'
--       Bishop -> 'B'
--       Pawn -> 'P'

main :: Effect Unit
main = do
  -- runGameLoop { fps: 20, window: FullScreen } game
  -- runGameLoop { fps: 20, window: FullScreen } game2
   runGame { window: FullScreen, fps: 20 } do
     keys <- popPressedKeys
     if keys == Nil then
       quit $ pure unit
     else do
       moveTo 1 1 0 
       dot 'a'
       moveTo 2 2 0
       dot 'b'
       moveTo 3 3 0
       dot 'c'
       flush
       quit $ pure unit
