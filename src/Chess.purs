module Chess where

import Prelude

import Ansi.Codes (Color(..))
import Control.Monad.Rec.Class (forever, whileJust)
import Control.Monad.State.Trans (StateT, evalStateT, get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Foldable (any, find, foldl, foldMap, foldr, for_)
import Data.Foldable.Extra (forWhileJust_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (odd)
import Data.List (List(..), alterAt, filter, fromFoldable, index, modifyAt, reverse, updateAt, zipWith)
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import TerminalGame (Game, Key(..), KeyState, Window(..), dot, flush, hline, local, moveTo, popPressedKey, quit, runGame, setBg, setFg, vline)
import Partial.Unsafe (unsafePartial)
import Ansi.Output (background, foreground, withGraphics)
import Data.String.CodeUnits as SCU


playChess :: Effect Unit
playChess = do
  runGame { fps: 20, window: FullScreen } initChessState chess

initChessState :: ChessState
initChessState =
  { board: initBoard
  , mode: Init
  , cursorPos: { col: ColE, row: Row3 }
  }

initBoard :: Board
initBoard = reverse $ fromFoldable
  [ pure <<< { piece: _, player: PBlack } <$> fromFoldable [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]
  , pure <<< { piece: _, player: PBlack } <$> fromFoldable (map ((#) { canBeEnPassant: false }) [ Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn ])
  , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
  , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
  , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
  , fromFoldable [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
  , pure <<< { piece: _, player: PWhite } <$> fromFoldable (map ((#) { canBeEnPassant: false }) [ Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn ])
  , pure <<< { piece: _, player: PWhite } <$> fromFoldable [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]
  ]


{---------- Main loop ----------}

type ChessState =
  { board :: Board
  , mode :: Mode
  , cursorPos :: Pos
  }

data Mode
  = Init
  | Selecting Player { isChecked :: Boolean }
  | Moving Player Pos { isChecked :: Boolean }
  | Judgement Player
  | Menu MenuSelection Mode
  | Quit

data MenuSelection
  = MenuReturn
  | MenuQuit

derive instance eqMenuSelection :: Eq MenuSelection

nextMenu :: MenuSelection -> MenuSelection
nextMenu = case _ of
  MenuReturn -> MenuQuit
  MenuQuit -> MenuQuit

previousMenu :: MenuSelection -> MenuSelection
previousMenu = case _ of
  MenuReturn -> MenuReturn
  MenuQuit -> MenuReturn

chess :: Game ChessState Unit
chess = do
  moveTo 4 1 0
  local $ gets _.mode >>= case _ of
    Init -> do
      modify_ \s -> s { mode = Selecting PWhite { isChecked: false } }

    Selecting player { isChecked } -> do
      (=<<) keyStateToButton <$> popPressedKey >>= case _ of
        Just (BDirection dir) -> maybe (pure unit) (\pos -> modify_ \s -> s { cursorPos = pos }) =<< gets (movePos dir <<< _.cursorPos)
        Just BSelect -> do
          { board, cursorPos } <- get
          case eq player <<< getSelectedPlayer <$> getBox cursorPos board of
            Just true -> modify_\s -> s { mode = Moving player cursorPos { isChecked } }
            _ -> pure unit
        Just BCancel    -> pure unit
        Just BMenu      -> modify_ \s -> s { mode = Menu MenuReturn (Selecting player { isChecked }) }
        Just (BPiece _) -> pure unit -- TODO
        Just (BCol _)   -> pure unit
        Just (BRow _)   -> pure unit
        Nothing         -> pure unit

      do
        moveTo 1 0 0
        drawLabel false $ show player <> " Selecting"
        { board, cursorPos } <- get
        moveTo 0 4 0
        drawBoard cursorPos board Nil
        flush

    Moving player pos { isChecked } ->
      (gets _.board <#> getBox pos) >>= case _ of
        Just selectedBoard@(SelectedBoard board { selectedPos, selectedBox }) | selectedBox.player == player -> do
          let movablePoss = getMovablePoss selectedBoard
          (=<<) keyStateToButton <$> popPressedKey >>= case _ of
            Just (BDirection dir) -> maybe (pure unit) (\pos -> modify_ \s -> s { cursorPos = pos }) =<< gets (movePos dir <<< _.cursorPos)
            Just BSelect -> do
              { cursorPos } <- get
              case find ((==) cursorPos <<< _.pos) movablePoss of
                Just { getUpdatedBoard } -> modify_ \s -> s { board = getUpdatedBoard unit, mode = Judgement player }
                _ -> pure unit
              pure unit
            Just BCancel    -> modify_ \s -> s { mode = Selecting player { isChecked } }
            Just BMenu      -> modify_ \s -> s { mode = Menu MenuReturn (Moving player pos { isChecked }) }
            Just (BPiece _) -> pure unit
            Just (BCol _)   -> pure unit -- TODO
            Just (BRow _)   -> pure unit -- TODO
            Nothing         -> pure unit

          do
            moveTo 2 0 0
            drawLabel false $ show player <> " Moving"
            { board, cursorPos } <- get
            moveTo 0 4 0
            drawBoard cursorPos board (map _.pos movablePoss)
            flush

        _ -> modify_ \s -> s { mode = Selecting player { isChecked } }

    Judgement player -> do
      modify_ \s -> s
        { mode = Selecting (nextPlayer player) { isChecked: judgeCheck player s.board }
        -- reset en passant flag
        , board = s.board <#> (map <<< map) case _ of
            { piece: Pawn { canBeEnPassant: true }, player } ->
              { piece: Pawn { canBeEnPassant: false }, player }
            box -> box
        }

      do
        moveTo 1 0 0
        drawLabel false $ show player <> " Judgement"
        { board, cursorPos } <- get
        moveTo 0 4 0
        drawBoard cursorPos board Nil
        flush

    Menu selection mode -> do
      (=<<) keyStateToButton <$> popPressedKey >>= case _ of
        Just (BDirection DUp)   -> modify_ \s -> s { mode = Menu (previousMenu selection) mode }
        Just (BDirection DDown) -> modify_ \s -> s { mode = Menu (nextMenu     selection) mode }
        Just BSelect -> case selection of
          MenuReturn -> modify_ \s -> s { mode = mode }
          MenuQuit   -> modify_ \s -> s { mode = Quit }
        Just BMenu -> modify_ \s -> s { mode = mode }
        _ -> pure unit

      drawMenu selection
      flush
    Quit -> do
      -- TODO: display quit screen
      quit $ pure unit


{---------- Views ----------}

drawBoard :: Pos -> Board -> List Pos -> Game ChessState Unit
drawBoard cursorPos board movablePoss = local do
  moveTo 2 0 0
  hline "a b c d e f g h"
  moveTo 2 1 0
  hline "---------------"
  moveTo 0 2 0
  vline "87654321"
  moveTo 1 2 0
  vline "||||||||"
  moveTo 2 2 0

  forWithIndex_ board \row xs ->
    forWithIndex_ xs \col x -> local do
      -- checkered board
      if odd $ row + col then
        setBg BrightRed
      else
        setBg BrightBlack

      -- positions where selected piece can move
      when ((_ /= Nothing) $ find ((==) { col, row } <<< posToInt) movablePoss) do
        setBg White

      -- cursor
      when (posToInt cursorPos == { col, row }) do
        setBg BrightBlue

      moveTo (col * 2) (7 - row) 0
      drawPiece x

  where
  drawPiece :: Maybe { piece :: Piece, player :: Player } -> Game ChessState Unit
  drawPiece = case _ of
    Just { piece, player } -> do
      setFg $ playerToColor player
      case piece of
        King   -> dot 'K'
        Queen  -> dot 'Q'
        Rook   -> dot 'R'
        Bishop -> dot 'B'
        Knight -> dot 'N'
        Pawn _ -> dot 'P'
    Nothing     -> dot ' '

drawLabel :: Boolean -> String -> Game ChessState Unit
drawLabel selected s = local do

  let l = "+" <> (SCU.fromCharArray $ replicate (SCU.length s) '-') <> "+"
  hline l
  moveTo 0 2 0
  hline l
  moveTo 0 1 0
  dot '|'
  moveTo (SCU.length s + 1) 1 0
  dot '|'

  when selected do
    setFg Black
    setBg White
  moveTo 1 1 0
  hline s

drawMenu :: MenuSelection -> Game ChessState Unit
drawMenu selection = local do
  let hl = "+" <> (SCU.fromCharArray $ replicate 22 '-') <> "+"
  let vl = SCU.fromCharArray $ replicate 7 '|'
  hline hl
  moveTo 0 8 0
  hline hl
  moveTo 0 1 0
  vline vl
  moveTo 23 1 0
  vline vl

  moveTo 2 1 0
  drawLabel (selection == MenuReturn) "Return to the game"
  moveTo 4 5 0
  drawLabel (selection == MenuQuit) "Quit the game"


{---------- Datas ----------}

data Piece
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn { canBeEnPassant :: Boolean }

derive instance eqPiece :: Eq Piece

instance showPiece :: Show Piece where
  show King     = "King"
  show Queen    = "Queen"
  show Rook     = "Rook"
  show Bishop   = "Bishop"
  show Knight   = "Knight"
  show (Pawn _) = "Pawn"

data Player
  = PWhite
  | PBlack

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
  show PWhite = "White"
  show PBlack = "Black"

nextPlayer :: Player -> Player
nextPlayer PWhite = PBlack
nextPlayer PBlack = PWhite

playerToColor :: Player -> Color
playerToColor PWhite = White
playerToColor PBlack = Black

data Direction = DLeft | DRight | DUp | DDown

data Button
  = BDirection Direction
  | BSelect | BCancel | BMenu
  | BPiece Piece
  | BCol Col
  | BRow Row

keyStateToButton :: KeyState -> Maybe Button
keyStateToButton = case _ of
  { key: KeyLeft      } -> pure $ BDirection DLeft
  { key: KeyRight     } -> pure $ BDirection DRight
  { key: KeyUp        } -> pure $ BDirection DUp
  { key: KeyDown      } -> pure $ BDirection DDown
  { key: KeyReturn    } -> pure BSelect
  { key: KeySpace     } -> pure BSelect
  { key: KeyBackspace } -> pure BCancel
  { key: KeyEscape    } -> pure BMenu
  _            -> Nothing

data Col
  = ColA | ColB | ColC | ColD | ColE | ColF | ColG | ColH

derive instance eqCol :: Eq Col

instance showCol :: Show Col where
  show ColA = "a"
  show ColB = "b"
  show ColC = "c"
  show ColD = "d"
  show ColE = "e"
  show ColF = "f"
  show ColG = "g"
  show ColH = "h"

colToInt :: Col -> Int
colToInt ColA = 0
colToInt ColB = 1
colToInt ColC = 2
colToInt ColD = 3
colToInt ColE = 4
colToInt ColF = 5
colToInt ColG = 6
colToInt ColH = 7

moveColLeft :: Col -> Maybe Col
moveColLeft = case _ of
  ColA -> Nothing
  ColB -> pure ColA
  ColC -> pure ColB
  ColD -> pure ColC
  ColE -> pure ColD
  ColF -> pure ColE
  ColG -> pure ColF
  ColH -> pure ColG

moveColRight :: Col -> Maybe Col
moveColRight = case _ of
  ColA -> pure ColB
  ColB -> pure ColC
  ColC -> pure ColD
  ColD -> pure ColE
  ColE -> pure ColF
  ColF -> pure ColG
  ColG -> pure ColH
  ColH -> Nothing

data Row
  = Row1 | Row2 | Row3 | Row4 | Row5 | Row6 | Row7 | Row8

derive instance eqRow :: Eq Row

instance showRow :: Show Row where
  show Row1 = "1"
  show Row2 = "2"
  show Row3 = "3"
  show Row4 = "4"
  show Row5 = "5"
  show Row6 = "6"
  show Row7 = "7"
  show Row8 = "8"

rowToInt :: Row -> Int
rowToInt Row1 = 0
rowToInt Row2 = 1
rowToInt Row3 = 2
rowToInt Row4 = 3
rowToInt Row5 = 4
rowToInt Row6 = 5
rowToInt Row7 = 6
rowToInt Row8 = 7

moveRowUp :: Row -> Maybe Row
moveRowUp = case _ of
  Row1 -> pure Row2
  Row2 -> pure Row3
  Row3 -> pure Row4
  Row4 -> pure Row5
  Row5 -> pure Row6
  Row6 -> pure Row7
  Row7 -> pure Row8
  Row8 -> Nothing

moveRowDown :: Row -> Maybe Row
moveRowDown = case _ of
  Row1 -> Nothing
  Row2 -> pure Row1
  Row3 -> pure Row2
  Row4 -> pure Row3
  Row5 -> pure Row4
  Row6 -> pure Row5
  Row7 -> pure Row6
  Row8 -> pure Row7

type Pos = { col :: Col, row :: Row }

allPoss :: List Pos
allPoss = do
   col <- fromFoldable [ ColA, ColB, ColC, ColD, ColE, ColF, ColG, ColH ]
   row <- fromFoldable [ Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8 ]
   pure { col, row }

posToInt :: Pos -> { row :: Int, col :: Int }
posToInt c = c { col = colToInt c.col, row = rowToInt c.row }

movePos :: Direction -> Pos -> Maybe Pos
movePos DLeft  { col, row } = { col: _, row    } <$> moveColLeft col
movePos DRight { col, row } = { col: _, row    } <$> moveColRight col
movePos DUp    { col, row } = { col,    row: _ } <$> moveRowUp row
movePos DDown  { col, row } = { col,    row: _ } <$> moveRowDown row

type Box = { piece :: Piece, player :: Player }

type Board = List (List (Maybe Box))

data SelectedBoard = SelectedBoard (List (List (Maybe Box))) { selectedPos :: Pos, selectedBox :: Box }

derive instance eqSelectedBoard :: Eq SelectedBoard

instance showSelectedBoard :: Show SelectedBoard where
  show (SelectedBoard board { selectedPos, selectedBox }) = "(SelectedBoard " <> show board <> "{ selectedPos: " <> show selectedPos <> ", selectedBox: " <> show selectedBox <> " })"

getSelectedPos :: SelectedBoard -> Pos
getSelectedPos (SelectedBoard _ { selectedPos }) = selectedPos

getSelectedBox :: SelectedBoard -> Box
getSelectedBox (SelectedBoard _ { selectedBox }) = selectedBox

getSelectedPiece :: SelectedBoard -> Piece
getSelectedPiece = _.piece <<< getSelectedBox

getSelectedPlayer :: SelectedBoard -> Player
getSelectedPlayer = _.player <<< getSelectedBox

getBox :: Pos -> Board -> Maybe SelectedBoard
getBox pos@{ col, row } board = do
  box <- unsafePartial (fromJust $ (_ `index` colToInt col) =<< (_ `index` rowToInt row) board)
  pure $ SelectedBoard board { selectedPos: pos, selectedBox: box }

getMovablePoss :: SelectedBoard -> List { pos :: Pos, getUpdatedBoard :: Unit -> Board }
getMovablePoss selectedBoard@(SelectedBoard board { selectedPos, selectedBox: selectedBox@{ piece, player } }) =
  case piece of
    King -> withUpdatedBoard <$> fromDirections
      [ [ DLeft ], [ DRight ], [ DUp ], [ DDown ]
      , [ DUp, DLeft ],  [ DDown, DLeft ]
      , [ DUp, DRight ], [ DDown, DRight ]
      ]
    Queen -> rookPossWith <> bishopPossWith
    Rook -> rookPossWith
    Bishop -> bishopPossWith
    Knight -> withUpdatedBoard <$> fromDirections
      [ [ DLeft,  DUp,    DUp ]
      , [ DRight, DUp,    DUp ]
      , [ DLeft,  DDown,  DDown ]
      , [ DRight, DDown,  DDown ]
      , [ DUp,    DLeft,  DLeft ]
      , [ DDown,  DLeft,  DLeft ]
      , [ DUp,    DRight, DRight ]
      , [ DDown,  DRight, DRight ]
      ]
    Pawn { canBeEnPassant } -> pawnPossWith
  where
  rookPossWith = withUpdatedBoard <$> fromDirectionsReplicate [ [ DLeft ], [ DRight ], [ DUp ], [ DDown ] ]
  bishopPossWith = withUpdatedBoard <$> fromDirectionsReplicate 
    [ [ DLeft, DUp ]
    , [ DRight, DUp ]
    , [ DLeft, DDown ]
    , [ DRight, DDown ]
    ]
  
  -- TODO: en passant
  withUpdatedBoard :: Pos -> { pos :: Pos, getUpdatedBoard :: Unit -> Board }
  withUpdatedBoard pos = { pos, getUpdatedBoard: \_ -> movePiece pos }

  pawnPossWith :: List { pos :: Pos, getUpdatedBoard :: Unit -> Board }
  pawnPossWith =
    -- cannot move to the position where the opponent's piece is
    (withUpdatedBoard <$> (filter (maybe true (const false) <<< flip getBox board) $ fromDirections
      case player, selectedPos.row of
        -- TODO: turn on en passant flag
        PWhite, Row2 -> [ [ DUp ], [ DUp, DUp ] ]
        PWhite, _    -> [ [ DUp ] ]
        PBlack, Row7 -> [ [ DDown ], [ DDown, DDown ] ]
        PBlack, _    -> [ [ DDown ] ]))
    <> (withUpdatedBoard <$> (foldr go Nil <<< map directionsToMove $
      case player of
        PWhite -> [ [ DUp, DLeft ], [ DUp, DRight ] ]
        PBlack -> [ [ DDown, DLeft ], [ DDown, DRight ] ]))
    -- TODO: repair en passant
    <> (withUpdatedBoard <$> (foldr goEnPassant Nil <<< map directionsToMove $
      case player, selectedPos.row of
        PWhite, Row5 -> [ [ DLeft ], [ DRight ] ]
        PBlack, Row4 -> [ [ DLeft ], [ DRight ] ]
        _, _         -> []))
    where
      go :: (Pos -> Maybe Pos) -> List Pos -> List Pos
      go move poss = case move selectedPos of
        Just pos -> case getBox pos board of
          -- get opponent's piece and move if it is diagonally in front
          Just sb | getSelectedPlayer sb /= player ->
            Cons pos poss
          -- empty position
          _ -> poss
        -- invalid move
        Nothing -> poss
      goEnPassant :: (Pos -> Maybe Pos) -> List Pos -> List Pos
      goEnPassant move poss = case move selectedPos of
        Just pos -> case getBox pos board of
          -- en passant
          Just sb | getSelectedPlayer sb /= player ->
            case getSelectedPiece sb of
              Pawn { canBeEnPassant: true } ->
                Cons pos poss
              _ -> poss
          -- empty position
          _ -> poss
        -- invalid move
        Nothing -> poss
  
  movePiece :: Pos -> Board
  movePiece pos = (set selectedPos Nothing <<< set pos (pure selectedBox)) board
    where
    set :: Pos -> Maybe Box -> Board -> Board
    set { col, row } box board =
      unsafePartial (fromJust $ alterAt (rowToInt row) (updateAt (colToInt col) box) board)
  
  fromDirections :: Array (Array Direction) -> List Pos
  fromDirections = foldr go Nil <<< map directionsToMove
    where
    go :: (Pos -> Maybe Pos) -> List Pos -> List Pos
    go move poss = case move selectedPos of
      Just pos -> case getBox pos board of
        -- empty position
        Nothing -> Cons pos poss
        Just sb ->
          -- the position of the opponent's piece can be moved
          if getSelectedPlayer sb /= player then
            Cons pos poss
          else
            poss
      -- invalid move
      Nothing -> poss

  fromDirectionsReplicate :: Array (Array Direction) -> List Pos
  fromDirectionsReplicate = join <<< fromFoldable <<< map (replicate <<< directionsToMove)
    where
    replicate :: (Pos -> Maybe Pos) -> List Pos
    replicate initMove = go initMove Nil
      where
      go :: (Pos -> Maybe Pos) -> List Pos -> List Pos
      go move poss =
        case move selectedPos of
          Just pos -> case getBox pos board of
            Nothing -> go (initMove <=< move) (Cons pos poss)
            -- if there is a piece in the direction of travel, it will not go any futher
            Just sb ->
              -- can move to the box of the oppenent's piece
              if getSelectedPlayer sb /= player then
                Cons pos poss
              else
                poss
          _ -> poss

  directionsToMove :: Array Direction -> Pos -> Maybe Pos
  directionsToMove = foldr ((<=<) <<< movePos) pure

judgeCheck :: Player -> Board -> Boolean
judgeCheck player board =
  any (maybe false (ownPiece && any existsOpponentsKing <<< map _.pos <<< getMovablePoss) <<< flip getBox board) allPoss
  where
  ownPiece :: SelectedBoard -> Boolean
  ownPiece = eq player <<< getSelectedPlayer

  existsOpponentsKing :: Pos -> Boolean
  existsOpponentsKing = fromMaybe false <<< map (eq King <<< getSelectedPiece && notEq player <<< getSelectedPlayer) <<< flip getBox board
  