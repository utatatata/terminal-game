module Test.Chess
  ( testGetPiece
  , testCursorToInt
  , testGetMovablePoss
  , testJudgeCheck
  )
  where

import Prelude
import Data.List (fromFoldable, index, reverse)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Chess (Board, Col(..), Row(..), Piece(..), Player(..), SelectedBoard(..), posToInt, getMovablePoss, getBox, initBoard, judgeCheck)

testGetPiece :: Spec Unit
testGetPiece =
  describe "Chess" do
    describe "getBox" do
      it "a1" do
        getBox { col: ColA, row: Row1 } initBoard `shouldEqual` Just (SelectedBoard initBoard { selectedPos: { col: ColA, row: Row1 }, selectedBox: { piece: Rook, player: PWhite } })

testGetMovablePoss :: Spec Unit
testGetMovablePoss =
  describe "Chess" do
    describe "testGetMovablePoss" do
      it "White King" do
        map _.pos (getMovablePoss (SelectedBoard board { selectedPos: { col: ColB, row: Row2 }, selectedBox: { piece: King, player: PWhite } }))
          `shouldEqual` fromFoldable
            [ { col: ColA, row: Row2 }, { col: ColC, row: Row2 }, { col: ColB, row: Row3 }, { col: ColB, row: Row1 }
            , { col: ColA, row: Row3 }, { col: ColA, row: Row1 }, { col: ColC, row: Row3 }, { col: ColC, row: Row1 }
            ]
      it "Black Pawn" do
        map _.pos (getMovablePoss (SelectedBoard board { selectedPos: { col: ColD, row: Row7 }, selectedBox: { piece: Pawn { canBeEnPassant: true }, player: PBlack } }))
          `shouldEqual` fromFoldable
            [ { col: ColD, row: Row6 }, { col: ColD, row: Row5 } ]
      it "White Rook" do
        map _.pos (getMovablePoss (SelectedBoard board { selectedPos: { col: ColE, row: Row2 }, selectedBox: { piece: Rook, player: PWhite } }))
          `shouldEqual` fromFoldable
            [ { col: ColC, row: Row2 }, { col: ColD, row: Row2 }
            , { col: ColH, row: Row2 }, { col: ColG, row: Row2 }, { col: ColF, row: Row2 }
            , { col: ColE, row: Row8 }, { col: ColE, row: Row7 }, { col: ColE, row: Row6 }, { col: ColE, row: Row5 }, { col: ColE, row: Row4 }, { col: ColE, row: Row3 }
            , { col: ColE, row: Row1 }
            ]
      it "Black Knight" do
        map _.pos (getMovablePoss (SelectedBoard board { selectedPos: { col: ColC, row: Row6 }, selectedBox: { piece: Knight, player: PBlack } }))
          `shouldEqual` fromFoldable
            [ { col: ColB, row: Row8 }, { col: ColD, row: Row8 }
            , { col: ColB, row: Row4 }, { col: ColD, row: Row4 }
            , { col: ColA, row: Row7 }, { col: ColA, row: Row5 }
            , { col: ColE, row: Row7 }, { col: ColE, row: Row5 }
            ]
  where
  board = reverse $ fromFoldable $ map fromFoldable
    [ [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Just { piece: Pawn { canBeEnPassant: true }, player: PWhite }, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Just { piece: Knight, player: PBlack }, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Just { piece: King, player: PWhite }, Nothing, Nothing, Just { piece: Rook, player: PWhite }, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    ]

testJudgeCheck :: Spec Unit
testJudgeCheck =
  describe "Chess" do
    describe "judgeCheck" do
      it "White check" do
        judgeCheck PWhite board `shouldEqual` true
  where
  board = reverse $ fromFoldable $ map fromFoldable
    [ [ Nothing, Nothing, Nothing, Nothing, Just { piece: King, player: PBlack }, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Just { piece: Knight, player: PWhite }, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    ]

testCursorToInt :: Spec Unit
testCursorToInt =
  describe "Chess" do
    describe "posToInt" do
      it "{ col: ColA, row: Row1 } |-> { col: 0, row: 0 }" do
        posToInt { col: ColA, row: Row1 } `shouldEqual` { col: 0, row: 0 }
