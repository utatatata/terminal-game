module Test.TerminalGame.Types
  ( testProjectOnWindow
  ) where

import Prelude
import Ansi.Codes (Color(..))
import Ansi.Output (background, foreground, withGraphics)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TerminalGame.Types (projectOnWindow, vec3)

testProjectOnWindow :: Spec Unit
testProjectOnWindow =
  describe "TerminalGame.Types" do
    describe "projectOnWindow" do
      it "overwrite" do
        projectOnWindow { width: 5, height: 2 }
          (M.fromFoldable
            [ Tuple (vec3 0 0 0)    { fg: White, bg: Black, mode: Nothing, char: 'a' }
            , Tuple (vec3 0 0 (-1)) { fg: White, bg: Black, mode: Nothing, char: 'b' }
            ])
          `shouldEqual` [ toStr White Black 'b' <> "    ", "     " ]
      it "out of screen" do
        projectOnWindow { width: 5, height: 2 } (M.fromFoldable [ Tuple (vec3 5 0 0) { fg: White, bg: Black, mode: Nothing, char: 'a' } ])
          `shouldEqual` [ "     ", "     " ]
        projectOnWindow { width: 5, height: 2 } (M.fromFoldable [ Tuple (vec3 0 2 0) { fg: White, bg: Black, mode: Nothing, char: 'a' } ])
          `shouldEqual` [ "     ", "     " ]
      it "change color" do
        projectOnWindow { width: 3, height: 3 }
          (M.fromFoldable
            [ Tuple (vec3 0 0 0) { fg: Red,    bg: Black, mode: Nothing, char: 'r' }
            , Tuple (vec3 1 1 0) { fg: Blue,   bg: Black, mode: Nothing, char: 'b' }
            , Tuple (vec3 2 2 0) { fg: Yellow, bg: Black, mode: Nothing, char: 'y' }
            ])
            `shouldEqual` [ toStr Red Black 'r' <> "  ", " " <> toStr Blue Black 'b' <> " ", "  " <> toStr Yellow Black 'y' ]
  where
  toStr :: Color -> Color -> Char -> String
  toStr fg bg char = withGraphics (foreground fg <> background bg) (SCU.singleton char)
