module Test.TerminalGame.Types
  ( testProjectOnWindow
  ) where

import Prelude
import Ansi.Codes (Color(..))
import Ansi.Output (background, foreground, withGraphics)
import Data.Map as M
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TerminalGame.Types (Canvas, projectOnWindow, vec3)

testProjectOnWindow :: Spec Unit
testProjectOnWindow =
  describe "TerminalGame.Types" do
    describe "projectOnWindow" do
      it "overwrite" do
        projectOnWindow { width: 5, height: 2 }
          (M.fromFoldable
            [ Tuple (vec3 0 0 0) { fg: White, bg: Black, char: 'a' }
            , Tuple (vec3 0 0 (-1)) { fg: White, bg: Black, char: 'b' }
            ])
          `shouldEqual` [ toStr White Black 'b' <> "    ", "     " ]
      it "out of screen" do
        projectOnWindow { width: 5, height: 2 } (M.fromFoldable [ Tuple (vec3 5 0 0) { fg: White, bg: Black, char: 'a' } ])
          `shouldEqual` [ "     ", "     " ]
        projectOnWindow { width: 5, height: 2 } (M.fromFoldable [ Tuple (vec3 0 2 0) { fg: White, bg: Black, char: 'a' } ])
          `shouldEqual` [ "     ", "     " ]
      it "change color" do
        projectOnWindow { width: 3, height: 3 }
          (M.fromFoldable
            [ Tuple (vec3 0 0 0) { fg: Red, bg: Black, char: 'r' }
            , Tuple (vec3 1 1 0) { fg: Blue, bg: Black, char: 'b' }
            , Tuple (vec3 2 2 0) { fg: Yellow, bg: Black, char: 'y' }
            ])
            `shouldEqual` [ toStr Red Black 'r' <> "  ", " " <> toStr Blue Black 'b' <> " ", "  " <> toStr Yellow Black 'y' ]
  where
  toStr :: Color -> Color -> Char -> String
  toStr fg bg c = withGraphics (foreground fg <> background bg) (SCU.singleton c)
