module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TerminalGame.Types (testProjectOnWindow)
import Test.Chess (testCursorToInt, testGetMovablePoss, testGetPiece, testJudgeCheck)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
         testProjectOnWindow
         testGetPiece
         testGetMovablePoss
         testJudgeCheck
         testCursorToInt
