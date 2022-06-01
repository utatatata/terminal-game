module Main where

import Prelude

import Effect (Effect)
import Chess (playChess)
import Panepon (playPanepon)

main :: Effect Unit
main = playPanepon
