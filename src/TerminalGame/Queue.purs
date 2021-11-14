module TerminalGame.Queue where

import Prelude
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe)

data Queue a
  = Queue (List a)

empty :: forall a. Queue a
empty = Queue Nil

push :: forall a. a-> Queue a -> Queue a
push x (Queue xs) = Queue (Cons x xs)

pop :: forall a. Queue a -> Maybe { last :: a, rest :: Queue a}
pop (Queue xs) = do
  { head, tail } <- L.uncons (L.reverse xs)
  pure  { last: head, rest: Queue (L.reverse tail) }
