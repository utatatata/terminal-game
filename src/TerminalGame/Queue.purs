module TerminalGame.Queue where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)

newtype Queue a
  = Queue (List a)

empty :: forall a. Queue a
empty = Queue Nil

enqueue :: forall a. a-> Queue a -> Queue a
enqueue x (Queue xs) = Queue (Cons x xs)

dequeue :: forall a. Queue a -> Maybe { last :: a, rest :: Queue a}
dequeue (Queue xs) = do
  { head, tail } <- L.uncons (L.reverse xs)
  pure  { last: head, rest: Queue (L.reverse tail) }

toList :: Queue ~> List
toList (Queue xs) = L.reverse xs

derive instance newtypeQueue :: Newtype (Queue a) _

derive newtype instance showQueue :: Show a => Show (Queue a)

derive newtype instance eqQueue :: Eq a => Eq (Queue a)

derive newtype instance eq1Queue :: Eq1 Queue

derive newtype instance ordQueue :: Ord a => Ord (Queue a)

derive newtype instance ord1Queue :: Ord1 Queue

derive newtype instance semigroupQueue :: Semigroup (Queue a)

derive newtype instance monoidQueue :: Monoid (Queue a)

derive newtype instance functorQueue :: Functor Queue

derive newtype instance functorWithIndexQueue :: FunctorWithIndex Int Queue

derive newtype instance foldableQueue :: Foldable Queue

derive newtype instance foldableWithIndexQueue :: FoldableWithIndex Int Queue

derive newtype instance unfoldable1Queue :: Unfoldable1 Queue

derive newtype instance unfoldableQueue :: Unfoldable Queue

derive newtype instance traversableQueue :: Traversable Queue

derive newtype instance traversableWithIndexQueue :: TraversableWithIndex Int Queue

derive newtype instance applyQueue :: Apply Queue

derive newtype instance applicativeQueue :: Applicative Queue

derive newtype instance bindQueue :: Bind Queue

derive newtype instance monadQueue :: Monad Queue

derive newtype instance altQueue :: Alt Queue

derive newtype instance plusQueue :: Plus Queue

derive newtype instance alternativeQueue :: Alternative Queue

derive newtype instance monadZeroQueue :: MonadZero Queue

derive newtype instance monadPlusQueue :: MonadPlus Queue

derive newtype instance extendQueue :: Extend Queue
