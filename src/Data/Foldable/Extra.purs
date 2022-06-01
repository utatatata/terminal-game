module Data.Foldable.Extra where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe(..))

forWhileJust_ :: forall a b f m. Monad m => Foldable f => Monoid b => f a -> (a -> m (Maybe b)) -> m Unit
forWhileJust_ xs f = void $ foldr go (pure (Just mempty)) xs
  where
  go a = (=<<) (const $ f a)

forUntilJust_ :: forall a b f m. Monad m => Foldable f => f a -> (a -> m (Maybe b)) -> m Unit
forUntilJust_ xs f = void $ foldr go (pure Nothing) xs
  where
  go a = (=<<) case _ of
    Just x -> pure (Just x)
    Nothing -> f a *> pure Nothing
