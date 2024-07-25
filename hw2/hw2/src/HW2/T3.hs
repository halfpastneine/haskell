module HW2.T3
  ( epart
  , mcat
  ) where
import Data.Foldable ()

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap isJust

isJust :: Monoid a => Maybe a -> a
isJust (Just x) = x
isJust _        = mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap getEither

getEither :: (Monoid a, Monoid b) => Either a b -> (a, b)
getEither (Left a)  = (a, mempty)
getEither (Right b) = (mempty, b)