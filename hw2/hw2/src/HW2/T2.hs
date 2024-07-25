module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep list = case (l, r) of
  (left, [])       -> left :| []
  (left, _ : rest) -> left :| toList (splitOn sep rest)
  where (l, r) = span (/= sep) list

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldl1 (\a b -> a ++ sep : b)