module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec y -> case y of
                [] -> []
                _ -> f (head y) : rec (tail y))
                                
fib :: Natural -> Natural
fib = fix (\rec (a, b) n -> if n == 0 then a else rec (b, a + b) (n - 1)) (0, 1)

fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))  
  