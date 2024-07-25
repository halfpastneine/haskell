module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N deriving (Show)



nplus :: N -> N -> N
nplus a Z = a
nplus a (S b) = S(nplus a b)

nmult :: N -> N -> N
nmult a Z = Z
nmult a (S b) = nplus (nmult a b) a

nsub :: N -> N -> Maybe N
nsub a Z = Just a
nsub Z a = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp a Z = GT
ncmp Z b = LT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = (S (nFromNatural (n - 1)))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S b) = (nToNum b) +1 

nEven :: N -> Bool
nEven Z = True
nEven (S (S a)) = nEven a
nEven (S a) = False

nOdd :: N -> Bool
nOdd n
    | nEven n == True = False
    | otherwise = True

ndiv :: N -> N -> N
ndiv a Z = error "Division by Zero"
ndiv a b 
    | ncmp a b == LT = Z
    | otherwise      = (S (ndiv (maybeValue (nsub a b)) b))

maybeValue :: Maybe N -> N 
maybeValue n = case n of
    Just n  -> n
    Nothing -> Z

nmod :: N -> N -> N
nmod a Z = error "Division by Zero"
nmod a b
    | ncmp a b == LT = a
    | ncmp a b == EQ = Z
    | otherwise      = maybeValue(nsub a (nmult (ndiv a b) b))
