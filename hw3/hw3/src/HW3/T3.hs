module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None     = None
joinOption (Some a) = a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)   = Error e
joinExcept (Success a) = a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# b) :# c) = a :# (c <> b)

joinList :: List (List a) -> List a
joinList Nil               = Nil
joinList (Nil :. Nil)      = Nil
joinList (Nil :. (c :. d)) = joinList (c :. d)
joinList ((a :. b) :. c)   = a :. joinList (b :. c)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> let (F f1) = f x in f1 x)