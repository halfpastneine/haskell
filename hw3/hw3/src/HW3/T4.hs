module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1
import Control.Monad (ap)

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S $ \x -> let (a :# e) = g x in (f a :# e) 

wrapState :: a -> State s a
wrapState a = S $ \x -> a :# x 

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \x -> let (S g :# e) = f x in g e

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \x -> () :# f x

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = ap

instance Monad (State s) where
  (>>=) (S f) g = S $ \x -> let (a :# e) = f x; (S b) = g a in b e

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) a b = Op (Add a b)
  (-) a b = Op (Sub a b)
  (*) a b = Op (Mul a b)
  abs a = Op (Abs a)
  signum a = Op (Sgn a)
  fromInteger a = Val (fromIntegral a)

instance Fractional Expr where
  (/) a b = Op (Div a b)
  fromRational a = Val (fromRational a)

eval :: Expr -> State [Prim Double] Double
eval (Val a)       = pure a
eval (Op(Sub a b)) = binEval (-) Sub a b
eval (Op(Add a b)) = binEval (+) Add a b
eval (Op(Mul a b)) = binEval (*) Mul a b
eval (Op(Div a b)) = binEval (/) Div a b
eval (Op(Abs a))   = unEval abs Abs a
eval (Op(Sgn a))   = unEval signum Sgn a

binEval :: (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> Expr -> Expr -> State [Prim Double] Double
binEval f g a b = do x <- eval a; y <- eval b; modifyState(g x y :); return (f x y)

unEval :: (Double -> Double) -> (Double -> Prim Double) -> Expr -> State [Prim Double] Double
unEval f g a = do x <- eval a; modifyState(g x :); return (f x)