module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Control.Monad (ap)

newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES $ \x -> 
    case g x of 
      Error e -> Error e
      Success (a :# b) -> Success $ f a :# b

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \x -> Success $ a :# x

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \x -> 
    case f x of 
      Error e -> Error e
      Success (a :# b) -> runES a b

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \x -> Success $ () :# f x

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) a f = joinExceptState $ mapExceptState f a

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a)       = pure a
eval (Op(Abs a))   = unEval abs Abs a
eval (Op(Sgn a))   = unEval signum Sgn a
eval (Op(Sub a b)) = binEval (-) Sub a b
eval (Op(Add a b)) = binEval (+) Add a b
eval (Op(Mul a b)) = binEval (*) Mul a b
eval (Op(Div a b)) = do 
    x <- eval a
    isZero <- eval b
    if isZero == 0 
      then throwExceptState DivideByZero 
      else modifyExceptState(Div x isZero :); return (x / isZero)

unEval :: (Double -> Double) -> (Double -> Prim Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
unEval f g a = do x <- eval a; modifyExceptState(g x :); return (f x)

binEval :: (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binEval f g a b = do x <- eval a; y <- eval b; modifyExceptState(g x y :); return (f x y)


