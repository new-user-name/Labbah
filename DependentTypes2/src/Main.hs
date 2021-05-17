{-# LANGUAGE GADTs, RankNTypes, DataKinds, TypeOperators, PolyKinds, TypeFamilies, UndecidableInstances, StandaloneKindSignatures #-}
module Main where

--import GHC.TypeLits
import Data.Type.Equality

data Nat = Zero | Succ Nat

type Add :: Nat -> Nat -> Nat
type family Add n m where
  Add Zero m = m
  Add (Succ n) m = Succ (Add n m)

-- это наше произведение, полученное в первой задаче
type Product :: Nat -> Nat -> Nat
type family Product n m where 
    Product Zero m = Zero
    Product m (Succ n) = Add (Product n m) m

data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

data Equality a b where
    R :: Equality a a


leftPlusZeroIsId :: SNat n -> Equality (Add Zero n) n -- в скобках -- прибавляем Zero к n, справа n. "Ноль" плюс n = n
leftPlusZeroIsId _ = R

leftProdZeroisZero :: SNat n -> Equality (Product Zero n) Zero -- в скобках -- умножаем Zero на n, справа "Ноль". "Ноль" * n = "Ноль" 
leftProdZeroisZero _ = R



main :: IO ()
main = do
  putStrLn "hello world"
