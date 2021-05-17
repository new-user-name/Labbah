{-# LANGUAGE GADTs, DataKinds, TypeOperators, PolyKinds, TypeFamilies, UndecidableInstances, StandaloneKindSignatures #-}
module Main where

import GHC.TypeLits
import Data.Type.Equality

-- Это определение из руководства
data N = Z | S N
one = S Z
two = S one

-- Это тоже определение из руководства, пользуемся им
-- Семейство делаем закрытым, чтобы извне нельзя было добавить ещё кейсы

type Add :: N -> N -> N
type family Add n m where
  Add Z m = m
  Add (S n) m = S (Add n m)

-- Это наше решение
type Product :: N -> N -> N --оба перемножаемые числа -- натуральные, результат -- натуральное число
type family Product n m where 
    Product Z m = Z -- умножение любого натурального числа на ноль даёт ноль
    Product m (S n) = Add (Product n m) m -- 𝑥 · 𝑠𝑢𝑐𝑐(𝑦) = 𝑥 · 𝑦 + 𝑥




type family To a where
  To 0 = Z
  To n = S (To (n - 1))

type family From a where
  From Z = 0
  From (S n) = 1 + From n

main :: IO ()
main = do
  print "OK"
