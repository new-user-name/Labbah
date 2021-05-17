module Main where

data Dual a = D a a deriving Show

real, dual :: Dual a -> a
real (D a _) = a
dual (D _ b) = b

instance Eq a => Eq (Dual a) where -- два дуальные числа равны, когда равны действительная и дуальная части
    (D a b) == (D c d) = (a == c) && (b == d)

instance Num a => Num (Dual a) where
    (D a b) + (D c d) = D (a + c) (b + d) -- сумма дуальных чисел
    (D a b) * (D c d) = D (a * c) (b * c + a * d) -- произведение дуальных чисел
    negate (D a b) = D (-a) (-b)
    fromInteger n = D (fromIntegral n) 0
    signum (D a b) = undefined -- не ясно, как выводить знак дуального числа
    abs (D a b) = undefined -- комплексные числа в этом месте дают функцию C -> R, а думальные должны давать
                            -- D->D, то есть абсолютное значение дуального числа тоже дуальное число. Непонятно.

instance Fractional a => Fractional (Dual a) where
     -- берём определение частного отсюда
    -- https://en.wikipedia.org/wiki/Automatic_differentiation#Automatic_differentiation_using_dual_numbers
    (D a b) / (D c d) = D (real x / (c*c)) (dual x / (c*c))
        where
        x = D a b * D c (-d)
    fromRational r = D (fromRational r) 0

instance Floating a => Floating (Dual a) where
    -- берём функции отсюда
    -- https://en.wikipedia.org/wiki/Automatic_differentiation#Automatic_differentiation_using_dual_numbers
    pi = D pi (2*pi)
    log (D a b) = D (log a) (b / a)
    sin (D a b) = D (sin a) (b* cos a)
    cos (D a b) = D (cos a) (b*(- sin a))
    sinh (D a b) = D (sinh a) (b* cosh a)
    cosh (D a b) = D (cosh a) (b* sinh a)
    exp (D a b) = D (exp a) (b * exp a)
    
-- https://1cov-edu.ru/mat_analiz/funktsii/giperbolicheskie_obratnie/
-- обратный гиперболический котангенс
arcth :: Floating a => a -> a
arcth x = (1/2) * log ((x + 1) / (x - 1))

-- его производная
arcth' :: Floating a => a -> a
arcth' x = - 1 / (x*x - 1)

arcthD :: Floating a => Dual a -> Dual a -- обратный гиперболический котангенс для дуального аргумента   
arcthD (D a b) = D (arcth a) (b * arcth' a)

f1, f1', f2, f2' :: Floating a => a -> a

 -- функция из задания
f1 x = sin (2 * exp (x*x))
-- её аналитическая производная
-- https://www.wolframalpha.com/input/?i=sin%282e%5Ex%5E2%29
f1' x = 4 * x * exp (x * x) * cos(2 * exp (x * x)) 

-- функция из задания
f2 x = x^3 - log (x ** 2) + 14 * cos (x/2) + (arcth x) ** 2
-- её аналитическая производная
-- https://www.wolframalpha.com/input/?i=x%5E3+-+ln+%28x%5E2%29+%2B+14*cos%28x%2F2%29+%2B+%28arcth+x%29%5E2
f2' x = 3*x*x - (2*arcth x )/(x*x - 1) - 2/x - 7 * sin(x/2)

automaticDerivative :: Num a => (Dual a -> Dual b) -> a -> b
automaticDerivative f x = dual $ f (D x 1) 

main :: IO ()
main = do
  --print $ sin (D 1 2) -- Main> D 0.8414709848078965 1.0806046117362795
  --print $ automaticDerivative sin pi -- -1.0; sin' x = cos x, cos pi = -1
  --print $ automaticDerivative f1 pi 
  
  --print $ automaticDerivative f1 pi --192560.63543778454
  --print $ f1' pi -- 192560.63543778454

  --print $ automaticDerivative f2 pi --21.897834913869858
  print $ f2' pi -- 21.897834913869858


  




