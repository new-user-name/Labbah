module Main where

-- наивная реализация производной: приращение
-- функции делим на приращение аргумента,
-- считаем, что функция достаточно гладкая
-- приращение аргумента выбираем как "маленькое число"

diff0 :: (Floating a) => (a -> a) -> a -> a -> a
diff0 f x dx = (f (x + dx) - f x) / dx

newtonIter f f' x 0 = x - f x / f' x
newtonIter f f' x k = let x_i = x - f x / f' x in
    newtonIter f f' x_i (k - 1)

k = 10 -- итерации
dx = 0.01 -- приращение аргумента в численном приближении производной

sinus = sin
sinus' x = diff0 sinus x dx -- хорошо бы point-free, но тогда надо переделывать производную
startSinus = 0.5

polynom x  = x^3 - 328*x**2 - 1999*x - 1670
polynom' x  = diff0 polynom x dx -- повторяющийся код, можно сделать обёртку
startPolynom = -6

main :: IO ()
main = do
  -- print $ diff0 sin 0 0.001 -- наш приближенный результат 0.9999998333333416
  -- print $ cos 0 -- sin'x = cos x, точный результат 1.0

  -- print $ diff0 (^2) 0 0.001 -- наш приближенный результат 1.0e-3
  -- print $ (*2) 0 -- (x^2)' = 2*x, точный реультат 0

  print $ newtonIter sinus sinus' startSinus k
  print $ newtonIter polynom polynom' startPolynom k
  -- замечания по решению: в обоих случаях 100 шагов явно избыточно,
  -- ноль синуса ищется очень быстро, ноль полинома медленнее, но
  -- и 10 достаточно (для точности "на глаз").
  -- У полинома есть и другие корни, см. например, тут:
  -- https://www.wolframalpha.com/input/?i=x%5E3+-+328*x%5E2+-+1999*x+-+1670+%3D+0
  -- при стартовом значении 300 метод быстро сходится к 334,
  -- при -6 быстро сходится к -5.
  -- Количество необходимых шагов можно определять "динамически",
  -- исходя из свойств конкретной функции. См. например, "Structure and Interpretation of Computer
  -- Programs, Second Edition JavaScript Adaptation", стр. 39, хотя метод
  -- описан наверняка и в исходном классическом тексте SICP.

  
