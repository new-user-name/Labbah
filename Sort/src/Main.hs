module Main where

import Data.Map
import Data.Char

data Token = Number Float  | Open | Close | Pwr | Mult | Div | Rem | Add | Sub deriving (Eq, Show)

operation:: Token -> Bool
operation Add = True
operation Mult = True
operation Div = True
operation Sub = True
operation Pwr = True
operation Rem = True
operation _ = False


opmap:: Map Char Token
opmap = fromList [ ('^', Pwr), ('(', Open), (')', Close), ('*', Mult), ('/', Div), ('%', Rem), ('+', Add), ('-', Sub)]

chompNumber:: String -> (Float, String)
chompNumber xs = (num, str) where 
        (n, str) = span (\x -> isNumber x || x == '.') xs
        num = read n :: Float


toTokens:: String -> [Maybe Token]
toTokens s = loop (Prelude.filter (/= ' ') s) [] where
    loop str tokens
        | Prelude.null str = tokens
        | isNumber $ head str = let
                    (num, str') = chompNumber str
                    tokens' = tokens ++ [Just (Number num)]
                in loop str' tokens'
        | otherwise = loop (tail str) (tokens ++ [Data.Map.lookup (head str) opmap])


precedence :: Token -> Int
precedence t = case t of (Number _) -> 4; Open -> 3; Close -> 3; Pwr -> 2; Mult -> 1; Div -> 1; Rem -> 1; Add -> 0; Sub -> 0 


morph:: [Maybe Token] -> [Token]
morph ts = morph' ts [] [] where
-- No tokens
    morph' [] [] q = q
    morph' [] s q =
        if head s == Open 
            then error "error at ()" 
            else morph' [] (tail s) (q ++ [head s])
    morph' (x:xs) s q = case x of
        (Just (Number n)) -> morph' xs s (q ++ [Number n])
        (Just Close) -> morph' xs s0 q0 where
            q0 = q ++ takeWhile (/= Open) s
            s0 = tail $ dropWhile (/= Open) s
        (Just Open) -> morph' xs (Open:s) q
        (Just o1) -> morph' xs s1 q1 where
            cnd o2 = operation o2 && (precedence o1 < precedence o2)
            spl = span cnd s
            q1 = q ++ fst spl
            s1 = o1 : snd spl
        Nothing -> error "error at tokens"


makeString:: [Token] -> String
makeString = concatMap makeString0 where
    makeString0 Open = "("
    makeString0 Close = ")"
    makeString0 Pwr = "^"
    makeString0 Mult = "*"
    makeString0 Div = "/"
    makeString0 Rem = "%"
    makeString0 Add = "+"
    makeString0 Sub = "-"
    makeString0 (Number n) = show n

evaluateTokens :: [Token] -> Float
evaluateTokens = head . Prelude.foldl folder []
  where folder values (Number i) = i:values
        folder (x:y:ys) Add = (x + y):ys
        folder (x:y:ys) Sub = (y - x):ys
        folder (x:y:ys) Mult = (x * y):ys
        folder (x:y:ys) Div = (y / x):ys
        -- folder (x:y:ys) Rem = (y `rem` x):ys
        folder (x:y:ys) Pwr = (x ** y):ys

toPoland :: String -> String
toPoland = makeString . morph . toTokens

evaluator :: String -> Float
evaluator = evaluateTokens . morph . toTokens

main :: IO ()
main = do
    print $ evaluator "(1+2)" -- 3
    print $ evaluator "10 * 2 ^ (3 - 1) * 3.5" -- 140
  -- не обрабатывает деление на 0 и %
  
  -- putStrLn $ toPoland "(1+2)" -- 12+
  -- putStrLn $ toPoland "(1+2*3)" -- 123*+
  -- putStrLn $ toPoland "(1 +2%3)" -- 123%+, space just removed
  -- print $ toPoland' "10*2^(3-1)*3.5"
