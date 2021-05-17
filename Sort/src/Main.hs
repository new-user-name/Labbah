module Main where

import Data.Map
import Data.Char

data Token = Number Int | Open | Close | Add | Sub | Mult | Div  deriving (Eq, Show)

operation:: Token -> Bool
operation Add = True
operation Mult = True
operation Div = True
operation Sub = True
operation _ = False


opmap:: Map Char Token
opmap = fromList [ ('+', Add), ('*', Mult), ('/', Div), ('(', Open), ('-', Sub), (')', Close)]

scanNumber:: String -> (Int, String)
scanNumber xs = (num, str) where 
        (n, str) = span isNumber xs
        num = read n :: Int


toTokens:: String -> [Maybe Token]
toTokens s = loop s [] where
    loop str tokens
        | Prelude.null str = tokens
        | isNumber $ head str = let
                    (num, str') = scanNumber str
                    tokens' = tokens ++ [Just (Number num)]
                in loop str' tokens'
        | otherwise = loop (tail str) (tokens ++ [Data.Map.lookup (head str) opmap])


precedence :: Token -> Int
precedence Open = 2
precedence Close = 2
precedence Mult = 1
precedence Div = 1
precedence Add = 0
precedence Sub = 0
precedence (Number _) = 3


morph:: [Maybe Token] -> [Token]
morph ts = morph' ts [] [] where
-- No tokens
    morph' [] [] q = q
    morph' [] s q =
        if head s == Open 
            then error "error at parentheses" 
            else morph' [] (tail s) (q ++ [head s])
    morph' (x:xs) s q = case x of
        (Just (Number n)) -> morph' xs s (q ++ [Number n])
        (Just Open) -> morph' xs (Open:s) q
        (Just Close) -> morph' xs s0 q0 where
            s0 = tail $ dropWhile (/= Open) s
            q0 = q ++ takeWhile (/= Open) s
        (Just o1) -> morph' xs s1 q1 where
            cond o2 = operation o2 && (precedence o1 < precedence o2)
            spl = span cond s
            s1 = o1 : snd spl
            q1 = q ++ fst spl
        Nothing -> error "error at tokens"

makeString:: [Token] -> String
makeString = concatMap makeString0 where
    makeString0 Open = "("
    makeString0 Close = ")"
    makeString0 Mult = "*"
    makeString0 Div = "/"
    makeString0 (Number n) = show n
    makeString0 Add = "+"
    makeString0 Sub = "-"
    

toPoland:: String -> String
toPoland = makeString . morph . toTokens

main :: IO ()
main = do
  -- enter without spaces  
  putStrLn $ toPoland "(1+2)" -- 12+
  putStrLn $ toPoland "(1+2*3)" -- 123*+
