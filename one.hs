import System.IO
import Control.Monad
import GHC.Base (VecElem(Int16ElemRep))
main :: IO ()
main = do
    contents <- readFile "one.in"
    let nums = map toInt . words $ contents
    (print . one) nums
    (print . two) nums

toInt (x:xs) = sign x * read xs
    where
        sign :: Char -> Int
        sign a = if a == 'L' then -1 else 1
toInt _ = error "Bad Input"

one :: [Int] -> Int
one = aux 0 50
    where
        aux :: Int -> Int -> [Int] -> Int
        aux sum pos [] = sum
        aux sum pos (x:xs) = aux newSum newPos xs
            where
                newSum = sum + (if mod (pos + x) 100 == 0 then 1 else 0)
                newPos = mod (mod (pos + x) 100 + 100) 100

two :: [Int] -> Int
two = aux 0 50
    where
        aux :: Int -> Int -> [Int] -> Int
        aux sum pos [] = sum
        aux sum pos (x:xs) = aux newSum newPos xs
            where
                sumOne = sum + (if mod (pos + x) 100 == 0 then (if pos == 0 then 0 else 1) else 0)
                sumTwo = sumOne + (if x < 0 && mod (mod (pos + x) 100 + 100) 100 > pos && pos /= 0 then 1 else 0)
                sumThree = sumTwo + (if x > 0 && mod (mod (pos + x) 100 + 100) 100 < pos && mod (mod (pos + x) 100 + 100) 100 /= 0 then 1 else 0)
                newSum = sumThree + div (abs x) 100
                newPos = mod (mod (pos + x) 100 + 100) 100