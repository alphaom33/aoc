import Data.Map (fromList, (!))
import Debug.Trace (trace)

tracer a = trace (show a) a

strToOp = fromList [
    ("*", (*))
    , ("+", (+))
    ]

split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

cross whites = if null $ head whites
    then []
    else map head whites : cross (map (drop 1) whites)

doEq (nums, op) = foldr (strToOp ! op) initial nums
    where
        initial = if op == "*" then 1 else 0

doLine :: [String] -> Int -> Int -> Int -> Int
doLine lines x y count
    | y == length lines - 1 || x >= length (head lines) || char `elem` "+*" = count
    | char == ' ' = if count > 0 then count else doLine lines x (y + 1) count
    | otherwise = doLine lines x (y + 1) $ count * 10 + read [char]
    where char = lines !! y !! x

getAll :: [String] -> Int -> [Int] -> (Int, [Int])
getAll lines x out
    | x >= length (head lines) || liney == 0 = (x, out)
    | otherwise = getAll lines (x + 1) (liney : out)
    where liney = doLine lines x 0 0

yeppers :: [String] -> Int -> [[Int]] -> [[Int]]
yeppers lines x out = if x >= length (head lines)
    then out
    else yeppers lines (x' + 1) (nums : out)
    where (x', nums) = getAll lines x []

main = do
    input <- readFile "data"
    let
        lined = lines input
        operators = split (== ' ') $ reverse $ last lined
        eqqed = yeppers lined 0 []
        opped = zip eqqed operators
    print $ sum $ map doEq opped

