import Data.List (permutations)
import Data.Bifunctor (Bifunctor(bimap))
import Debug.Trace (trace)

tracer a = trace (show a) a

dropEnd l = reverse $ drop 1 $ reverse l

checkRolls matrix x y = sum $ map check $ filter (\ (x, y) -> x >= 0 && x < length (head matrix) && y >= 0 && y < length matrix) $  map (bimap (x +) (y +)) ughs
    where
        check (x, y) = if matrix !! y !! x == '@' then 1 else 0
        ughs = [
            (1, 0)
            , (1, 1)
            , (0, 1)
            , (-1, 1)
            , (-1, 0)
            , (-1, -1)
            , (0, -1)
            , (1, -1)
            ]

killFriend (x, y) mat = let
    (bef, aft) = splitAt (y + 1) mat
    (befo, afte) = splitAt (x + 1) (last bef)
    in
        dropEnd bef ++ (dropEnd befo ++ '.' : afte) : aft

doIt matrix x y count
    | x == length (head matrix) = doIt matrix (x - length matrix) (y + 1) count
    | y == length matrix = count
    | matrix !! y !! x == '.' = doIt matrix (x + 1) y count
    | otherwise = doIt matrix (x + 1) y (if checkRolls matrix x y < 4 then (x, y) : count else count)

goForIt matrix count
    | null done = count
    | otherwise = goForIt (foldr killFriend matrix done) (count + length done)
    where done = doIt matrix 0 0 []

main = do
    input <- readFile "data"
    print $ goForIt (lines input) 0

