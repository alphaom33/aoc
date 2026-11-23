import Data.Char (chr)
import Debug.Trace (trace)

tracer a = trace (show a) a

calcAdd current = maximum $ map (read. pure) current

calcJoltage :: String -> String -> Int -> Int
calcJoltage (l:ls) current count = calcJoltage ls current' (count * 10 + add)
    where
        add = calcAdd current
        current' = drop 1 (dropWhile (/= chr (add + 48)) current) ++ [l]

calcJoltage "" current count = count * 10 + calcAdd current

main = do
    input <- readFile "data"
    let
        lined = lines input
        out = sum $ map (\ a -> calcJoltage (reverse $ take 11 $ reverse a) (reverse $ drop 11 $ reverse a) 0) lined
    print out
