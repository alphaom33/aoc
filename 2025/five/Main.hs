import Data.Set (fromList, union)
import Debug.Trace (trace)
import Data.List (sort, sortBy)
import Data.Bifunctor (Bifunctor(second))

tracer a = trace (show a) a

ugh ("":ls) mhm = (mhm, ls)
ugh (l:ls) mhm = ugh ls (l : mhm)

checkId (range:ranges) id = if fst range <= id && id <= snd range
    then 1
    else checkId ranges id

checkId [] _ = 0

checkIds ranges (id:ids) count = checkIds ranges ids (count + checkId ranges id)
checkIds _ [] count = count

getRangeNums (range:ranges) (out:outs) = getRangeNums ranges $ if fst range <= snd out 
    then second (max (snd range)) out : outs
    else range : out : outs

getRangeNums [] outs = outs

main = do
    input <- readFile "data"
    let
        (ranges, ids) = ugh (lines input) []

        ranged :: [(Int, Int)]
        ranged = sort $ map (\ x -> (read $ takeWhile (/= '-') x, read $ drop 1 $ dropWhile (/= '-') x)) ranges
        rangeNums = getRangeNums (tail ranged) (take 1 ranged)

    print $ sum (map (uncurry (-)) rangeNums) - length rangeNums
