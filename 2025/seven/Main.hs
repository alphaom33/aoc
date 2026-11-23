import Data.Set (fromList, elems)
import Data.List (sort, sortBy, nub)
import Data.Bifunctor (Bifunctor(second))
import Debug.Trace (trace)

tracer a = trace (show a) a

check x ('S':_) = x
check x (_:cs) = check (x + 1) cs

runAlls lines coords y out = if y == length lines
    then coords
    else runAlls lines coords' (y + 1) (out + plus)
    where (plus, coords') = runIt lines coords y

nubbins ((mult1, x1):(mult2, x2):xes) = if x1 == x2
    then nubbins ((mult1 + mult2, x1):xes)
    else (mult1, x1) : nubbins ((mult2, x2):xes)

nubbins xes = xes

runIt lines coords y = second (nubbins . sort) $ dumbness coords [] 0
    where
        dumbness (x:xes) returner out = 
            let (splits, nexes) = goS lines x y
            in dumbness xes (nexes ++ returner) (out + splits)

        dumbness [] returner out = (out, returner)


goS lines (mult, x) y
    | x < 0 || x == length (head lines) = (0, [])
    | char == '^' = (1, [(mult, x - 1), (mult, x + 1)])
    | otherwise = (0, [(mult, x)])
    where char = lines !! y !! x

main = do
    input <- readFile "data"
    let
        lined = lines input
        x = check 0 (head lined)
    print $ sum $ map fst $ runAlls lined [(1, x)] 0 0
