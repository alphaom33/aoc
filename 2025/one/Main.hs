import Debug.Trace (trace)
import Control.Monad (when)
import Data.Set (fromList, elems)

tracer a = trace (show a) a

shootMe ugh (-1) yep = shootMe ugh 99 yep
shootMe ugh 100 yep = shootMe ugh 0 yep

shootMe 0 num yep = (num, yep)

shootMe ugh num yep = shootMe (ugh - signum ugh) (num + signum ugh) (yep + (if num == 0 then 1 else 0))

go :: [String] -> Int -> Int -> Int
go (l:ls) num count = trace (show (num', count')) $ go ls num' count'
    where
        add = read (drop 1 l) * (if head l == 'R' then 1 else -1)
        (num', count') = shootMe add num count

go [] num count = count + (if num == 0 then 1 else 0)

main = do
    input <- readFile "input"
    let rots = lines input
    print $ go rots 50 0

