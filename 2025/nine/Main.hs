{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (sortBy, maximumBy, partition, sort, minimumBy)
import Debug.Trace (trace)
import Data.Function (on)

tracer a = trace (show a) a

split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

doNonsense (c:cs) thing = map (c,) thing : doNonsense cs (drop 1 thing)
doNonsense [] _ = []

distit (p1, p2) = (abs (head p1 - head p2) + 1) * (abs (last p1 - last p2) + 1)

insiderx (([x1, y1], [x2, y2]):ls) (_:lines) = insiderx ls (ahhhhhh ([x1, higher y1], [x1, lower y2]) lines)
    where
        doCheck y = checkInside lines (x1 - 1, y)

        higher y = if doCheck (y - 1) then y + 1 else y
        lower y = if doCheck (y + 1) then y - 1 else y

        checkInside lines (x, y) = odd $ length $ filter (checkPointIntersection (x, y)) lines
        checkPointIntersection (checkx, checky) ([px1, py1], [px2, py2]) = px1 <= checkx && py1 <= checky && checky <= py2
insiderx _ lines = lines

insidery (([x1, y1], [x2, y2]):ls) (_:lines) = insidery ls (ahhhhhh ([left x1, y1], [right x2, y1]) lines)
    where
        doCheck x = checkInside lines (x, y1 + 1)

        left x = if doCheck (x - 1) then x + 1 else x
        right x = if doCheck (x + 1) then x - 1 else x

        checkInside lines (x, y) = odd $ length $ filter (checkPointIntersection (x, y)) lines
        checkPointIntersection (checkx, checky) ([px1, py1], [px2, py2]) = py1 <= checky && px1 <= checkx && checkx <= px2
insidery _ lines = lines


lineit :: [[Int]] -> [([Int], [Int])]
lineit (p1:p2:ps) = (p1', p2') : lineit (p2:ps)
    where [p1', p2'] = sortBy (\ a b -> compare (sum p1) (sum p2)) [p1, p2]
lineit _ = []

checkIntersections (checkx1, checkx2, checky1, checky2) ([px1, px2], [py1, py2]) = (checkx2 > px1 && px2 > checkx1) && (checky2 > py1 || py2 > checky2)

nintersect lines ([x1, y1], [x2, y2]) = not $ any (checkIntersections (x, x + diffx, y, y + diffy)) lines
    where 
        x = min x1 x2
        y = min y1 y2
        diffx = abs $ x1 - x2
        diffy = abs $ y1 - y2

ahhhhhh a b = reverse $ a : reverse b

main = do
    input <- readFile "data"
    let
        points :: [[Int]]
        points = map (map read . split (== ',')) $ lines input
        nonsensed = doNonsense points (drop 1 points)

        lined = lineit (last points : points)

        (vertical, horizontal) = partition (\ (a, b) -> head a == head b) lined
        sortedx = sortBy (\ (a,_) (b,_) -> compare (head a) (head b)) vertical
        sortedy = sortBy (\ (a,_) (b,_) -> compare (last a) (last b)) horizontal
        insidedx = insiderx sortedx sortedx
        insidedy = insidery sortedy sortedy

        filtered = map (filter (nintersect (insidedx ++ insidedy))) nonsensed
        folded = concat filtered

    print $ nintersect insidedy ([7, 1], [7, 7])
    -- print $ sortBy (\ a b -> compare (distit b) (distit a)) folded


-- because it's clockwise, if checking from left, line should be pointing up
-- if you cast from inside, you'll hit an odd number of edges, from outside it will be even

-- count edges. Odd means in, even means out
