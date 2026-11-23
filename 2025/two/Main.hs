import Debug.Trace (trace)

split p s = case dropWhile p s of
                "" -> []
                s' -> w : split p s''
                    where (w, s'') = break p s'

checkReps n = any (subbit n) [1..(length n `div` 2)]
    where
        subbit n len = checkSub (drop len n) len (take len n)
        checkSub "" _ _ = True
        checkSub n len against = (take len n == against) && checkSub (drop len n) len against

checkRange l h n count
    | n > h = count
    | otherwise = checkRange l h (n + 1) $ if checkReps (show n) then n : count else count

main = do
    input <- readFile "data"
    let
        ranges = split (== ',') $ reverse $ drop 1 $ reverse input
        ranged :: [[Int]]
        ranged = map (map read . split (== '-')) ranges
        checked = foldr (\ [l, h] count -> count + sum (checkRange l h l [])) 0 ranged
    print $ checked
