import GHC.Float (powerFloat)
import Debug.Trace (trace)
import Data.List (sortBy, intersect, sort, nub)
import Data.Ord (comparing, Down (Down))

tracer a = trace (show a) a

split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

dst :: [Float] -> [Float] -> Float
dst p1 p2 = sqrt $ sum $ map pow $ zipWith (-) p1 p2
    where pow x = x * x

doNonsense (c:cs) thing = map (c,) thing ++ doNonsense cs (drop 1 thing)
doNonsense [] _ = []

coolAny p left (o:os) = if p o
    then Just (reverse left, o, os)
    else coolAny p (o:left) os

coolAny _ _ [] = Nothing

buildGraph c out = case coolAny (elem $ fst c) [] out of
    Nothing -> case coolAny (elem $ snd c) [] out of
        Nothing -> [fst c, snd c] : out
        Just (pre, el, post) -> pre ++ (nub $ fst c : el) : post
    Just (pre1, el1, post1) -> case coolAny (elem $ snd c) [] out of
        Nothing -> pre1 ++ (nub $ snd c : el1) : post1
        Just (pre2, el2, post2) -> if el1 == el2
            then out
            else (nub $ fst c : snd c : el1 ++ el2) : ((pre1 ++ post1) `intersect` (pre2 ++ post2))

connectAll (c:cs) out = if length (head moreGraphed) == 1000
    then c
    else connectAll cs moreGraphed
    where moreGraphed = buildGraph c out

main = do
    input <- readFile "data"
    let
        coords = map (map read . split (== ',')) $ lines input
        compared = doNonsense coords $ drop 1 coords
        sorted = sortBy (\ a b -> compare (uncurry dst a) (uncurry dst b)) compared

        -- graphed = buildGraph (take 1000 sorted) []
        -- numbered = take 3 $ sortBy (comparing Down) (map (length . nub) graphed)

    print $ connectAll (tail sorted) (buildGraph (head sorted) [])
