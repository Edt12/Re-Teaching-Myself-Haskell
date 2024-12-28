import Data.List (permutations,nub)

square :: (Num a ) => a -> a
square x = x* x

factors :: Int -> [(Int,Int)]
factors target = deletePermutations [(x,y) | x <- [0..target],y <- [0..target], x * y == target]
{-
turns permuations into the same as puts min elem at the start and max at the end of pair
-}
normaliseTuple :: Ord a => (a,a) -> (a,a)
normaliseTuple (x,y) = (min x y , max x y)
{-
first sort the list so all permutations are same then use nub to delete duplicates
-}
deletePermutations :: [(Int,Int)] -> [(Int,Int)]
deletePermutations tupleList = nub (map normaliseTuple tupleList)

main :: IO ()
main = putStrLn "hello world"   