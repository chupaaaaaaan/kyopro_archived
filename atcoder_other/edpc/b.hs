import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Int]
  hs <- listArray (1,n) . map read . words <$> getLine :: IO (UArray Int Int)
  let solve = solve2
  print $ solve hs n k ! n

-- 貰うDP
solve1 :: UArray Int Int -> Int -> Int -> UArray Int Int
solve1 hs n k = runSTUArray $ do
  dp <- newArray (1,n) 0
  forM_ [1..n] $ \x ->
    minimum <$> forM [(max (x-k) 1)..(x-1)] (\y -> liftM2 (+) (readArray dp y) (return . abs $ hs!x - hs!y)) >>=  writeArray dp x
  return dp

-- 配るDP
solve2 :: UArray Int Int -> Int -> Int -> UArray Int Int
solve2 hs n k = runSTUArray $ do
  dp <- newArray (1,n) maxBound
  writeArray dp 1 0
  forM_ [1..n] $ \x ->
    forM [(x+1)..(min (x+k) n)] $ \y -> liftM2 (+) (readArray dp x) (return . abs $ hs!x - hs!y) >>= \a -> readArray dp y >>= writeArray dp y . min a
  return dp
