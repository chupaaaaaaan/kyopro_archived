import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntArray1Dr :: Int -> IO (UArray Int Int)
getAsIntArray1Dr n = U.listArray (1,n) <$> getAsInt

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- getAsIntArray1Dr n
  let solve = solve2
  print $ solve hs n U.! n

-- 貰うDP
solve1 :: UArray Int Int -> Int -> UArray Int Int
solve1 hs n = runSTUArray $ do
  dp <- newArray (1,n) 0
  forM_ [2..n] $ \x ->
    if x == 2
    then writeArray dp 2 . abs $ (hs U.! 2) - (hs U.! 1)
    else do
      a <- liftM2 (+) (readArray dp (x-1)) (return . abs $ hs U.! x - hs U.! (x-1))
      b <- liftM2 (+) (readArray dp (x-2)) (return . abs $ hs U.! x - hs U.! (x-2))
      writeArray dp x $ min a b
  return dp

-- 配るDP
solve2 :: UArray Int Int -> Int -> UArray Int Int
solve2 hs n = runSTUArray $ do
  dp <- newArray (1,n) maxBound
  writeArray dp 1 0
  forM_ [1..(n-1)] $ \x ->
    if x == n-1
    then do
      a <- liftM2 (+) (readArray dp x) (return . abs $ hs U.! x - hs U.! (x+1))
      readArray dp n >>= writeArray dp n . min a
    else do
      a <- liftM2 (+) (readArray dp x) (return . abs $ hs U.! x - hs U.! (x+1))
      b <- liftM2 (+) (readArray dp x) (return . abs $ hs U.! x - hs U.! (x+2))
      readArray dp (x+1) >>= writeArray dp (x+1) . min a
      readArray dp (x+2) >>= writeArray dp (x+2) . min b
  return dp
