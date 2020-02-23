import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntLine :: Int -> IO [[Int]]
getAsIntLine n = replicateM n getAsInt

getAsIntArray1Dr :: Int -> IO (UArray Int Int)
getAsIntArray1Dr n = U.listArray (1,n) <$> getAsInt

getAsIntArray1Dc :: Int -> IO (UArray Int Int)
getAsIntArray1Dc n = U.listArray (1,n) . concat <$> getAsIntLine n

-- n: number of lines
-- m: number of values per a line
getAsIntArray2D :: Int -> Int -> IO (UArray (Int,Int) Int)
getAsIntArray2D n m = U.listArray ((1,1),(n,m)) . concat <$> getAsIntLine n


main :: IO ()
main = do
  [n,w] <- getAsInt
  wvs <- getAsIntArray2D n 2
  print wvs

solve1 :: UArray (Int,Int) Int -> Int -> Int -> UArray (Int,Int) Int
solve1 wvs n' w' = runSTUArray $ do
  dp <- newArray ((1,1),(n',w')) 0
  -- forM_ [(n_,w_) | n_ <- [1..n'], w_ <- [1..w']] $ \(n,w) -> do
  --   writeArray dp (x,w') 1
  return dp
