import           Control.Monad
import           Data.Array.IArray

main :: IO ()
main = do
  n <- readLn :: IO Int
  abcs <- listArray (1,n) <$> replicateM n ((\[x,y,z] -> (x,y,z)) . map read . words <$> getLine) :: IO (Array Int (Int,Int,Int))
  print abcs
