module Main where
import SAT.CNFIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)


chain :: Int -> [[Int]]
chain 1 = [[1]]
chain n = [1 .. n] : chain (n -1)

main = do
  (n:_) <- getArgs
  toFile ("zero-" ++ n ++ ".cnf") $ chain (read n)
  
