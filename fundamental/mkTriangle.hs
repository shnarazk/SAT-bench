module Main where
import SAT.DIMACS
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

chain :: Int -> [[Int]]
chain 1 = [[1]]
chain n = ([-(n-1) .. -1] ++ [n]) : chain (n -1)

main = do
  (n:_) <- getArgs
  toFile ("triangle-" ++ n ++ ".cnf") $ chain (read n)
  
