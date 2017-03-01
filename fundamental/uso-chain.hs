module Main where
import SAT.CNFIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)


chain :: Int -> [[Int]]
chain 2 = [[2,1]]
chain n = [n-1, n] : chain (n -1)

main = do
  (n:_) <- getArgs
  toFile ("uso-chain-" ++ n ++ ".cnf") $ chain (read n)
  
