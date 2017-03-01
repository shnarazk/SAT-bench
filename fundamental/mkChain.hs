module Main where
import SAT.CNFIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

chain :: Int -> [[Int]]
chain 2 = [[2,-1], [1]]
chain n = [n-1, -n] : chain (n -1)

chainK :: Int -> Int -> [[Int]]
chainK k n = [take (k - 1) [-x + 1 ..] ++ [x] | x <- [k .. n]] ++ [[x] | x <- [1 .. k - 1]]

main = do
  (k:n:_) <- getArgs
  toFile ("chain-" ++ k ++ "-" ++ n ++ ".cnf") $ chainK (read k)(read n)
  
