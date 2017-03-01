-- solving sudoku for SAT solver
-- http://apollon.issp.u-tokyo.ac.jp/~watanabe/sample/sudoku/index_j.html
import Data.List

offset = 0

problem = 
  [2,0,0,0,0,7,0,5,0
  ,0,0,0,0,0,0,3,0,0
  ,0,0,1,2,0,8,0,0,0
  ,7,0,0,0,0,3,0,0,1
  ,0,0,2,0,0,0,0,6,0
  ,0,6,0,0,0,1,4,0,0
  ,0,0,4,1,0,6,0,0,8
  ,0,0,8,0,0,0,7,0,0
  ,0,5,0,0,0,0,0,9,0]

index (_, _, 0) = "0\n"
index (x, y, n) 
  | n > 0 = (show $ (y-1) * 9 * 9 + (x-1) * 9 + (n-1) + 1 + offset) ++ " "
  | n < 0 = "-" ++ (show $ (y-1) * 9 * 9 + (x-1) * 9 + (-n-1) + 1 + offset) ++ " "

allGrid = [(x,y) | x <- [1..9], y <- [1..9] ]

main = do 
  let a = makeProblem problem ++ g ++ concatMap (\(x,y) -> concat $ hrzn(x,y) ++ vtcl(x,y) ++ box(x,y)) allGrid
  putStrLn $ "p cnf " ++ (show $ 9*9*9) ++ " " ++ (show $ cc a)
  putStr $ concatMap index $ a

-- make problem restrict
makeProblem l = mp 1 1 l
 where 
   next x y l 
     | x == 9 = mp 1 (y+1) l
     | otherwise = mp (x+1) y l
   mp x y (n:l) 
     | n>0 = (x, y, n):((0,0,0):(next x y l))
     | otherwise = next x y l
   mp _ _ _ = []

-- input number to all grids
g = concatMap g' allGrid
 where 
   g' (x, y) = g'' 9
     where
       g'' 0 = [(x, y, 0)]
       g'' n = (x, y, n):(g'' $ n-1)

-- restrict number for sudoku rule
hrzn (x, y) = [[(x, y, -n), (x, y', -n), (0,0,0)] | n <- [1..9], y' <- [1..9]\\[y]]
vtcl (x, y) = [[(x, y, -n), (x', y, -n), (0,0,0)] | n <- [1..9], x' <- [1..9]\\[x]]
box (x, y) = [[(x, y, -n), (x', y', -n), (0,0,0)] | n <- [1..9], x' <- [(boxS x)..(boxS x + 2)], y' <- [(boxS y)..(boxS y + 2)], (x/=x' || y/=y')]
 where boxS n = n - ((n-1) `mod` 3)

-- count clause
cc ((_, _, 0):l) = 1 + (cc l)
cc (_:l) = cc l
cc _ = 0
