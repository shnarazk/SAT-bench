-- solving sudoku for SAT solver
import Data.List

radix = 16
boxSize = 4

offset = 0

problem =
  [ 2 ,  0 ,  5 ,  0 ,  3 ,  0 , 10 ,  0 ,  0 ,  4 ,  0 , 14 ,  0 , 11 ,  0 ,  8 ,
    0 ,  3 ,  0 ,  0 ,  2 ,  0 , 14 ,  0 ,  0 , 11 ,  0 , 15 ,  0 ,  0 ,  1 ,  0 ,
    11 ,  0 ,  0 , 14 ,  9 ,  8 ,  0 ,  6 ,  1 ,  0 ,  2 , 13 , 12 ,  0 ,  0 ,  4 ,
    0 ,  6 ,  0 ,  0 ,  0 , 11 ,  0 ,  0 ,  0 ,  0 ,  3 ,  0 ,  0 ,  0 , 14 ,  0 ,
    0 , 12 ,  6 ,  5 ,  0 ,  3 ,  1 ,  0 ,  0 , 16 , 13 ,  0 , 14 ,  4 , 15 ,  0 ,
    0 ,  0 ,  0 ,  0 , 12 ,  0 ,  0 ,  5 , 15 ,  0 ,  0 ,  4 ,  0 ,  0 ,  0 ,  0 ,
    16 , 15 ,  0 ,  0 ,  8 ,  6 ,  4 ,  0 ,  0 ,  9 , 11 ,  1 ,  0 ,  0 , 13 ,  3 ,
    0 ,  1 , 14 ,  0 ,  0 , 16 ,  0 ,  0 ,  0 ,  0 ,  5 ,  0 ,  0 ,  9 , 12 ,  0 ,
    0 ,  9 ,  0 , 16 ,  0 ,  2 ,  0 , 11 , 10 ,  0 ,  1 ,  0 ,  4 ,  0 ,  5 ,  0 ,
    6 ,  0 ,  0 ,  0 ,  1 ,  0 ,  0 , 16 ,  5 ,  0 ,  0 , 12 ,  0 ,  0 ,  0 , 13 ,
    10 ,  0 ,  8 ,  0 ,  0 ,  4 , 13 ,  0 ,  0 , 14 ,  9 ,  0 ,  0 , 12 ,  0 ,  6 ,
    0 ,  4 , 13 ,  2 ,  0 ,  0 , 12 ,  0 ,  0 ,  3 ,  0 ,  0 , 11 ,  1 ,  9 ,  0 ,
    0 ,  0 ,  0 , 12 ,  0 , 15 , 16 ,  0 ,  0 ,  1 ,  6 ,  0 ,  7 ,  0 ,  0 ,  0 ,
    14 , 10 ,  0 ,  0 , 13 ,  1 ,  0 ,  9 ,  4 ,  0 , 12 ,  5 ,  0 ,  0 ,  6 , 15 ,
    0 ,  0 , 11 , 15 ,  4 ,  0 ,  0 ,  2 , 13 ,  0 ,  0 ,  9 ,  1 , 14 ,  0 ,  0 ,
    0 ,  2 ,  1 ,  0 ,  0 ,  5 ,  8 ,  0 ,  0 , 15 , 10 ,  0 ,  0 , 16 , 11 ,  0 ]

index (_, _, 0) = "0\n"
index (x, y, n) 
  | n > 0 = (show $ (y-1) * radix * radix + (x-1) * radix + (n-1) + 1 + offset) ++ " "
  | n < 0 = "-" ++ (show $ (y-1) * radix * radix + (x-1) * radix + (-n-1) + 1 + offset) ++ " "

allGrid = [(x,y) | x <- [1..radix], y <- [1..radix] ]

main = do 
  let a = makeProblem problem ++ g ++ concatMap (\(x,y) -> concat $ hrzn(x,y) ++ vtcl(x,y) ++ box(x,y)) allGrid
  putStrLn $ "p cnf " ++ (show $ radix*radix*radix) ++ " " ++ (show $ cc a)
  putStr $ concatMap index $ a

-- make problem restrict
makeProblem l = mp 1 1 l
 where 
   next x y l 
     | x == radix = mp 1 (y+1) l
     | otherwise = mp (x+1) y l
   mp x y (n:l) 
     | n>0 = (x, y, n):((0,0,0):(next x y l))
     | otherwise = next x y l
   mp _ _ _ = []

-- input number to all grids
g = concatMap g' allGrid
 where 
   g' (x, y) = g'' radix
     where
       g'' 0 = [(x, y, 0)]
       g'' n = (x, y, n):(g'' $ n-1)

-- restrict number for sudoku rule
hrzn (x, y) = [[(x, y, -n), (x, y', -n), (0,0,0)] | n <- [1..radix], y' <- [1..radix]\\[y]]
vtcl (x, y) = [[(x, y, -n), (x', y, -n), (0,0,0)] | n <- [1..radix], x' <- [1..radix]\\[x]]
box (x, y) = [[(x, y, -n), (x', y', -n), (0,0,0)] | n <- [1..radix], x' <- [(boxS x)..(boxS x + boxSize -1)], y' <- [(boxS y)..(boxS y + boxSize -1)], (x/=x' || y/=y')]
 where boxS n = n - ((n-1) `mod` boxSize)

-- count clause
cc ((_, _, 0):l) = 1 + (cc l)
cc (_:l) = cc l
cc _ = 0
