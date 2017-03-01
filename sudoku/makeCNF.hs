-- solving sudoku for SAT solver
import Data.List

offset = 0

problem = 
  [5,3,0,0,7,0,0,0,0
  ,6,0,0,1,9,5,0,0,0
  ,0,9,8,0,0,0,0,6,0
  ,8,0,0,0,6,0,0,0,3
  ,4,0,0,8,0,3,0,0,1
  ,7,0,0,0,2,0,0,0,6
  ,0,6,0,0,0,0,2,8,0
  ,0,0,0,4,1,9,0,0,5
  ,0,0,0,0,8,0,0,7,9]

index (_, _, 0) = "0\n"
index (x, y, n) 
  | n > 0 = (show $ (y-1) * 9 * 9 + (x-1) * 9 + (n-1) + 1 + offset) ++ " "
  | n < 0 = "-" ++ (show $ (y-1) * 9 * 9 + (x-1) * 9 + (-n-1) + 1 + offset) ++ " "

allGrid = [(x,y) | x <- [1..9], y <- [1..9] ]

main = do 
  let a = makeProblem problem ++ g ++ concatMap (\(x,y) -> concat $ hrzn(x,y) ++ vtcl(x,y) ++ box(x,y)) allGrid
  putStrLn $ "p cnf " ++ (show $ 9*9*9) ++ " " ++ (show $ cc a)
  putStr $ concatMap index $ a

toList =snd (break  l)
  where
    showClause c
      | length c == 1 = showLiteral $ head c
      | otherwise = "(" ++ intercalate " \\vee " [showLiteral l | l <- c] ++ ")"
    showLiteral l
      | l < 0 = "\\neg " ++ "x_{" ++ show (abs l) ++ "} "
      | otherwise = "x_{" ++ show (abs l) ++ "} "
    l = makeProblem problem ++ g ++ concatMap  (\(x,y) -> concat $ hrzn(x,y) ++ vtcl(x,y) ++ box(x,y)) allGrid
    break :: [(Int, Int, Int)] -> ([Int], [[Int]])
    break [] = ([], [])
    break (x:l)
      | x == (0,0,0) = if a == [] then ([], r) else ([], a:r)
      | otherwise = (toLit x:a, r)
      where
        (a, r) = break l
    toLit (x, y, n)
      | n == 0 = (y-1) * 9 * 9 + (x-1) * 9 + (n-1) + 1 + offset
      | n > 0 = (y-1) * 9 * 9 + (x-1) * 9 + (n-1) + 1 + offset
      | n < 0 = negate $ (y-1) * 9 * 9 + (x-1) * 9 + (-n-1) + 1 + offset

toLatex = intercalate " \n\\wedge " [showClause c | c <- snd (break  l)]
  where
    showClause c
      | length c == 1 = showLiteral $ head c
      | otherwise = "(" ++ intercalate " \\vee " [showLiteral l | l <- c] ++ ")"
    showLiteral l
      | l < 0 = "\\neg " ++ "x_{" ++ show (abs l) ++ "} "
      | otherwise = "x_{" ++ show (abs l) ++ "} "
    l = makeProblem problem ++ g ++ concatMap  (\(x,y) -> concat $ hrzn(x,y) ++ vtcl(x,y) ++ box(x,y)) allGrid
    break :: [(Int, Int, Int)] -> ([Int], [[Int]])
    break [] = ([], [])
    break (x:l)
      | x == (0,0,0) = if a == [] then ([], r) else ([], a:r)
      | otherwise = (toLit x:a, r)
      where
        (a, r) = break l
    toLit (_, _, 0) = 0
    toLit (x, y, n) 
      | n > 0 = (y-1) * 9 * 9 + (x-1) * 9 + (n-1) + 1 + offset
      | n < 0 = negate $ (y-1) * 9 * 9 + (x-1) * 9 + (-n-1) + 1 + offset

-- | make problem restrict
makeProblem l = mp 1 1 l
 where 
   next x y l 
     | x == 9 = mp 1 (y+1) l
     | otherwise = mp (x+1) y l
   mp x y (n:l) 
     | n>0 = (x, y, n):((0,0,0):(next x y l))
     | otherwise = next x y l
   mp _ _ _ = []

-- | input number to all grids
g = concatMap g' allGrid
 where 
   g' (x, y) = g'' 9
     where
       g'' 1 = [(x, y, 1)]
       g'' n = (x, y, n):(g'' $ n-1)

-- | restrict number for sudoku rule
hrzn (x, y) = [[(x, y, -n), (x, y', -n), (0,0,0)] | n <- [1..9], y' <- [1..9]\\[y]]
vtcl (x, y) = [[(x, y, -n), (x', y, -n), (0,0,0)] | n <- [1..9], x' <- [1..9]\\[x]]
box (x, y) = [[(x, y, -n), (x', y', -n), (0,0,0)] | n <- [1..9], x' <- [(boxS x)..(boxS x + 2)], y' <- [(boxS y)..(boxS y + 2)], (x/=x' || y/=y')]
 where boxS n = n - ((n-1) `mod` 3)

-- | count clause
cc ((_, _, 0):l) = 1 + (cc l)
cc (_:l) = cc l
cc _ = 0
