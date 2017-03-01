-- This is a derived version of parseLog to generate a R's dataframe file from logs
-- adapt for format 406
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Ord
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.IO
import Text.Printf
import Sih4LogFormat

data Stat = Stat
            {
              targetFile :: String
            , executionTime :: Double
            , restartIntervalScale :: Double
            , restartSizeScale :: Double
            , restartConflictScale :: Double
            }
            deriving (Eq, Show, Ord)

main :: IO ()
main = do
  (dir: _) <- getArgs
  files <- liftM (map ((dir ++) . (pathSeparator :)) . filter (isSuffixOf ".json")) $ getDirectoryContents dir
  stats <- liftM (sortBy (comparing (abs . restartSizeScale))) $ mapM toLog files
  let (is, cs) = groups stats
  let isTarget k i c x = isInfixOf k (targetFile x) && (i == 0 || restartIntervalScale x == i) && (c == 0 || restartConflictScale x == c)
  withFile (dir ++ ".df") WriteMode $ \h -> do
    hPutStrLn h "problem, scale, conflict, weight, time"
    forM_ ["UF125", "UF150", "UF175", "UF200", "UF225", "een", "ibm"] $ \p -> do
      forM_ is $ \i -> do
        forM_ cs $ \c -> do
          let l = filter (isTarget p i c) stats
          mapM_ (\s -> hPutStrLn h $ printf "%s, %7.3f, %7.3f, %7.3f, %7.3f" p i c (restartSizeScale s) (executionTime s)) l

toLog :: FilePath -> IO Stat
toLog !file = do
  (Just runs) <- decode <$> B.readFile file
  let params = restart . solver . head $ runs
  let st = map stats runs
  let elapsed = sum $ map elapsedTime st
  let assign = average $ map assignments st
  let conf = average $ map conflicts st
  let unass = average $ map unassignments st
  return $ Stat file elapsed (interval params) (size params) (conflict params)

groups :: [Stat] -> ([Double], [Double])
groups l =(sort $ nub intervals, sort $ nub starts)
  where
    intervals = map restartIntervalScale l
    starts = map restartConflictScale l

average :: Real a => [a] -> Double
average l = (fromRational . toRational $ sum l) / fromIntegral (length l)

