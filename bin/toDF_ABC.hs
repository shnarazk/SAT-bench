-- | generate a R's DataFrame file (`keyword`-*.df) and BenchmarkSummary file (`keyword`-*-bm.json)
--   from Sih4LogList file (*.json)
-- USAGE:
-- $0 keyword-for-generated-files
-- adapt for format 406
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Ord
import System.Environment (getArgs)
import System.Directory
import System.IO
import Text.Printf
import Sih4LogFormat
import Sih4Lab

data Stat = Stat
            {
              targetFile :: String
            , executionTime :: Double
            , pA :: Double
            , pB :: Double
            , pC :: Double
            }
            deriving (Eq, Show, Ord)

main :: IO ()
main = do
  (dir: _) <- getArgs
  -- files <- liftM (map ((dir ++) . (pathSeparator :)) . filter (isSuffixOf ".json")) $ getDirectoryContents dir
  files <-liftM (filter (not . isPrefixOf "benchmark-") . filter (isSuffixOf ".json")) $ getDirectoryContents "."
  ls <- liftM (sortBy (comparing pA)) $ mapM toStat files
  let (as, bs, cs) = groups ls
  let
    isTarget :: String -> Double -> Double -> Double -> Stat -> Bool
    isTarget k a b c x = isInfixOf k (targetFile x) && (pA x == a) && (pB x == b) && (pC x == c)
  withFile (dir ++ ".df") WriteMode $ \h -> do
    hPutStrLn h "problem, A, B, C, time"
    forM_ ["UF125", "UF150", "UF175", "UF200", "UF225", "een", "ibm"] $ \p -> do
      forM_ as $ \a -> do
        forM_ bs $ \b -> do
          forM_ cs $ \c -> do
            mapM_ (\(Stat _ t _ _ _) -> hPutStrLn h $ printf "%s, %7.3f, %7.3f, %7.3f, %7.3f" p a b c t) $ filter (isTarget p a b c) ls

  let es = [ BenchmarkEntry p (show (a, b, c)) t (pdfFilenameFor p a b c)
           | p <- ["UF125", "UF150", "UF175", "UF200", "UF225", "een", "ibm"]
           , a <- as
           , b <- bs
           , c <- cs
           , Stat _ t _ _ _ <- filter (isTarget p a b c) ls]
  withFile (dir ++ "-bm.json") WriteMode $ \h -> do
    B.hPutStr h . encode $ BenchmarkSummary es
    hPutStr h "\n"

toStat :: FilePath -> IO Stat
toStat filename = do
  (Just runs) <- decode <$> B.readFile filename
  -- let params = restart . solver . head $ runs
  let st = map stats runs
  let elapsed = sum $ map elapsedTime st -- use `sum` here, caz we need the total time of uf-01.cnf .. uf-099.cnf
  -- let assign = average $ map assignments st
  -- let conf = average $ map conflicts st
  -- let unass = average $ map unassignments st
  let a = extraParameter1 . solver . head $ runs
  let b = extraParameter2 . solver . head $ runs
  let c = extraParameter3 . solver . head $ runs
  return $ Stat filename elapsed a b c

groups :: [Stat] -> ([Double], [Double], [Double])
groups l =(canonize pA, canonize pB, canonize pC)
  where canonize = sort . nub . flip map l

average :: Real a => [a] -> Double
average l = (fromRational . toRational $ sum l) / fromIntegral (length l)

-- roundFloat :: Double -> Double
-- roundFloat x = read ((printf "%3.3f\n" x) :: String)
