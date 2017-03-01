-- | analyze the output of sih4 in assignLogger branch, then dump a CSV file
-- USAGE
-- $0 < s4l(l).json > a.csv
-- where
--  s4l(l) is Sih4Log or Sih4LogList
--  at restart point, the 1st element is set to -1
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Parallel.Strategies
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Numeric (showFFloat)
import System.Console.GetOpt
import System.Environment (getArgs)
import Sih4LogFormat

data ConfigurationOption = ConfigurationOption
                           {
                             segmentLen :: Int		-- ^ セグメントの長さ
                           }

defaultConfigration = ConfigurationOption
    {
      segmentLen = 1
    }

options :: [OptDescr (ConfigurationOption -> ConfigurationOption)]
options =
  [
    Option ['l'] ["segment-length"]
    (ReqArg (\v c -> c { segmentLen = read v }) (show (segmentLen defaultConfigration)))
    "segment length"
  ]
usage :: String
usage = "Usage: analyzeAssignLogToDF [OPTIONS] < output-log"

parseOptions :: [String] -> IO ConfigurationOption
parseOptions argv =
    case getOpt Permute options argv of
      (o, [], []) -> do
        return $ foldl (flip id) defaultConfigration o
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo usage options))

-- | triplet by
type ReversedMDLLog =[(Int, Int, Int)]
type MDLLog =[(Int, Int, Int)]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

isRestartPoint :: (Int, Int, Int) -> Bool
isRestartPoint = (== -1) . fst3

type Dump =[(Double, Int)]

getMdlLogs :: IO [ReversedMDLLog]
getMdlLogs = do
  res <- decode <$> B.getContents
  case res of
    Nothing -> return []
    Just l  -> return  $ map (read . mdlLog . stats) l

main :: IO ()
-- main = mapM_ dump . map (convertDL (segmentLength conf)) =<< getMdlLogs
-- | 最近のお気に入り
--main = mapM_ dump . map (convertMaxDL (segmentLength conf)) =<< getMdlLogs

-- | RestartByConflict実験ブランチ用
main = do
  conf <- parseOptions =<< getArgs
  mapM_ dump . {- withStrategy rpar -} map (convertAllMaxDL (segmentLen conf)) =<< getMdlLogs

-- | ヒストグラム生成用
-- main = mapM_ hist =<< getMdlLogs

dump :: Dump -> IO ()
dump  l = mapM_ (\(x, y) -> putStrLn (showFFloat (Just 4) x ",\t" ++ show y)) l

hist :: ReversedMDLLog -> IO ()
hist  = putStrLn . show. length . lastRestartSegment

toSegments :: Int -> MDLLog -> [MDLLog]
toSegments len = splitBy len

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n l
  | length l <= n = [l]
  | otherwise = take n l : splitBy n (drop n l)

initialRestartSegment :: ReversedMDLLog -> ReversedMDLLog
initialRestartSegment = reverse . takeWhile (not . isRestartPoint) . reverse

lastRestartSegment :: ReversedMDLLog -> ReversedMDLLog
lastRestartSegment = takeWhile (not . isRestartPoint)

splitAtRestart :: ReversedMDLLog -> [ReversedMDLLog]
splitAtRestart [] = []
splitAtRestart l
  | null b =[a]
  | otherwise = a : splitAtRestart (tail b)
  where
    (a, b) = break isRestartPoint l

--------------------------------------------------------------------------------

-- | 決定レベル対割当量
convertDL :: ReversedMDLLog -> Dump
convertDL = convertDL' . lastRestartSegment

convertDL' :: ReversedMDLLog -> Dump
convertDL' [] = []
convertDL' [_] = []
convertDL' ((dl, num, _) : (dl', num', new') : l) = (fromIntegral dl, num-num') : convertDL ((dl', num', new') : l)

--------------------------------------------------------------------------------

-- | 正規化された時間対割当量
convert :: ReversedMDLLog -> Dump
convert l = zip (map ((/ len). fromIntegral) [1 :: Int ..]) . reverse . convert' $ lastRestartSegment l
  where
    len = fromIntegral $ length l

--------------------------------------------------------------------------------

-- | 時間対割当量
convertN :: ReversedMDLLog -> Dump
convertN l = zip (map fromIntegral [1:: Int ..]) . reverse . convert' $ lastRestartSegment l

convert' :: ReversedMDLLog -> [Int]
convert' [] = []
convert' [(_, num, _)] = [num]
convert' ((_, num, _) : (dl', num', new') : l) = (num-num') : convert' ((dl', num', new') : l)

--------------------------------------------------------------------------------

-- | 最後のリスタート後の時間対セグメント内最大決定レベル
convertMaxDL :: Int -> ReversedMDLLog -> Dump
convertMaxDL len l = zip [1 ..] . map (subtract x) . decseq len . lastRestartSegment $ l
  where
    x = maximum . map fst3 $ initialRestartSegment l

decseq :: Int -> ReversedMDLLog -> [Int]
decseq len = reverse . map (\x -> if null x then 0 else maximum (map fst3 x)) . toSegments len

--------------------------------------------------------------------------------

-- | 全リスタートスパンに渡る時間対セグメント内最大決定レベル
convertAllMaxDL :: Int -> ReversedMDLLog -> Dump
convertAllMaxDL len l = concatMap (zip [1 ..] . map (subtract x) . decseq len) $ splitAtRestart l
  where
    x = maximum . map fst3 $ initialRestartSegment l
