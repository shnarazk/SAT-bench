{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Numeric (showFFloat)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Posix.Env (getEnvDefault)
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

version :: String
version = "gsink 0.1.0"

data ConfigurationOption = ConfigurationOption
                     {
                       header :: String
                     , timeout :: Double
                     }
                         deriving (Show)

defaultConfigration = ConfigurationOption
  {
    header = ""
  , timeout = 2000.0
  }

options :: [OptDescr (ConfigurationOption -> ConfigurationOption)]
options =
  [
    Option ['s'] ["header"]
     (ReqArg (\v c -> c { header = v }) "'haeder'")
     "a header string"
  , Option ['t'] ["timeout"]
     (ReqArg (\v c -> c { timeout = read v }) (show (timeout defaultConfigration)))
     "a default timeout"
    ]

usage :: String
usage = "[" ++ version ++ "] Usage: gsink [OPTIONS]"

parseOptions :: [String] -> IO ConfigurationOption
parseOptions argv =
    case getOpt Permute options argv of
      (o, [], []) -> return $ foldl (flip id) defaultConfigration o
      (o, l, [])  -> return $ foldl (flip id) defaultConfigration o
      (_, _, err) -> ioError (userError (concat err ++ usageInfo usage options))

data LBool = LFalse | LBottom | LTrue

instance Semigroup LBool where
  x <> LBottom = x
  LBottom <> x = x
  x <> _       = x

instance Monoid LBool where
  mempty = LBottom

data Result = Result LBool Double

instance Semigroup Result where
  (Result b1 x) <> (Result b2 y) = Result (mappend b1 b2) (max x y)

instance Monoid Result where
  mempty = Result mempty 0

main :: IO ()
main = do
  args <- getArgs
  conf <- parseOptions $ args
  str <- getContents
  case parse cpuAndResult "" str of
    Left _   -> putStrLn $ header conf ++  showFFloat (Just 3) (timeout conf) ",3"
{-
    Left err -> let l = lines str !! (sourceLine (errorPos err) - 1)
                    i = replicate (sourceColumn (errorPos err) -1) ' ' ++ "^\n"
                in do putStrLn(l ++ "\n" ++ i ++ show err)
                      mapM_ putStrLn $ zipWith (++) (map ((++ ": ") . show)  [1 ..]) (lines str)
-}
    Right (Result LTrue x)   -> putStrLn $ header conf ++ showFFloat (Just 3) x ",1"
    Right (Result LFalse x)  -> putStrLn $ header conf ++ showFFloat (Just 3) x ",2"
    Right (Result LBottom x) -> putStrLn $ header conf ++ showFFloat (Just 3) x ",3"
  return ()

cpuAndResult :: Parsec String () Result
cpuAndResult = do
  manyTill otherLine (try (string "c CPU time"))
  t <- timeLine
  manyTill anyChar endOfLine
  r <- choice [try satLine, try unsatLine]
  return $ mappend r t

timeLine = do
  spaces <* char ':' <* spaces
  x <- many1 digit <* char '.'
  y <- many1 digit <* spaces <* char 's' <* endOfLine
  return $ Result mempty (read (x ++ "." ++ y))
  
satLine :: Parsec String () Result
satLine = do
  string "s SATISFIABLE"
  return $ Result LTrue 0

unsatLine :: Parsec String () Result
unsatLine = do
  string "s UNSATISFIABLE"
  return $ Result LFalse 0

otherLine :: Parsec String () ()
otherLine = do manyTill anyChar (try endOfLine); return ()
