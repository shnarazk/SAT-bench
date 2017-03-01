-- merge Sih4Log files into a Sih4LogList file
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)

main :: IO ()
main = do
  mapM_ B.putStr . zipWith B.append ("[\n" : repeat ",\n") =<< mapM B.readFile =<< getArgs
  putStrLn "]"


