import Text.ParserCombinators.Parsec
import Text.Printf

main = do 
  src <- getContents
  putStr $ concatMap indexr $ readResult src

readResult src = case parse (do 
                                string "SAT"
                                many space
                                num <- many readVar
                                return num) [] src of
    Right madeSrc -> madeSrc
    Left err -> error $ show err

readVar :: Parser Int
readVar = do string "-"
             n <- many1 digit
             many1 space
             return $ negate $ read n
      <|> do n <- many1 digit
             many1 space
             return $ read n

radix = 16

indexr m 
  | m > 0 = 
    let 
      y = ((m-1) `div` (radix * radix)) + 1
      m' = ((m-1) `mod` (radix * radix))
      x = (m' `div` radix) + 1
      n = (m' `mod` radix) + 1
    in (printf "%2d " n) ++ if x==radix then "\n" else []
  | otherwise = []
