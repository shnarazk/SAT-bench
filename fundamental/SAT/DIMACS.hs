{-# LANGUAGE Safe #-}

-- | Read/Write a CNF file only with ghc standard libraries
module SAT.DIMACS
       (
         -- * Input
         fromFile
       , clauseListFromFile
       , fromMinisatOutput
       , clauseListFromMinisatOutput
         -- * Output
       , toFile
       , toDIMACSString
       , asDIMACSString
       , asDIMACSString_
         -- * Bool Operation
       , module SAT.BoolExp
       )
       where
import SAT.DIMACS.Reader
import SAT.DIMACS.Writer
import SAT.DIMACS.MinisatReader
import SAT.BoolExp

-- | String from BoolFrom
asDIMACSString :: BoolForm -> String
asDIMACSString = toDIMACSString . asList

-- | String from BoolFrom
asDIMACSString_ :: BoolForm -> String
asDIMACSString_ = toDIMACSString . asList_
