module Symbol (
  SymbolTable,
  numToBin
  ) where

import Numeric
--import qualified Data.Map as Map 
import qualified Data.HashTable.IO as H

import Data.Char



numToBin :: String -> String
numToBin s = showIntAtBase 2 intToDigit (read s) ""

--symbolTable :: SymbolTable
--symbolTable = Map.unions [
--  (Map.fromList [("SP", "0"), ("LCL", "1"), ("ARG", "10"), ("THIS", "11"),("THAT", "110"),("SCREEN", numToBin "16384"),("KBD", numToBin "24576")]),
--  (Map.fromList [("R" ++ show n, numToBin $ show n) | n <- [0..15]])]
  --where prefix v = take (16 - (length $ numToBin v)) $ repeat '0'
--Map.map prefix 