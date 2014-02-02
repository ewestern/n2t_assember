module Symbol (
  symbolTable,
  SymbolTable
  ) where

import Numeric
import qualified Data.Map as Map 
import Data.Char

type SymbolTable = Map.Map String String

numToBin :: String -> String
numToBin s = showIntAtBase 2 intToDigit (read s) ""

symbolTable :: SymbolTable
symbolTable = Map.unions [
  (Map.fromList [("SP", "0"), ("LCL", "1"), ("ARG", "10"), ("THIS", "11"),("THAT", "110"),("SCREEN", numToBin "16384"),("KBD", numToBin "24576")]),
  (Map.fromList [("R" ++ show n, numToBin $ show n) | n <- [0..15]])]
