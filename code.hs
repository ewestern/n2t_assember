module Code (
    Table,
    destToBinary,
    compToBinary,
    jumpToBinary,
    aToBinary,
    cToBinary
    ) where

import Parser
import Types
import Numeric
import Data.Char
import Symbol
import qualified Data.Map as Map

type Table k v = Map.Map k v


destToBinary :: [Register] -> String
destToBinary regs = map convert [A, D, M]
  where convert reg = (if reg `elem` regs then '1' else '0')

compTable :: Table Comp String
compTable = Map.fromList [
                      (Comp Nothing Nothing Nothing (Just Zero), "101010"), -- 0
                      (Comp Nothing Nothing  Nothing (Just One), "111111"), -- 1                      
                      (Comp Nothing (Just Minus) Nothing (Just One), "111010"), -- -1                      
                      (Comp (Just D) Nothing Nothing  Nothing, "001100"), --D
                      (Comp (Just A) Nothing Nothing Nothing, "110000"), -- A                  
                      (Comp (Just D) (Just Not) Nothing  Nothing, "001101"), -- !D
                      (Comp (Just A) (Just Not) Nothing Nothing, "110001"), --  !A
                      (Comp (Just D) (Just Minus) Nothing Nothing, "001111"), -- -D
                      (Comp (Just A) (Just Minus) Nothing Nothing, "110011"), -- -A
                      (Comp (Just D) (Just Plus) Nothing (Just One), "011111"), -- D+1
                      (Comp (Just A) (Just Plus) Nothing (Just One), "110111"), -- A+1
                      (Comp (Just D) (Just Minus) Nothing (Just One), "001110"), -- D-1
                      (Comp (Just A) (Just Minus) Nothing (Just One), "110010"), -- A-1
                      (Comp (Just D) (Just Plus) (Just A) Nothing, "110010"), -- D+A
                      (Comp (Just A) (Just Plus) (Just D) Nothing, "110010"), -- A+D
                      (Comp (Just D) (Just Minus) (Just A) Nothing, "010011"), -- D-A
                      (Comp (Just A) (Just Minus) (Just D) Nothing, "000111"), -- A-D
                      (Comp (Just D) (Just And) (Just A) Nothing, "000000"), -- D&A
                      (Comp (Just A) (Just And) (Just D) Nothing, "000000"), -- A&D
                      (Comp (Just D) (Just Or) (Just A) Nothing, "010101"), -- D|A
                      (Comp (Just A) (Just Or) (Just D) Nothing, "010101") -- A|D
                      ]

compToBinary :: Comp -> String
compToBinary k = "111" ++ [aValue k] ++ mapVal k
  where aValue (Comp (Just M) _ _ _)      = '1'
        aValue (Comp _ _ (Just M) _)      = '1'
        aValue _                          = '0'
        mapVal (Comp (Just M) op r2 bin)  = Map.findWithDefault (show k) (Comp (Just A) op r2 bin) compTable
        mapVal (Comp r1 op (Just M) bin)  = Map.findWithDefault (show k) (Comp r1 op (Just A) bin) compTable
        mapVal comp                       = Map.findWithDefault (show k) comp compTable

jumpTable :: Table (Maybe Jump) String
jumpTable = Map.fromList [
      (Just JGT, "001"),
      (Just JEQ, "010"),
      (Just JGE, "011"),
      (Just JLT, "100"),
      (Just JNE, "101"),
      (Just JLE, "110"),
      (Just JMP, "111")
  ]

jumpToBinary :: (Maybe Jump) -> String
jumpToBinary k = Map.findWithDefault "000" k jumpTable

isNumeric :: String -> Bool
isNumeric s = length (dropWhile (\x -> isDigit x) s) == 0

cToBinary :: [Register] -> Comp -> (Maybe Jump) -> String
cToBinary d c j = destToBinary d ++ compToBinary c ++ jumpToBinary j

aToBinary :: String -> SymbolTable -> Maybe String
aToBinary s symTable
  | isNumeric s     = Just $ prefix s ++ numToBin s
  | otherwise       = Map.lookup s symTable
  where prefix v = take (16 - (length $ numToBin v)) $ repeat '0'

--aToBinary :: String -> Int -> SymbolTable -> (String, SymbolTable, Int)
--aToBinary str idx symTable
---- e.g @123
--  | isNumeric str           = (convert str, symTable, idx)
--  --e.g @sum && in symbol table
--  | Map.member str symTable = (maybe "" id (Map.lookup str symTable), symTable, idx)
--  -- e.g @sum && not in symbol table
--  --      add {sum: idx} to table
--  | otherwise             = (convert $ show (idx + 1), Map.insert str (convert $ show (idx + 1)) symTable, idx + 1)
--  where 
--    prefix v = take (16 - (length $ numToBin v)) $ repeat '0'
--    convert v = (prefix v ++ numToBin v)
