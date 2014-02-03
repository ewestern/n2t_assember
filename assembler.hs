module Assembler where
import Types
import Code
import Parser
import Text.Parsec
import System.Environment 
import Symbol
import qualified Data.Map as Map
import Control.Monad.State


--assemble :: Program -> [String]
--assemble program = secondPass

--assemble :: Program -> [String]
--assemble program = Map.elems $ firstPass program symbolTable
maybe' = flip maybe id

assemble = map interpret
  where 
      --interpret (AInstruction s)      = aToBinary s symbolTable
        interpret (CInstruction d c j)  = destToBinary d ++ compToBinary c ++ jumpToBinary j  


-- want Counter monad: performs an action



firstPass :: Program -> SymbolTable -> SymbolTable
firstPass prog symMap = snd $ foldl enterSymbol (0, symMap) prog
  where enterSymbol (idx, map) inst = case inst of  AInstruction s -> (idx +1, map)
                                                    CInstruction d c j -> (idx +1, map)
                                                    Pseudo s -> (idx, Map.insert s (show idx) map)

--At its heart, a value of type (State s a) is a function from initial state s 
--to final value a and final state s: (a,s). 


--get set the result value to the state and left the state unchanged.
--put set the result value to () and set the state value.



secondPass :: Program -> SymbolTable -> 
secondPass prog symMap = do
  inst <- prog
  return inst

-- start state = (resultValue, SymbolTable)
--startState = (symbolTable, 15)

--secondPass :: Program State SymbolTable String
secondPass [] = do
  (str, _) <- get
  return str 

secondPass (x:xs) = do
  (table, idx) <- get
  let (byteStr, newTable) = case x of
    AInstruction s      -> aToBinary s table
    CInstruction d c j  -> Just cToBinary d c j


-- only going to update state when we enter a new AInstruction value
    where 
-- create new entry in the table of {str : numToBinary idx}
      newEntry str = Map.insert str 
      update (res, sym) =  

main = do
  args <- getArgs
  prog <- readFile $ head args
  let path = takeWhile (\x -> x /= '.') (head args)
  case parse parseProgram "" prog of
    Left err -> print err
    Right x -> writeFile (path ++ ".hack") (unlines $ assemble  x)

