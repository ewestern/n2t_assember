module Assembler where
import Types
import Code
import Parser
import Text.Parsec
import System.Environment 
import Symbol
import qualified Data.Map as Map


assemble :: Program -> [String]
assemble program = Map.elems $ firstPass program symbolTable
--assemble = map interpret
--  where interpret (AInstruction s)      = aToBinary s
--        interpret (CInstruction d c j)  = destToBinary d ++ compToBinary c ++ jumpToBinary j  


-- want Counter monad: performs an action

firstPass :: Program -> SymbolTable -> SymbolTable
firstPass prog symMap = snd $ foldl enterSymbol (0, symMap) prog
  where enterSymbol (idx, map) inst = case inst of  AInstruction s -> (idx +1, map)
                                                    CInstruction d c j -> (idx +1, map)
                                                    Pseudo s -> (idx, Map.insert s (show idx) map)

secondPass :: Program -> SymbolTable -> [String]
secondPass prog symMap = snd $ foldl interpret (16, symMap) prog
  where   interpret (AInstruction s)      = aToBinary s symMap
          interpret (CInstruction d c j)  = cToBinary d c j 


main = do
  args <- getArgs
  prog <- readFile $ head args
  let path = takeWhile (\x -> x /= '.') (head args)
  --let i = (last elemIndices '.' (head args))

  case parse parseProgram "" prog of
    Left err -> print err
    Right x -> writeFile (path ++ ".hack") (unlines $ assemble  x)

