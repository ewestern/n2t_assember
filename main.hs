module Main where
import Types
import Code
import Parser
import Text.Parsec hiding (State)
import System.Environment 
import Symbol
import qualified Data.Map as Map
import Control.Monad.State
import Data.String.Utils
import Data.Maybe

type TableState = (String, SymbolTable)
type StateMonad = State TableState
maybe' = flip maybe id

firstPass :: Program -> SymbolTable -> SymbolTable
firstPass prog symMap = snd $ foldl enterSymbol (0, symMap) prog
  where enterSymbol (idx, map) inst = case inst of  AInstruction s -> (idx +1, map)
                                                    CInstruction d c j -> (idx +1, map)
                                                    Pseudo s -> (idx, Map.insert s (fromJust $ aToBinary (show idx) map) map)


type AssembleState = (String, SymbolTable, Int)

translateAndUpdate :: Instruction -> AssembleState -> AssembleState
translateAndUpdate inst (istr, table, idx) = case inst of
                                      AInstruction s      -> case aToBinary s table of 
                                                              Just bs -> (istr ++ "\n" ++ bs, table, idx)
                                                              Nothing ->  let   bs      = fromJust $ aToBinary (show idx) table
                                                                                aTable  = Map.insert s bs table
                                                                          in (istr ++ "\n" ++ bs, aTable, idx + 1)
                                      CInstruction d c j  -> let cstr = cToBinary d c j in (istr ++ "\n" ++ cstr, table, idx) 
                                      _                   -> (istr, table, idx)

secondPass [] = do
  st <- get
  return st

secondPass (x:xs) = do  
  st <- get
  put $ translateAndUpdate x st
  secondPass xs

assemble program = evalState (secondPass program) initialState
  where initialState = ("", firstPass program symbolTable, 16)

main = do
  args <- getArgs
  prog <- readFile $ head args
  let path = takeWhile (\x -> x /= '.') (head args)
  case parse parseProgram "" prog of
    Left err -> print err
    --Right x -> print $ firstPass x sy
    Right x -> writeFile (path ++ ".hack") $ let (byteStr, sT, idx) = assemble x in lstrip byteStr