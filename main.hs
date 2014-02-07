module Main where
import Types
import Code
import Parser
import Text.Parsec hiding (State)
import System.Environment 
import qualified Data.Map as Map
import qualified Data.HashTable.IO as H
import Control.Monad.State
import Data.String.Utils
import Data.Maybe

type TableState = (String, SymbolTable)
type StateMonad = State TableState
maybe' = flip maybe id

firstPass :: Program -> (String -> String -> ())
firstPass prog insert = snd $ foldl enterSymbol 0 prog
  where enterSymbol idx inst = case inst of AInstruction s -> idx +1
                                                     CInstruction d c j -> (idx +1, insert)
                                                     Pseudo s -> (idx, insert s (fromJust $ aToBinary (show idx)))

--insert':: SymbolTable -> k -> v -> SymbolTable
--insert' table k v = do

--  H.insert table k v



translateAndUpdate :: Instruction -> AssembleState -> AssembleState
translateAndUpdate inst (istr, table, idx) = case inst of
                                      AInstruction s      -> case aToBinary s table of 
                                                              Just bs -> (istr ++ "\n" ++ bs, table, idx)
                                                              Nothing ->  let   bs      = fromJust $ aToBinary (show idx) table
                                                                                aTable  = H.insert table s bs 
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

--assemble table program = evalState (secondPass program) initialState
--  where initialState = ("", firstPass program table, 16)

symbols = concat [[("SP", "0"), ("LCL", "1"), ("ARG", "10"), ("THIS", "11"),("THAT", "110"),("SCREEN", numToBin "16384"),("KBD", numToBin "24576")],
                  [("R" ++ show n, numToBin $ show n) | n <- [0..15]]]

main = do
  args <- getArgs
  prog <- readFile $ head args
  table <- H.fromList symbols 
  --let sT = 
  let path = takeWhile (\x -> x /= '.') (head args)
  case parse parseProgram "" prog of
    Left err -> print err
    --Right x -> print $ firstPass x sy
    --Right x -> writeFile (path ++ ".hack") $ let (byteStr, sT, idx) = assemble table x in lstrip byteStr
