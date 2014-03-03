{-#LANGUAGE FlexibleContexts #-}
module Main where
import Types
import Code
import Parser
import Text.Parsec hiding (State)
import System.Environment 
import qualified Data.Map as Map
import Control.Monad.State
import Data.String.Utils
import Data.Maybe
import Data.List
import qualified Data.HashTable.IO as H

type SymbolTable = H.BasicHashTable String String

firstPass :: SymbolTable -> Program -> Int -> IO ()
firstPass symTable [] idx = return ()

firstPass symTable (x:xs) idx = do
  case x of 
    AInstruction s      -> firstPass symTable xs (idx +1)
    CInstruction d c j  -> firstPass symTable xs (idx +1)
    Pseudo s            -> H.insert symTable s (fromJust $ aToBinary (show idx)) >> firstPass symTable xs idx

secondPass :: SymbolTable -> [Instruction] -> ([String], Int) -> IO [String]
secondPass table [] (strL, idx) = return strL
secondPass table (x:xs) (strL, idx) = do
  case x of
      AInstruction s ->  case aToBinary s of
                          -- if it's a number, add it to the binary
                          Just bs -> secondPass table xs (strL ++ [bs], idx)
                          Nothing -> do
                            ms <- H.lookup table s
                            case ms of 
                              -- if it's in the lookup table, add the looked-up value to the binary
                              Just ls -> secondPass table xs (strL ++ [ls], idx)
                              -- if not, insert
                              Nothing -> H.insert table s num >> secondPass table xs (strL ++ [num], idx + 1)
                                  where num = fromJust $ aToBinary (show idx)

      CInstruction d c j  -> secondPass table xs (strL ++ [(cToBinary d c j)], idx)
      _                   -> secondPass table xs (strL, idx)

assemble :: SymbolTable -> Program -> IO [String]
assemble table program = (firstPass table program 0) >> (secondPass table program ([], 16))

symbols :: [(String, String)]
symbols = map (\(k, v) -> (k, prefix v ++ v)) $ concat [[("SP", "0"), ("LCL", "1"), ("ARG", "10"), ("THIS", "11"),("THAT", "100"),("SCREEN", numToBin "16384"),("KBD", numToBin "24576")],
                  [("R" ++ show n, numToBin $ show n) | n <- [0..15]]]
            where prefix v = take (16 - (length v)) $ repeat '0'

main :: IO ()
main = do
  args <- getArgs
  prog <- readFile $ head args
  table <- H.fromList symbols
  let path = takeWhile (\x -> x /= '.') (head args)
  case parse parseProgram "" prog of
    Left err -> print err
    Right x -> do
      fp <- firstPass table x 0
      result <- assemble table x
      writeFile (path ++ ".hack") $ intercalate "\n" result
