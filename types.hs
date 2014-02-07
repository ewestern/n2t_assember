module Types (
  Instruction(..),
  Register(..),
  Comp(..),
  Operator(..),
  Bin(..),
  Jump(..),
  Program(..),
  SymbolTable,
  Table,
  AssembleState
  ) where

import qualified Data.HashTable.IO as H
import qualified Data.Map as M
type SymbolTable = H.BasicHashTable String String
type Table k v = M.Map k v
type AssembleState = (String, SymbolTable, Int)
data Operator = Plus | Minus | Or | And | Not deriving (Show, Eq, Ord)
data Register = A | M | D  deriving (Show, Eq, Ord)
data Bin = Zero | One deriving (Show, Eq, Ord)
data Jump = JGT | JEQ | JGE | JLT | JNE | JLE | JMP deriving (Show, Eq, Ord)

data Comp = Comp {
  reg1 :: Maybe Register,
  op :: Maybe Operator,
  reg2 :: Maybe Register,
  bin :: Maybe Bin
} deriving (Show, Eq, Ord)

data Instruction = AInstruction String 
      | CInstruction {
        dest :: [Register],
        comp :: Comp,
        jump :: Maybe Jump
} | Pseudo String deriving (Show)

type Program = [Instruction]
