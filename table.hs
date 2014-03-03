module SymbolTable {
    
} where

import qualified Data.Map as Map

-- todo: construct 
comp

table :: Map k v
table = fromList [()]

dest :: Map k v
dest = fromList [("M","001" ), ("D" ,"010"),("MD", "011"),("A", "100"),("AM", "101"),("AD", "110"),("AMD", "111")]

jump :: Map k v
jump = fromList [("JGT", "001"), 
                ("JEQ", "010"),
                ("JGE", "011"),
                ("JLT","100"),
                ("JNE", "101"),
                ("JLE", "110"),
                ("JMP", "111")]
