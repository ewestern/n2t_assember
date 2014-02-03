module Parser (
  parseProgram
)
 where

import Types
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr hiding (Operator)
import Text.Parsec.Perm
import Text.Parsec.Combinator 
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Numeric
import Data.Char

--class Prim a where
--  makeComp      :: a -> Comp
--  extendComp    :: Comp -> a -> Comp


langdef :: LanguageDef a
langdef = emptyDef { P.commentStart    = "/*"
                   , P.commentEnd      = "*/"
                   , P.commentLine     = "//"
                   , P.identStart       = upper
                   , P.identLetter     = alphaNum
           }

lexer :: P.TokenParser a
lexer  = P.makeTokenParser langdef

whiteSpace  = P.whiteSpace lexer
lexeme      = P.lexeme lexer
symbol      = P.symbol lexer
natural     = P.natural lexer
parens      = P.parens lexer
semi        = P.semi lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
operator    = P.operator lexer

parseAInst :: Parser Instruction
parseAInst = do
    char '@'
    x <- many1 alphaNum
    return $ AInstruction x
    <?> "parse AInst"

parseDest :: Parser [Register]
parseDest = do
    d <- many1 parseReg
    char '='
    return d

parseOp :: Parser Operator
parseOp = do
  o <- oneOf "+!-|&"
  case o of
    '+' -> return Plus
    '!' -> return Not
    '-' -> return Minus
    '|' -> return Or
    '&' -> return And

parseReg :: Parser Register 
parseReg = do
  r <- oneOf "DAM"
  case r of
    'D' -> return D 
    'A' -> return A
    'M' -> return M 

parseBin :: Parser Bin
parseBin = do
  r <- oneOf "01"
  case r of
    '0' -> return Zero
    '1' -> return One

parseComp :: Parser Comp
parseComp = do
  reg1 <- optionMaybe (try parseReg)
  op <- optionMaybe (try parseOp)
  reg2 <- optionMaybe (try parseReg)
  bin <- optionMaybe (try parseBin)
  let c = Comp{reg1 = reg1, op = op, reg2 = reg2, bin = bin}
  case c of
    Comp Nothing Nothing Nothing Nothing -> fail "Cannot parse empty string"
    Comp (Just A) _ (Just M) _            -> fail "Cannot use two A registers"
    Comp (Just M) _ (Just A) _            -> fail "Cannot use two A registers"
    _ -> return c


parseJump :: Parser Jump
parseJump = do
    semi
    j <- identifier
    case j of 
      "JGT" -> return JGT
      "JEQ" -> return JEQ
      "JGE" -> return JGE
      "JLT" -> return JLT          
      "JNE" -> return JNE
      "JLE" -> return JLE
      "JMP" -> return JMP

parseCInst :: Parser Instruction
parseCInst = do
    dest <-  option [] (try parseDest) <?> "fail parse dest"
    c <- parseComp
    jump <- optionMaybe parseJump
    return $ CInstruction{dest = dest, comp = c, jump = jump} 

parsePseudo :: Parser Instruction
parsePseudo =  do
  value <- parens $ many1 letter
  return $ Pseudo value


parseProgram :: Parser Program
parseProgram = do
    whiteSpace
    result <- many $ lexeme $ try parseAInst <|> parseCInst <|> parsePseudo
    eof
    return result


--"@2\nD=A\n@3\nD=D+A\n@0\n\nM=D"