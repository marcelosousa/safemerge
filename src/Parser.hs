{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Parser where

import Data.Char
import qualified Data.Map as M

import Types

import Text.ParserCombinators.UU hiding (parse, (<$$>))
import Text.ParserCombinators.UU.Utils hiding (pLParen, pRParen)
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Demo.Examples (run)

(**>) :: Applicative f => f a -> f b -> f b
p **> q = p *> q

-- |'pAlphaNumeric' ~=> @[a-zA-Z0-9]@
pAlphaNumeric :: Parser Char
pAlphaNumeric = pLower <|> pUpper <|> pDigit

-- |'pSpace' ~=> @[ \\r\\n\\t]@
pSpace :: Parser Char
pSpace = pAnySym " \r\n\t" <?> "<single-whitespace>"

-- |'pSpaces1' ~=> @[ \\r\\n\\t]+@
pSpaces1 :: Parser String
pSpaces1 =  pList1 pSpace

pString :: Parser String
pString = pList1 pAlphaNumeric

pLSquare, pRSquare :: Parser Char
pLSquare = pSym '['
pRSquare = pSym ']'

pLabel :: Parser Label
pLabel = pLSquare *> pSpaces *> pString <* pSpaces <* pRSquare

pLabels :: Parser [Label]
pLabels = (:) <$> pLSquare **> pSpaces **> pString <*> pSpaces **> pList (pSym ',' **> pSpaces **> pString <* pSpaces) <* pRSquare

-- Parse statements
pStat :: Parser Stat
pStat = const Skip <$> pToken "skip"
    <|> Assume <$> pToken "assume"  **> pSpaces1 **> pParens pExpr
    <|> Assign <$> pLhs <*> pSpaces **> pSym '=' **> pSpaces **> pExpr

pLhs :: Parser Lhs
pLhs =  LhsVar <$> pString
    <|> LhsArray <$> pString <*> pLSquare **> pExpr <* pRSquare 

opCodes :: [String]
opCodes = ["&&", "||", "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!="]

-- digit2Num converts a char to a num.
digit2Num :: Num a => Char -> a
digit2Num a = fromInteger $ toInteger $ ord a - ord '0'

-- | 'pNumeralStr' ~=> @(0|([1-9][0-9]*))@ 
pNumeralStr :: Parser String
pNumeralStr =  pList pDigit

-- | 'pSNumeral' converts a string form of a <numeral> into a Num @a@.
pSNumeral :: Num a => Parser a
pSNumeral = foldl (\a b -> a * 10 + (digit2Num b)) 0 <$> pNumeralStr <?> "<numeral>"

toOp :: String -> OpCode
toOp s = case s of
  "&&" -> And
  "||" -> Or
  "+" -> Add
  "-" -> Sub
  "*" -> Mult
  "/" -> Div
  "%" -> Mod
  "<" -> Le
  "<=" -> Leq
  ">" -> Ge
  ">=" -> Geq
  "==" -> Eq
  "!=" -> Neq
  
pExpr :: Parser Expr
pExpr =  (\lhs op rhs -> Op lhs (toOp op) rhs) <$> pExpr <*> pSpaces **> pAny pToken opCodes <*> pSpaces **> pExpr 
     <|> C <$> pSNumeral
     <|> V <$> pString
     <|> A <$> pString <*> pLSquare **> pExpr <* pRSquare 
     <|> F <$> pString <*> pParens (pList (pExpr <* pSym ','))  -- fix this one

pProgLine :: Parser (Label, (Stat, [Label]))
pProgLine = (\a b c -> (a, (b,c))) <$> pLabel <*> pSpaces **> pStat <*> pSpaces **> pLabels

{-
Program Syntax:
  [label] -- initial label
  [label] Statement [label, label]
  [label] -- final label
-}
pProg :: Parser Program 
pProg = (\a b c -> (a, M.fromList b, c)) <$> pLabel <*> pSpaces **> pList pProgLine <*> pSpaces **> pLabels 

{-
   [label] |->
      Program
-}
pEditProg :: Parser (Label, Program)
pEditProg = (,) <$> pLabel <*> pSpaces **> pToken "|->" **> pSpaces **> pProg

pEdit :: Parser Edit
pEdit = M.fromList <$> pList pEditProg

parseProg :: String -> Program 
parseProg = runParser "Error" pProg

parseEdit :: String -> Edit
parseEdit = runParser "Error" pEdit
