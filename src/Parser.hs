{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Types

import Text.ParserCombinators.UU hiding (parse, (<$$>))
import Text.ParserCombinators.UU.Utils hiding (pLParen, pRParen)
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Demo.Examples (run)

pProg :: Parser Program
pProg = undefined

pEdit :: Parser Edit
pEdit = undefined
 
