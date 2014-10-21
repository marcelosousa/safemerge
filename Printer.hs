module Printer where

import Language

instance Show Program where
    show (Program (decls,defs)) = 
        let decls' = foldr (\d r -> show d ++ "\n" ++ r) "" decls 
            defs'  = foldr (\d r -> show d ++ "\n" ++ r) "" defs
        in decls' ++ defs'


instance Show Definition where
    show (FunctionDef _ fn params stat) = 
      fn ++ "(" ++ show params ++ "){\n" ++ show stat ++ "\n}"

instance Show a => Show (AnnStatement a) where
    show (Assign _ var e) = show var ++ " := " ++ show e ++ ";"
    show (Local  _ var Nothing) = show var ++ ";" 
    show (Local  _ var (Just v)) = show var ++ " := " ++ show v ++ ";"
    show (Sequence s1 s2) = show s1 ++ "\n" ++ show s2
    show (IfThen _ c t) = 
        let th = unlines $ map ("  "++) $ lines $ show t
        in "if(" ++ show c ++ "){\n" ++ th ++ "}"
    show (If _ c t e) = 
        let th = unlines $ map ("  "++) $ lines $ show t
            eh = unlines $ map ("  "++) $ lines $ show e
        in "if(" ++ show c ++ "){\n" ++ th ++ "}else{\n" ++ eh ++ "}"
    show (While _ c s) = "while(" ++ show c ++ "){\n" ++ show s ++ "\n}"
    show (Return _ e) = "return " ++ show e ++ ";"
    show (CallS _ fn args) = fn ++ "(" ++ show args ++ ");"

instance Show Expression where
    show (Call fn args) = fn ++ "(" ++ foldr (\b r -> show b ++ "," ++ r) ")" args
    show (BinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2 
    show (UnaryOp op e) = show e ++ " " ++ show op
    show (Const v) = show v
    show (Ident x) = x                           -- x
    show (Index lhs rhs) = show lhs ++ "[" ++ show rhs ++ "]"

instance Show Value where
    show (IntValue i) = show i
    show (FloatValue f) = show f
    show (StrValue s) = s
    
instance Show Declaration where
    show (FunctionDecl _ fn params) = fn ++ "(" ++ show params ++ ");"
    show (GlobalDecl _ i Nothing) = i ++ ";\n"
    show (GlobalDecl _ i (Just v)) = i ++ " = " ++ show v ++ ";\n"
    
    