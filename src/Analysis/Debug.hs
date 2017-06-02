{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Debug
-- Copyright :  (c) 2017 Marcelo Sousa
-- Utilities to Debug the Analysis
-------------------------------------------------------------------------------
module Analysis.Debug (menuText, prompt) where

breaker :: String
breaker = "=========================================="
header  = "=============ANALYSER STATE==============="
subhead = "=================OPTIONS=================="
opt1    = "======= 1. Print current statement ======="
opt2    = "======= 2. Print edits             ======="
opt3    = "======= 3. Print program           ======="
opt4    = "======= 4. Print current VC        ======="
opt5    = "======= 5. Print SSA map           ======="
opt6    = "======= 6. Print full analyzer env ======="
opt7    = "======= c. Continue analysis       ======="
opt8    = "======= q. Exit                    ======="
prompt  = "======= Select Option: "

menuText :: String
menuText = unlines [breaker,header,subhead,opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8] 

