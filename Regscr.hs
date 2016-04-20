module Regscr
       (
         regApply,
         ruleScript
       )
       where

import Data.List
import Text.Regex.Posix
regnextsym = ">>>>"
regseriessym = ":::"
{-
regEval :: [String] -> String ->[String]
regEval source reg = regEval' source (regrize reg []) 
  where regEval' :: [String] ->[String] -> [String]
        regEval' source [] = source
        regEval' source (treg:e) =
          regEval' (unlines . flatAndmonoize $ map (=~ treg ) source) e
        monoize ::(Eq x) => [x] -> [x]
        monoize = (map head).group
        flatAndmonoize = monoize . concat
        regnextsym =">>>>"  --spacebar is in the consideration
        regrize :: String -> [String] -> [String]
        regrize "" x = x
        regrize str x =
          let (a, _ ,b) = (str =~regnextsym) :: (String,String,String)
          in regrize b (a : x)
-}
regrize :: String -> [String]
regrize regSource = regrize' regSource
  where regrize' :: String -> [String]
        regrize' "" = []
        regrize' str =
          let (a, _ ,b) = (str =~ regnextsym) :: (String, String, String)
          in  (a: (regrize' b))

regApply :: [(String,[String])] -> (String,String) -> String
regApply rules (iweb,input) = apply'  (snd . head $ filter (\(x,_) -> x =~ iweb || x == "default") rules) input
  where apply' :: [String] -> String -> String
        apply' [] input = input
        apply' (procedure:others)  input = apply' others. unlines . monoize . beam $ input =~ procedure
        monoize :: (Eq x) => [x] -> [x]
        monoize = (map head).group
        beam :: [[x]] -> [x]
        beam [] = []
        beam (a:l) = a ++ (beam l)
   
ruleScript :: [String] -> [(String,[String])]
ruleScript rule = evalRule rule []
  where evalRule :: [String] -> [(String,[String])] -> [(String,[String])]
        evalRule [] ret = ret
        evalRule (x:origin) ret = let (a, _ ,b) = (x =~ regseriessym) :: (String ,String ,String)
                                  in evalRule origin ((a,(regrize b)):ret)
