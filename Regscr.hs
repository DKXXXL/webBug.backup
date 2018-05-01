module Regscr
       (
         regApply,
         ruleScript
       )
       where

import Data.List
import Data.List.Split
import Text.Regex.Posix

regnextsym = ">>>>"
regseriessym = ":::"

type Rule = String -> Bool
type Tag = String
type Content = String
type Extraction = (Tag, Content) -> [Content]
type Extract = Content -> [Content]


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

-- regrize :: String -> [String]
-- regrize regSource = regrize' regSource
--   where regrize' :: String -> [String]
--         regrize' "" = []
--         regrize' str =
--           let (a, _ ,b) = (str =~ regnextsym) :: (String, String, String)
--           in  (a: (regrize' b))

-- regApply :: [(String,[String])] -> (String,String) -> [String]
-- regApply rules (iweb,input) =
--   lines $ apply'  (snd . head $ filter (\(x,_) -> x =~ iweb || x == "default") rules) input
--   where apply' :: [String] -> String -> String
--         apply' [] input = input
--         apply' (procedure:others) input =
--           apply' others. unlines . monoize . beam $ input =~ procedure
--         monoize :: (Eq x) => [x] -> [x]
--         monoize = (map head).group
--         beam :: [[x]] -> [x]
--         beam [] = []
--         beam (a:l) = a ++ (beam l)
   
-- ruleScript :: [String] -> [(String,[String])]
-- ruleScript rule = evalRule rule []
--   where evalRule :: [String] -> [(String,[String])] -> [(String,[String])]
--         evalRule [] ret = ret
--         evalRule (x:origin) ret = let (a, _ ,b) = (x =~ regseriessym) :: (String ,String ,String)
--                                   in evalRule origin ((a,(regrize b)):ret)

bindRule :: Rule -> Rule -> Rule 
bindRule f g = \x -> (f x) && (g x)
    
-- ruleGenerator will generate a rule according to regular expression
ruleGenerator :: String -> Rule 
ruleGenerator = (=~)

-- rulesGenerator will generate a rule according to regular expression and '>>>>'
rulesGenerator :: String -> Rule
rulesGenerator regexps = foldr1 bindRule alltherule
  where groupOfRegexp = splitOn regnextsym regexps
        alltherule = map ruleGenerator groupOfRegexp

bindExtract :: Extract -> Extract -> Extract
bindExtract = (<=<)

extractGenerator :: String -> Extract
extractGenerator regexp content = allpattern content
  where allpattern remains = 
          case remains =~ regexp of (_, "", _) -> []
                                    (_, new, left) -> new:(allpattern left)

extractsGenerator :: String -> Extract
extractsGenerator regexp = foldr1 bindExtract alltheextract
  where groupOfExt = splitOn regseriessym regexp
        alltheextract = map extractGenerator groupOfExt

bindExtraction :: Extraction -> Extraction -> Extraction
bindExtraction f g = \x -> nonempty (f x) (g x)
  where nonempty [] h = h 
        nonempty g _ = g


extractionGenerator :: String -> Extraction
extractionGenerator patternAndcorr = 
  \(tag, ctx) -> if (patternrule tag) then filter ctx else []
  where (pattern, _ , rules) = patternAndcorr =~ regseriessym
        patternrule = ruleGenerator pattern
        filter = extractsGenerator rules


