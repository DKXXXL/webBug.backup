module Regscr where

import Data.List
import Data.List.Split
import Text.Regex.Posix

regnextsym = ">>>>"
regseriessym = ":::"

type Rule = String -> Bool
type Tag = String
type Content = String
type Extraction = (Tag, Content) -> [Content]
type RExtract = Content -> [Content]



bindRule :: Rule -> Rule -> Rule 
bindRule f g = \x -> (f x) && (g x)
    
-- ruleGenerator will generate a rule according to regular expression
ruleGenerator :: String -> Rule 
ruleGenerator x = (=~ x)

-- rulesGenerator will generate a rule according to regular expression and '>>>>'
rulesGenerator :: String -> Rule
rulesGenerator regexps = foldr1 bindRule alltherule
  where groupOfRegexp = splitOn regnextsym regexps
        alltherule = map ruleGenerator groupOfRegexp

bindExtract :: RExtract -> RExtract -> RExtract
bindExtract f g = \x -> (f x >>= g)

extractGenerator :: String -> RExtract
extractGenerator regexp content = allpattern content
  where allpattern :: String -> [String]
        allpattern remains = 
          case ((remains =~ regexp)::(String, String, String)) of (_, "", _) -> []
                                                                  (_, new, left) -> new:(allpattern left)

extractsGenerator :: String -> RExtract
extractsGenerator regexp = foldr1 bindExtract alltheextract
  where groupOfExt = splitOn regnextsym regexp
        alltheextract = map extractGenerator groupOfExt

bindExtraction :: Extraction -> Extraction -> Extraction
bindExtraction f g = \x -> nonempty (f x) (g x)
  where nonempty [] h = h 
        nonempty g _ = g


extractionGenerator :: String -> Extraction
extractionGenerator patternAndcorr = 
  \(tag, ctx) -> if (patternrule tag) then filter ctx else []
  where (pattern, _ , rules) = (patternAndcorr =~ regseriessym) :: (String, String, String)
        patternrule = rulesGenerator pattern
        filter = extractsGenerator rules

extractionsGenerator :: String -> Extraction
extractionsGenerator = foldr1 bindExtraction . map extractionGenerator . lines




