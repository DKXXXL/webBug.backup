
import Regscr
import WebBug3
import Control.Monad.State
--import System.Posix.Files

historyfile = "history"
queuefile  = "queue"
outputfile = "output"
rulefile   = "rule"

inits = do a <- initFile historyfile
           b <- initFile queuefile
           c <- initFile outputfile
           initFile rulefile
  where initFile x = appendFile x ""

--regrule = ruleScript (lines $ readFile rulefile)
f :: (String,String) -> IO ((String,String))

f x = do h <- readFile rulefile
         appendFile outputfile $ show .(\n -> regApply n x).ruleScript $ lines h
         return x
           
main = do inits
          startBug queuefile f
