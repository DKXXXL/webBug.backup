
import Regscr
--import WebBug3
import WebBug4 (webBug)
import WBData (output, queue, inPut, outPut)
import Control.Monad.State
--import System.Posix.Files
{-

historyfile = "history"
queuefile  = "queue"
outputfile = "output"
rulefile   = "rule"

-}

--specialRequest :: String -> Maybe String
--t2 :: ([a] -> [Maybe a]) -> ([a] -> [a]) 
reGenerate :: [String] -> IO Maybe String
reGenerate [] = outPut queue
reGenerate (a:[]) = return $ Just a
reGenerate (a:l) = do result <- inPut queue a
                      reGenerate l

validData :: (String,String) -> IO ()
validData ("",_) = return ()


main = webBug reGenerate validData "" 
















{-
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


-}
