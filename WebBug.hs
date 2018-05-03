module Main where
import Regscr 
--import WebBug3
import WebBug3 (getWebContext)
import Control.Monad.State

--import System.Posix.Files


historyfile = "history"
queuefile  = "queue"
outputfile = "output"
rulefile   = "rule"
nextweb = "nextweb"



type History = String
type QueueAddr = String
type Output = String
type FileName = String
type Recorder a = a -> IO ()

webrule :: IO ((Tag, Content) -> [Content])
webrule = fmap extractionsGenerator $ readFile nextweb

extractOutput :: IO ((Tag, Content) -> [Content])
extractOutput = fmap extractionsGenerator $ readFile rulefile

outputALine :: String -> IO ()
outputALine = appendFile outputfile . (++ "\n")

outputSeveralLines :: [String] -> IO ()
outputSeveralLines x =  (mapM outputALine x) >> (return () )


data Buffer s = Buffer 
  {
    saver :: s -> IO (),
    cache :: [s],
    cleaner ::  IO ()
  }

bufferCons :: FileName -> IO (Buffer String)
bufferCons filename = 
  do content <- readFile filename
     length content `seq`
       return $ Buffer ((appendFile filename) . (++ "\n")) (lines content) (writeFile filename "")

save :: s -> (Buffer s) -> IO (Buffer s)
save x buf = (saver buf x) >> (return $ buf {cache = (cache buf) ++ [x]})

ssave :: [s] -> Buffer s -> IO (Buffer s)
ssave xl = foldr (<=<) (return) $ map (\x -> save x) $ xl

clean :: (Buffer s) -> IO (Buffer s)
clean buf = (cleaner buf) >> (return $ buf {cache = []})

srewrite :: [s] -> Buffer s -> IO (Buffer s)
srewrite xl buf = clean buf >>= (\b -> ssave xl b) 

debugInfo :: QueueAddr -> Content  -> IO ()
debugInfo q x = putStrLn q 


historyFilter' :: History -> ([Content] -> [Content])
historyFilter' x = filter (/= x) 

historyFilter :: [History] -> ([Content] -> [Content])
historyFilter xl = foldr (.) (\x -> x) $ map historyFilter' xl

data BackGrd = BackGrd 
  {
    historyBuffer :: Buffer History,
    queueBuffer :: Buffer QueueAddr,
    outputL :: Recorder [Output]
    }

type Bugstate = StateT BackGrd IO

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a:_) = Just a

-- main idea of webbug
webbug :: Bugstate ()
webbug = do 
      qB <- gets $ queueBuffer
      hB <- gets $ historyBuffer
      let webqueue = cache qB
      let past = cache hB
      case webqueue of [] -> return ()
                       (nextwebaddr:nextallother) -> 
                                  do
                                  
                                  webContent <- lift $ getWebContext nextwebaddr
                                  lift $ debugInfo nextwebaddr webContent
                                  allnewwebs <- lift $ webrule <*> return (nextwebaddr, webContent)
                                  alloutPut <- lift $ extractOutput <*> return (nextwebaddr, webContent)
                                  let newwebqueue = historyFilter (nextwebaddr:past) (allnewwebs ++ nextallother)
                                  op <- gets $ outputL
                                  lift $ op alloutPut
                                  nhB <- lift $ save nextwebaddr hB
                                  nqB <- lift $ srewrite newwebqueue qB
                                  modify $ \s -> s {historyBuffer = nhB, queueBuffer = nqB}
                                  webbug


initialBackground :: IO BackGrd
initialBackground = 
  do history <- bufferCons historyfile
     queue <- bufferCons queuefile
     return (BackGrd history queue outputSeveralLines)
     
    



main :: IO ()
main = do init <- initialBackground
          evalStateT webbug init
          return ()




          




-- --specialRequest :: String -> Maybe String
-- --t2 :: ([a] -> [Maybe a]) -> ([a] -> [a]) 

-- reGenerate :: [String] -> IO (Maybe String)
-- reGenerate [] = outPut queue
-- reGenerate (a:[]) = return $ Just a
-- reGenerate (a:l) = do result <- inPut queue a
--                       reGenerate l

-- regrule = ruleScript $ alloutPut rule

-- validData :: (String,String) -> IO ()
-- validData ("",_) = return ()
-- validData x = allinPut output $ regApply regrule x 


-- main = webBug reGenerate validData "" 
















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
