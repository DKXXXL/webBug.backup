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
type Recorder a = a -> IO ()

webrule :: IO ((Tag, Content) -> [Content])
webrule = fmap extractionsGenerator $ readFile nextweb

extractOutput :: IO ((Tag, Content) -> [Content])
extractOutput = fmap extractionsGenerator $ readFile rulefile

outputALine :: String -> IO ()
outputALine = writeFile outputfile . (++ "\n")

outputSeveralLines :: [String] -> IO ()
outputSeveralLines x =  (mapM outputALine x) >> (return () )


data Buffer s = Buffer 
  {
    saver :: s -> IO (),
    cache :: [s]
  }

save :: s -> (Buffer s) -> IO (Buffer s)
save x buf = (saver buf x) >> (return $ buf {cache = x: (cache buf)})

ssave :: [s] -> Buffer s -> IO (Buffer s)
ssave xl = foldr1 (<=<) . map (\x -> save x) $ xl


historyFilter' :: History -> ([Content] -> [Content])
historyFilter' x = filter (/= x) 

historyFilter :: [History] -> ([Content] -> [Content])
historyFilter xl = foldr1 (.) $ map historyFilter' xl

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
      let allthewebs = cache qB
      let past = cache hB
      case head' allthewebs of Nothing -> return ()
                               (Just nextwebaddr) -> 
                                  do
                                  lift $ putStrLn nextwebaddr
                                  webContent <- lift $ getWebContext nextwebaddr
                                  allnewwebs <- lift $ webrule <*> return (nextwebaddr, webContent)
                                  alloutPut <- lift $ extractOutput <*> return (nextwebaddr, webContent)
                                  let allnewwebs' = historyFilter (nextwebaddr:past) allnewwebs
                                  op <- gets $ outputL
                                  lift $ op alloutPut
                                  nhB <- lift $ save nextwebaddr hB
                                  nqB <- lift $ ssave allnewwebs' qB
                                  modify $ \s -> s {historyBuffer = nhB, queueBuffer = nqB}
                                  webbug


initialBackground :: IO BackGrd
initialBackground = 
  do historycontent <- fmap lines $ readFile historyfile 
     queuecontent <- fmap lines $ readFile queuefile
     return (BackGrd (Buffer historyRecord historycontent)
                      (Buffer queueRecord queuecontent)
                      outputSeveralLines)
  where historyRecord = writeFile historyfile . (++ "\n")
        queueRecord = writeFile queuefile . (++ "\n")
     
    



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
