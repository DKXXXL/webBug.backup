newtype FBase = String


history = "history"
queue  = "queue"
output = "output"
rule   = "rule"


inPut :: FBase -> String -> IO ()


outPut :: FBase -> IO (Maybe String)


nofind :: FBase -> String -> IO String


allinPut :: FBase -> [String] -> IO ()
allinPut y [] = return ()
allinPut y (x:l) = do result <- inPut y x
                      allinPut y l

alloutPut :: FBase -> IO [String]
alloutPut y = liftM (foldr combine' []) alloutput 
  where alloutput :: IO [(Maybe String)]
        alloutput' = (outPut y) : alloutput'
        alloutput = t3 alloutput'
        combine' :: (Maybe String) -> [String] -> [String]
        combine' Nothing _ = []
        combine' (Just x) y = (x:y)


t3 :: [IO a] -> IO [a]
t3 (x':l) = x' >>= (\x -> return $ x : (t3 l))

