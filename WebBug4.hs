import WBData (rule ,history, inPut, outPut, alloutPut, nofind)
import RegScr

webGet :: String -> IO String
webGet = getWebContext
  where getWebContext :: String -> IO String
        getWebContext web =do
          print web
          t <- try (downloadURL web) :: IO (Either SomeException [Char])
          case t of (Left sth) -> return ""
                    (Right sth) -> return sth
            where downloadURL url =
                    do resp <- simpleHTTP request
                       case resp of
                         Left x ->  return ""
                         Right r -> return $ rspBody r 
                         where request = Request {rqURI = uri,
                                                  rqMethod = GET,
                                                  rqHeaders = [],
                                                  rqBody = ""}
                               uri = fromJust $ parseURI url

noRepeat :: String -> IO String
noRepeat = nofind history


nextWeb :: (String,String) -> IO [String]
nextWeb x = liftM (( \n -> regApply n x) . ruleScript) (alloutPut rule)  
                
webBug' :: ([String] ->IO (Maybe String)) ->
           ((String,String) -> IO ()) ->
           (String -> IO [String]) ->
           (String -> IO String) ->
           String ->
           IO String
webBug' reGenerator output nextweb' webGet' initial =
  do webcontext' <- webGet' initial
     case webcontext' of Nothing -> return ""
                         (Just webcontext) -> do
                           nextstep <- nextweb' (initial, webcontext)
                           result <- output (initial, webcontext)
                           nextinit <- reGenerator nextstep
                           webBug' reGenerator output nextweb' webGet' nextinit


t1 :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
t1 f g = \x -> ((f x) >>= g)

webBug :: ([String] -> IO String) ->
          (String -> IO ()) ->
          String ->
          IO String
webBug x y z = webBug' x y nextweb (noRepeat `t1` webGet) z 


