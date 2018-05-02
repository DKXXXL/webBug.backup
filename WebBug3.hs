
module WebBug3
       (
      getWebContext
       ) where
         

import Network.URI
import Network.HTTP
import Control.Monad.State
import Control.Exception
import Data.Maybe
import Data.Char
import Data.Ord
import Data.List (group, foldl')
import Data.Sequence (unstableSort)



getWebContext :: String ->IO String
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
