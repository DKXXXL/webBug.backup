
module WebBug3
       (
         startBug
         ,getWebContext
       ) where
         

import Regscr
import Network.URI
import Network.HTTP
import Control.Monad.State
import Control.Exception
import Data.Maybe
import Data.Char
import Data.Ord
import Data.List (group, foldl')
import Data.Sequence (unstableSort)


data FileBuffer a = FileBuffer { _inside :: [a] , _outside :: String , _trans :: String -> [a]} | NONE

returnBuffer ::(Show a) => [a] -> String -> (String -> [a]) -> IO (FileBuffer a)
returnBuffer xs _outside _trans =do (appendFile _outside (show xs))
                                    return (FileBuffer xs _outside _trans) 

addhistory :: [String] -> [String] ->IO [String]
addhistory xs history = do (appendFile (nextBuffname historyfile) $ show $ unlines xs)
                           return (xs ++ history)

nextBuffname :: String -> String
nextBuffname [] = []
nextBuffname (x:xs) = (nextBuffname' x) : (nextBuffname xs)
  where nextBuffname' 'z' = 'a'
        nextBuffname'  x  = chr . (+1) $ ord x
{-
instance Ord String where
  (a:x) < (b:y) = (a < b) || ((a == b) && (x < y))
  (a:x) < [] = False
  []    < (b:y) = True
  a <= b = (a < b) || (a == b)
  a > b = not (a <= b)
  a >= b = not (a < b)
-}
bugFetching ::FileBuffer String -> ((String,String) -> IO (String,String)) -> [String] -> IO b0
bugFetching input@(FileBuffer _in _out _trans) f history=
--  ((liftM (monoize .beam)).beam' $ map (\x -> liftM (historyfilter) (getWebContext x >>= (getNextWeb. f' .(\n -> (x,n))))) _in)
--  >>= (nextbugFetching $!)
  ((liftM (monoize . beam)) . beam' $ map (\x -> getWebContext x >>= (f'. (\n -> (x,n))) >>= getNextWeb >>= (return. historyfilter)) _in)
  >>= (nextbugFetching)

  where monoize :: [[Char]] -> [[Char]]
        monoize = map head . group -- . unstableSort
        beam ::[[a]] -> [a]
        beam [] = []
        beam (x:xs) = x ++ (beam xs)
        beam' ::[IO [a]] -> IO [[a]]
        beam' [] = return []
        beam' (x:xs) = x >>= (\x' -> ((beam' xs)>>= (\xs' -> return (x' : xs'))))
        nextbugFetching xs =
          ioCall'
          (\x y ->bugFetching x f y)
          (returnBuffer xs (_out) _trans)
          (addhistory xs history)
          --(addhistory xs history) 
        
        historyfilter :: [String] -> [String]
        historyfilter xs = filter (\p -> (foldl' (&&) True (map (\x ->not $ x == p) history))) xs
        f' = f 
        
startBug startwebfile f =
  ioCall'
  (\x y -> bugFetching x f y)
  startbuffer'
  history' 
  where startbuffer' = (readFile startwebfile) >>= ((\n -> returnBuffer n (nextBuffname startwebfile) lines). lines)
        history' = liftM lines $ readFile historyfile
ioCall' :: (a ->b -> IO c) -> IO a -> IO b -> IO c
ioCall' fio ioa iob = ioCall'' (liftM fio ioa) iob 
  where ioCall'' :: IO (b -> IO c) -> IO b -> IO c
        ioCall'' iof iob = iof >>= (\f -> iob >>= f)
historyfile = "history"
nextwebrule = "nextweb"


getNextWeb :: (String,String) ->IO [String]
getNextWeb x =liftM (lines .( \n -> regApply n x) . ruleScript .lines) (readFile nextwebrule)  
--  where nextwebrule' = ruleScript $ lines nextwebrule

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
