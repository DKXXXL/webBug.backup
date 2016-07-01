newtype FBase = String


history = "history"
queue  = "queue"
output = "output"
rule   = "rule"


inPut :: FBase -> String -> IO ()


outPut :: FBase -> IO (Maybe String)


nofind :: FBase -> String -> IO String
