module Top.Util.IntErr where

internalError :: String -> String -> String -> a
internalError moduleName functionName message 
   =  error . unlines $
           [ ""
           , "INTERNAL ERROR - " ++ message
           , "** Module   : " ++ moduleName
           , "** Function : " ++ functionName
           ]
           
