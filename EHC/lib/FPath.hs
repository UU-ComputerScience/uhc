module FPath 
  (FPath(..), fpathToStr, fpathIsEmpty, fpathSetBase, fpathSetSuff, fpathRemoveSuff, mkFPath, emptyFPath
  , fpathSetDir, fpathPrependDir, mkTopLevelFPath, mkInitSearchPath)
where

-------------------------------------------------------------------------------------------
-- File path utils
-------------------------------------------------------------------------------------------

data FPath
  = FPath
      { fpathMbDir      :: Maybe String
      , fpathBase       ::       String
      , fpathMbSuff     :: Maybe String
      }
    deriving (Show,Eq,Ord)

emptyFPath :: FPath
emptyFPath
  = mkFPath ""

fpathIsEmpty :: FPath -> Bool
fpathIsEmpty fp = null (fpathBase fp)

fpathToStr :: FPath -> String
fpathToStr fpath
  = let adds f = maybe f (\s -> f ++ "." ++ s) (fpathMbSuff fpath)
        addd f = maybe f (\d -> d ++ "/" ++ f) (fpathMbDir fpath)
     in addd . adds . fpathBase $ fpath

fpathSetBase :: String -> FPath -> FPath
fpathSetBase s fp
  = fp {fpathBase = s}

fpathSetSuff :: String -> FPath -> FPath
fpathSetSuff "" fp
  = fpathRemoveSuff fp
fpathSetSuff s fp
  = fp {fpathMbSuff = Just s}

fpathSetDir :: String -> FPath -> FPath
fpathSetDir "" fp
  = fpathRemoveDir fp
fpathSetDir d fp
  = fp {fpathMbDir = Just d}

fpathPrependDir :: String -> FPath -> FPath
fpathPrependDir "" fp
  = fp
fpathPrependDir d fp
  = maybe (fpathSetDir d fp) (\fd -> fpathSetDir (d ++ "/" ++ fd) fp) (fpathMbDir fp)

fpathRemoveSuff :: FPath -> FPath
fpathRemoveSuff fp
  = fp {fpathMbSuff = Nothing}

fpathRemoveDir :: FPath -> FPath
fpathRemoveDir fp
  = fp {fpathMbDir = Nothing}

splitOnLast :: Char -> String -> Maybe (String,String)
splitOnLast splitch fn
  = case fn of
      ""     -> Nothing
      (f:fs) -> let rem = splitOnLast splitch fs
                 in if f == splitch
                    then maybe (Just ("",fs)) (\(p,s)->Just (f:p,s)) rem
                    else maybe Nothing (\(p,s)->Just (f:p,s)) rem

mkFPath :: String -> FPath
mkFPath fn
  = let (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast '/' fn)
        (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)
     in FPath d b' s

fpathSplit :: String -> (String,String)
fpathSplit fn
  = let (d,b)  = maybe ("",fn) id (splitOnLast '/' fn)
        (b',s) = maybe (b,"") id (splitOnLast '.' b)
        b''    = if null d then b' else d ++ "/" ++ b'
     in (b'',s)

mkTopLevelFPath :: String -> String -> FPath
mkTopLevelFPath suff fn
  = let fpNoSuff = mkFPath fn
     in maybe (fpathSetSuff suff fpNoSuff) (const fpNoSuff) . fpathMbSuff $ fpNoSuff

mkInitSearchPath :: FPath -> [String]
mkInitSearchPath fp = maybe [] (:[]) (fpathMbDir fp) ++ [""]


{-
data FPath
  = FPath
      { fpathMbDir      :: Maybe String
      , fpathBase       ::       String
      , fpathMbSuff     :: Maybe String
      }
    deriving (Show,Eq,Ord)

emptyFPath :: FPath
emptyFPath = mkFPath ""

fpathIsEmpty :: FPath -> Bool
fpathIsEmpty fp = null (fpathBase fp)

splitOnLast :: Char -> String -> Maybe (String,String)
splitOnLast splitch fn
  = case fn of
      ""     -> Nothing
      (f:fs) -> let rem = splitOnLast splitch fs
                 in if f == splitch
                    then maybe (Just ("",fs)) (\(p,s)->Just (f:p,s)) rem
                    else maybe Nothing (\(p,s)->Just (f:p,s)) rem

mkFPath :: String -> FPath
mkFPath fn
  = let (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast '/' fn)
        (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)
     in FPath d b' s

fpathRemoveSuff :: FPath -> FPath
fpathRemoveSuff fp
  = fp {fpathMbSuff = Nothing}

fpathToStr :: FPath -> String
fpathToStr fpath
  = let adds f = maybe f (\s -> f ++ "." ++ s) (fpathMbSuff fpath)
        addd f = maybe f (\d -> d ++ "/" ++ f) (fpathMbDir fpath)
     in addd . adds . fpathBase $ fpath
-}