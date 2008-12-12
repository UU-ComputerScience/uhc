module EH.Util.FPath 
  ( FPath(..), fpathSuff
  , FPATH(..)
  , emptyFPath
  -- , mkFPath
  , fpathFromStr
  , mkFPathFromDirsFile
  , fpathToStr, fpathIsEmpty
  , fpathSetBase, fpathSetSuff, fpathSetDir
  , fpathRemoveSuff, fpathRemoveDir
  , fpathPrependDir, mkTopLevelFPath
  
  , fpathDirSep, fpathDirSepChar
  
  , fpathOpenOrStdin
  
  , SearchPath, FileSuffixes
  , mkInitSearchPath
  , searchPathFromString
  , searchPathForReadableFile
  )
where

import IO
import Maybe
import Data.List
import Control.Monad
import Directory

-------------------------------------------------------------------------------------------
-- File path utils
-------------------------------------------------------------------------------------------

data FPath
  = FPath
      { fpathMbDir      :: !(Maybe  String)
      , fpathBase       ::         !String
      , fpathMbSuff     :: !(Maybe  String)
      }
    deriving (Show,Eq,Ord)

emptyFPath :: FPath
emptyFPath
  = mkFPath ""

fpathIsEmpty :: FPath -> Bool
fpathIsEmpty fp = null (fpathBase fp)

fpathToStr :: FPath -> String
fpathToStr fpath
  = let adds f = maybe f (\s -> f ++ "."         ++ s) (fpathMbSuff fpath)
        addd f = maybe f (\d -> d ++ fpathDirSep ++ f) (fpathMbDir fpath)
     in addd . adds . fpathBase $ fpath

fpathFromStr :: String -> FPath
fpathFromStr fn
  = FPath d b' s
  where (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)

fpathSuff :: FPath -> String
fpathSuff = maybe "" id . fpathMbSuff

fpathSetBase :: String -> FPath -> FPath
fpathSetBase s fp
  = fp {fpathBase = s}

fpathSetSuff :: String -> FPath -> FPath
fpathSetSuff "" fp
  = fpathRemoveSuff fp
fpathSetSuff s fp
  = fp {fpathMbSuff = Just s}

fpathSetNonEmptySuff :: String -> FPath -> FPath
fpathSetNonEmptySuff "" fp
  = fp
fpathSetNonEmptySuff s fp
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
  = maybe (fpathSetDir d fp) (\fd -> fpathSetDir (d ++ fpathDirSep ++ fd) fp) (fpathMbDir fp)

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

{-
mkFPath :: String -> FPath
mkFPath fn
  = let (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)
     in FPath d b' s
-}

mkFPathFromDirsFile :: Show s => [s] -> s -> FPath
mkFPathFromDirsFile dirs f
  = fpathSetDir (concat $ intersperse fpathDirSep $ map show $ dirs) (mkFPath (show f))

{-
fpathSplit :: String -> (String,String)
fpathSplit fn
  = let (d,b)  = maybe ("",fn) id (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,"") id (splitOnLast '.' b)
        b''    = if null d then b' else d ++ fpathDirSep ++ b'
     in (b'',s)
-}

mkTopLevelFPath :: String -> String -> FPath
mkTopLevelFPath suff fn
  = let fpNoSuff = mkFPath fn
     in maybe (fpathSetSuff suff fpNoSuff) (const fpNoSuff) . fpathMbSuff $ fpNoSuff

-------------------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------------------

fpathDirSep :: String
fpathDirSep = "/"

fpathDirSepChar :: Char
fpathDirSepChar = head fpathDirSep

-------------------------------------------------------------------------------------------
-- Class 'can make FPath of ...'
-------------------------------------------------------------------------------------------

class FPATH f where
  mkFPath :: f -> FPath

instance FPATH String where
  mkFPath fn
    = FPath d b' s
    where (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast fpathDirSepChar fn)
          (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)

-------------------------------------------------------------------------------------------
-- Open path for read or return stdin
-------------------------------------------------------------------------------------------

fpathOpenOrStdin :: FPath -> IO (FPath,Handle)
fpathOpenOrStdin fp
  = if fpathIsEmpty fp
    then return (mkFPath "<stdin>",stdin)
    else do { let fn = fpathToStr fp
            ; h <- openFile fn ReadMode
            ; return (fp,h)
            }

-------------------------------------------------------------------------------------------
-- Search path utils
-------------------------------------------------------------------------------------------

type SearchPath = [String]
type FileSuffixes = [String]

mkInitSearchPath :: FPath -> SearchPath
mkInitSearchPath fp = maybe [] (:[]) (fpathMbDir fp) ++ [""]

searchPathFromString :: String -> [String]
searchPathFromString
  = unfoldr f
  where f "" = Nothing
        f sp = Just (break (== ';') sp)

searchPathForReadableFile :: SearchPath -> FileSuffixes -> FPath -> IO (Maybe FPath)
searchPathForReadableFile paths suffs fp
  = let select f fps
          = foldM chk Nothing fps
          where chk r fp
                  = case r of
                      Nothing -> f fp
                      Just _  -> return r
        tryToOpen mbSuff fp
          = do { let fp' = maybe fp (\suff -> fpathSetNonEmptySuff suff fp) mbSuff
               ; fExists <- doesFileExist (fpathToStr fp')
               -- ; hPutStrLn stderr (show fp ++ " - " ++ show fp')
               ; if fExists
                 then return (Just fp')
                 else return Nothing
               }
        tryToOpenWithSuffs suffs fp
          = case suffs of
              [] -> tryToOpen Nothing fp
              _  -> select (\(ms,f) -> tryToOpen ms f) ((Nothing,fp) : zipWith (\s f -> (Just s,f)) suffs (repeat fp))
        tryToOpenInDir dir
          = select (tryToOpenWithSuffs suffs) [fpathPrependDir dir fp,fpathSetDir dir fp]
     in select tryToOpenInDir paths
