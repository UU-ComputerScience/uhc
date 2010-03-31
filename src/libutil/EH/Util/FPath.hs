module EH.Util.FPath 
  ( FPath(..), fpathSuff
  , FPATH(..)
  , FPathError -- (..)
  , emptyFPath
  -- , mkFPath
  , fpathFromStr
  , mkFPathFromDirsFile
  , fpathToStr, fpathIsEmpty
  , fpathSetBase, fpathSetSuff, fpathSetDir
  , fpathUpdBase
  , fpathRemoveSuff, fpathRemoveDir
  
  , fpathIsAbsolute

  , fpathAppendDir, fpathUnAppendDir
  , fpathPrependDir, fpathUnPrependDir
  , fpathSplitDirBy
  , mkTopLevelFPath

  , fpathDirSep, fpathDirSepChar
  
  , fpathOpenOrStdin, openFPath
  
  , SearchPath
  , FileSuffixes, FileSuffix
  , mkInitSearchPath, searchPathFromFPath, searchPathFromFPaths
  , searchPathFromString
  , searchFPathFromLoc
  , searchLocationsForReadableFiles, searchPathForReadableFiles, searchPathForReadableFile
  
  , fpathEnsureExists
  
  , filePathMkPrefix, filePathUnPrefix
  , filePathCoalesceSeparator
  , filePathMkAbsolute, filePathUnAbsolute
  )
where

import IO
import Data.Maybe
import Data.List
import Control.Monad
import System.IO
import System.Directory

import EH.Util.Utils

-------------------------------------------------------------------------------------------
-- Making prefix and inverse, where a prefix has a tailing '/'
-------------------------------------------------------------------------------------------

filePathMkPrefix :: String -> String
filePathMkPrefix d@(_:_) | last d /= '/'    = d ++ "/"
filePathMkPrefix d                          = d

filePathUnPrefix :: String -> String
filePathUnPrefix d | isJust il && l == '/'  = filePathUnPrefix i
  where il = initlast d
        (i,l) = fromJust il
filePathUnPrefix d                          = d

filePathCoalesceSeparator :: String -> String
filePathCoalesceSeparator ('/':d@('/':_)) = filePathCoalesceSeparator d
filePathCoalesceSeparator (c:d) = c : filePathCoalesceSeparator d
filePathCoalesceSeparator d     = d

-------------------------------------------------------------------------------------------
-- Making into absolute path and inverse, where absolute means a heading '/'
-------------------------------------------------------------------------------------------

filePathMkAbsolute :: String -> String
filePathMkAbsolute d@('/':_ ) = d
filePathMkAbsolute d          = "/" ++ d

filePathUnAbsolute :: String -> String
filePathUnAbsolute d@('/':d') = filePathUnAbsolute d'
filePathUnAbsolute d          = d

-------------------------------------------------------------------------------------------
-- File path
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

-------------------------------------------------------------------------------------------
-- Observations
-------------------------------------------------------------------------------------------

-- TBD. does not work under WinXX, use FilePath library
fpathIsAbsolute :: FPath -> Bool
fpathIsAbsolute fp
  = case fpathMbDir fp of
      Just ('/':_) -> True
      _            -> False

-------------------------------------------------------------------------------------------
-- Utilities, (de)construction
-------------------------------------------------------------------------------------------

fpathFromStr :: String -> FPath
fpathFromStr fn
  = FPath d b' s
  where (d ,b) = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,Nothing)  (\(b,s) -> (b,Just s)) (splitOnLast '.'             b )

fpathDirFromStr :: String -> FPath
fpathDirFromStr d = emptyFPath {fpathMbDir = Just d}

fpathSuff :: FPath -> String
fpathSuff = maybe "" id . fpathMbSuff

fpathSetBase :: String -> FPath -> FPath
fpathSetBase s fp
  = fp {fpathBase = s}

fpathUpdBase :: (String -> String) -> FPath -> FPath
fpathUpdBase u fp
  = fp {fpathBase = u (fpathBase fp)}

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

fpathSplitDirBy :: String -> FPath -> Maybe (String,String)
fpathSplitDirBy byDir fp
  = do { d      <- fpathMbDir fp
       ; dstrip <- stripPrefix byDir' d
       ; return (byDir',filePathUnAbsolute dstrip)
       }
  where byDir' = filePathUnPrefix byDir

fpathPrependDir :: String -> FPath -> FPath
fpathPrependDir "" fp
  = fp
fpathPrependDir d fp
  = maybe (fpathSetDir d fp) (\fd -> fpathSetDir (d ++ fpathDirSep ++ fd) fp) (fpathMbDir fp)

fpathUnPrependDir :: String -> FPath -> FPath
fpathUnPrependDir d fp
  = case fpathSplitDirBy d fp of
      Just (_,d) -> fpathSetDir d fp
      _          -> fp

fpathAppendDir :: FPath -> String -> FPath
fpathAppendDir fp ""
  = fp
fpathAppendDir fp d
  = maybe (fpathSetDir d fp) (\fd -> fpathSetDir (fd ++ fpathDirSep ++ d) fp) (fpathMbDir fp)

-- remove common trailing part of dir
fpathUnAppendDir :: FPath -> String -> FPath
fpathUnAppendDir fp ""
  = fp
fpathUnAppendDir fp d
  = case fpathMbDir fp of
      Just p -> fpathSetDir (filePathUnPrefix prefix) fp
             where (prefix,_) = splitAt (length p - length d) p
      _      -> fp

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

mkFPathFromDirsFile :: Show s => [s] -> s -> FPath
mkFPathFromDirsFile dirs f
  = fpathSetDir (concat $ intersperse fpathDirSep $ map show $ dirs) (mkFPath (show f))

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
  mkFPath = fpathFromStr

instance FPATH FPath where
  mkFPath = id

-------------------------------------------------------------------------------------------
-- Class 'is error related to FPath'
-------------------------------------------------------------------------------------------

class FPathError e

instance FPathError String

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

openFPath :: FPath -> IOMode -> Bool -> IO (String, Handle)
openFPath fp mode binary
  | fpathIsEmpty fp = case mode of
                        ReadMode      -> return ("<stdin>" ,stdin )
                        WriteMode     -> return ("<stdout>",stdout)
                        AppendMode    -> return ("<stdout>",stdout)
                        ReadWriteMode -> error "cannot use stdin/stdout with random access"
  | otherwise       = do { let fNm = fpathToStr fp
                         ; h <- if binary
                                then openBinaryFile fNm mode
                                else openFile fNm mode
                         ; return (fNm,h)
                         }

-------------------------------------------------------------------------------------------
-- Directory
-------------------------------------------------------------------------------------------

fpathEnsureExists :: FPath -> IO ()
fpathEnsureExists fp
  = do { let d = fpathMbDir fp
       ; when (isJust d) (createDirectoryIfMissing True (fromJust d))
       }

-------------------------------------------------------------------------------------------
-- Search path utils
-------------------------------------------------------------------------------------------

type SearchPath = [String]
type FileSuffix   =  Maybe String
type FileSuffixes = [FileSuffix]

searchPathFromFPaths :: [FPath] -> SearchPath
searchPathFromFPaths fpL = nub [ d | (Just d) <- map fpathMbDir fpL ] ++ [""]

searchPathFromFPath :: FPath -> SearchPath
searchPathFromFPath fp = searchPathFromFPaths [fp]

mkInitSearchPath :: FPath -> SearchPath
mkInitSearchPath = searchPathFromFPath

searchPathFromString :: String -> [String]
searchPathFromString
  = unfoldr f
  where f "" = Nothing
        f sp = Just (break (== ';') sp)

-- Simple function that returns a particular file under a
-- certain root dir.
searchFPathFromLoc :: FilePath -> FPath -> [(FilePath,FPath)]
searchFPathFromLoc loc fp = [(loc,fpathPrependDir loc fp)]

searchLocationsForReadableFiles :: FPathError e => (loc -> FPath -> [(loc,FPath,[e])]) -> Bool -> [loc] -> FileSuffixes -> FPath -> IO [(FPath,loc,[e])]
searchLocationsForReadableFiles getfp stopAtFirst locs suffs fp
  = let select stop f fps
          = foldM chk [] fps
          where chk r fp
                  = case r of
                      (_:_) | stop -> return r
                      _            -> do r' <- f fp
                                         return (r ++ r')
        tryToOpen loc mbSuff fp
          = do { let fp' = maybe fp (\suff -> fpathSetNonEmptySuff suff fp) mbSuff
               ; fExists <- doesFileExist (fpathToStr fp')
               -- ; hPutStrLn stderr (show fp ++ " - " ++ show fp')
               ; if fExists
                 then return [(fp',loc)]
                 else return []
               }
        tryToOpenWithSuffs suffs (loc,fp,x)
          = fmap (map (tup12to123 x)) $ 
            case suffs of
              [] -> tryToOpen loc Nothing fp
              _  -> select stopAtFirst
                      (\(ms,f) -> tryToOpen loc ms f)
                      ({- (Nothing,fp) : -} zipWith (\s f -> (s,f)) suffs (repeat fp))
        tryToOpenInDir loc
          = select True (tryToOpenWithSuffs suffs) (getfp loc fp)
     in select True tryToOpenInDir locs

searchPathForReadableFiles :: Bool -> SearchPath -> FileSuffixes -> FPath -> IO [FPath]
searchPathForReadableFiles stopAtFirst locs suffs fp
  = fmap (map tup123to1) $ searchLocationsForReadableFiles (\s f -> map (tup12to123 ([]::[String])) $ searchFPathFromLoc s f) stopAtFirst locs suffs fp

searchPathForReadableFile :: SearchPath -> FileSuffixes -> FPath -> IO (Maybe FPath)
searchPathForReadableFile paths suffs fp
  = do fs <- searchPathForReadableFiles True paths suffs fp
       return (listToMaybe fs)

