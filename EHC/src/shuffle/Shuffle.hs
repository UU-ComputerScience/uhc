-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set(Set)
import System
import IO
import System.Console.GetOpt
import EH.Util.ParseUtils
import EH.Util.DependencyGraph
import EH.Util.FPath
import EH.Util.Pretty
import Common
import MainAG
import ChunkParser
import CDoc
import CDocSubst
import Data.Char

-------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------

type FPathWithAlias = (Maybe String,FPath)

main :: IO ()
main
  = do { args <- getArgs
       ; let oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
             opts           = foldr ($) defaultOpts o
       ; if optHelp opts
         then putStrLn (usageInfo "Usage shuffle [options] [file ([alias=]file)*|-]\n\noptions:" cmdLineOpts)
         else if null errs
              then  let (f,frest) = if null n then (emptyFPath,[]) else if head n == "-" then (emptyFPath,tail n) else (mkFPath (head n),tail n)
                    in  if optGenDeps opts
                        then genDeps f opts
                        else doCompile (Nothing,f) (map mkFPathAlias frest) opts
              else  putStr (head errs)
       }
  where mkFPathAlias s
          = case break (=='=') s of
              (a,('=':f)) -> (Just a ,mkFPath f)
              _           -> (Nothing,mkFPath s)

readShFile :: FPathWithAlias -> Opts -> IO (FPathWithAlias,T_AGItf)
readShFile (a,fp) opts
  = do { (fp,fh) <- fpathOpenOrStdin fp
       ; txt <- hGetContents fh
       ; let toks = scan shuffleScanOpts ScSkip txt
       ; let (pres,perrs) = parseToResMsgs pAGItf toks
       ; if null perrs
         then return ((a,fp),pres)
         else do { mapM_ (hPutStrLn stderr . show) perrs
                 ; exitFailure
                 }
       }

doCompile' :: FPath -> Opts -> IO Syn_AGItf
doCompile' f opts
  = do { ((_, fp), pres) <- readShFile (Nothing,f) opts
       ; return (wrapAG_T opts fp Set.empty Map.empty pres)
       }

doCompile :: FPathWithAlias -> [FPathWithAlias] -> Opts -> IO ()
doCompile fpa fpaRest opts
  = do { xrefExceptFileContent
           <- case optMbXRefExcept opts of
                Just f -> do c <- readFile f
                             return (Set.unions . map (Set.fromList . words) . lines $ c)
                Nothing -> return Set.empty
       ; allPRes@(((_,fp'),pres):restPRes) <- mapM (\f -> readShFile f opts) (fpa:fpaRest)
       ; let (nmChMp,hdL) = allNmChMpOf allPRes
             fb = fpathBase fp'
             res = wrapSem fp' xrefExceptFileContent Map.empty pres
             bld = selBld opts res
             topChNmS = Set.unions [ Set.map (mkFullNm fb) . Map.keysSet . bldNmChMp $ b | b <- bld ]
       ; subsChNmS <- putBld nmChMp bld
       ; let allChNmS = subsChNmS `Set.union` topChNmS
             hdL' = [ h | (n,h) <- hdL, n `Set.member` allChNmS ]
       ; putHideBld opts fb nmChMp hdL'
       }
  where mkFullNm b n = mkNm b `nmApd` n
        selBld opts res
          = if optAG opts then bldAG_Syn_AGItf res
            else if optHS opts || optPlain opts then bldHS_Syn_AGItf res
            else bldLaTeX_Syn_AGItf res
        putErrs em
          = if Map.null em
            then return ()
            else do { mapM_ (\e -> hPutPPFile stderr (pp e) 80) . Map.elems $ em
                    ; exitFailure
                    }
        putBld nmChMp b
          = if not (null b)
            then do { b' <- mapM (cdocSubstInline nmChMp . bldCD) $ b
                    ; let (bs,nms,errml) = unzip3 b'
                          errm = Map.unions errml
                    ; putErrs errm
                    ; mapM_ (cdPut stdout) bs
                    ; return (Set.unions nms)
                    }
            else return Set.empty
        putHideBld opts fb nmChMp hdL
          = case optChDest opts of
              (ChHere,_) -> return ()
              (ChHide,f) -> do { let (d,_,es) = cdocSubst nmChMp . cdVer $ hdL
                               ; putErrs es
                               ; h <- openFile f' WriteMode
                               ; cdPut h d
                               ; hClose h
                               }
                         where f' = if null f then (fb ++ ".hide") else f
        wrapSem fp xr nmChMp pres = wrapAG_T opts fp xr nmChMp pres
        allNmChMpOf pres
          = (Map.unions m1,concat m2)
          where (m1,m2)
                  = unzip
                       [ (Map.mapKeys mkN (Map.unions (nMp:bMpL)) `Map.union` nMp,concat hdLL)
                       | ((ma,fp),pr) <- pres
                       , let nmPre = maybe (fpathBase fp) id ma
                             mkN = mkFullNm (mkNm nmPre)
                             r = wrapSem fp Set.empty Map.empty pr
                             nMp = gathNmChMp_Syn_AGItf r
                             (bMpL,hdLL) = unzip [ (bldNmChMp b,[ (mkN n,h) | (n,h) <- bldHideCD b]) | b <- selBld opts r ]
                       ]

trans :: Monad m => (String -> m [String]) -> [String] -> m (Map String [String])
trans f
  = trans' Map.empty
  where
    trans' mp []
      = return mp
    trans' mp keys
      = do rs <- mapM f keys
           let mp' = Map.fromList (zip keys rs) `Map.union` mp
           let keys' = filter (\k -> not (Map.member k mp')) (concat rs)
           trans' mp' keys'

genDeps :: FPath -> Opts -> IO ()
genDeps f opts
  = do filenames <- do c <- readFile (fpathToStr f)
                       return $ filter (not . null) (lines c)
       depMap <- trans getDeps filenames
       let depGraph = mkDpdGrFromEdgesMpPadMissing depMap
       let depL = [(r, filter (not . (`elem` filenames)) . stripIgn . Set.toList $ dgReachableFrom depGraph r) | r <- filenames ]
       genDepsMakefile depL opts
       return ()
  where
    mp = optDepTerm opts
    ignSet = optDepIgn opts
    stripIgn = filter (not . flip Set.member ignSet . stripDir)

    getDeps fname
      = Map.findWithDefault
          ( do let baseDir = optDepBaseDir opts
               let baseDir' = if last baseDir /= '/' then baseDir ++ "/" else baseDir
               let fpath = fpathSetSuff "cag" $ mkFPath (baseDir' ++ fname)
               res <- doCompile' fpath opts
               return (deps_Syn_AGItf res)
          ) name (Map.map return mp)
      where
        name = fpathBase . mkFPath $ fname

stripDir :: String -> String
stripDir = fpathToStr . fpathRemoveDir . mkFPath

genDepsMakefile :: [(String, [String])] -> Opts -> IO ()
genDepsMakefile deps opts
  = do mapM_ (putStrLn . mkDep) deps
       putStrLn ""
       putStrLn mkOrigDepList
       putStrLn ""
       putStrLn mkDepList
       putStrLn ""
       putStrLn mkMainList
  where
    namePrefix = optDepNamePrefix opts
    srcPrefix = optDepSrcVar opts
    dstPrefix = optDepDstVar opts
    depListVar = optDepDpdsVar opts
    mainListVar = optDepMainVar opts
    origListVar = optDepOrigDpdsVar opts

    mkDep (file, deps)
      = let name = encode file
            fileWithoutExt = stripExt file
            deps' = map stripExt deps
            ignSet = optDepIgn opts
         in unlines
             [ name ++ "_MAIN_SRC_AG := $(patsubst %,$(" ++ srcPrefix ++ ")%.cag," ++ fileWithoutExt ++ ")"
             , name ++ "_DPDS_SRC_AG := $(patsubst %,$(" ++ srcPrefix ++ ")%.cag," ++ unwords deps' ++ ")"
             , name ++ "_DPDS_ORIG   := $(patsubst %,$(" ++ srcPrefix ++ ")%.cag," ++ unwords (filter (not . flip Set.member ignSet . stripDir) deps') ++ ")"
             , name ++ "_DPDS_DERIV  := $(patsubst %,$(" ++ dstPrefix ++ ")%.ag," ++ unwords deps' ++ ")"
             , "$(patsubst $(" ++ srcPrefix ++ ")%.cag,$(" ++ dstPrefix ++ ")%.hs,$(" ++ name ++ "_MAIN_SRC_AG)) : $(" ++ name ++ "_DPDS_DERIV)"
             ]

    mkOrigDepList
      = origListVar ++ " := $(sort " ++ unwords (map ((\n -> "$(" ++ n ++ "_DPDS_ORIG)") . encode . fst) deps) ++ ")"

    mkDepList
      = depListVar ++ " := $(sort " ++ unwords (map ((\n -> "$(" ++ n ++ "_DPDS_SRC_AG)") . encode . fst) deps) ++ ")"

    mkMainList
      = mainListVar ++ " := " ++ unwords (map ((\n -> "$(" ++ n ++ "_MAIN_SRC_AG)") . encode . fst) deps)

    encode n = namePrefix ++ map (toUnder . toUpper) (stripExt n)

    stripExt n
      | '.' `elem` n = reverse $ tail $ dropWhile (/= '.') $ reverse n
      | otherwise    = n
    
    toUnder c
      | isAlphaNum c = c
      | otherwise    = '_'

-------------------------------------------------------------------------
-- Cmdline opts
-------------------------------------------------------------------------

cmdLineOpts
  =  [  Option "a"  ["ag"]              (NoArg oAG)
          "generate code for ag, default=no"
     ,  Option "h"  ["hs"]              (NoArg oHS)
          "generate code for haskell, default=no"
     ,  Option "l"  ["latex"]           (NoArg oLaTeX)
          "generate code for latex, default=no"
     ,  Option ""   ["preamble"]        (OptArg oPreamble "yes|no")
          "include preamble (marked by version=0), default=yes"
     ,  Option ""   ["line"]            (OptArg oLinePragmas "yes|no")
          "insert #LINE pragmas, default=no"
     ,  Option "p"  ["plain"]           (NoArg oPlain)
          "generate plain code, default=no"
     ,  Option ""   ["text2text"]       (NoArg oGenT2T)
          "generate code for haskell, default=no"
     ,  Option ""   ["index"]           (NoArg oIndex)
          "combined with latex, generate index entries, default=no"
     ,  Option ""   ["gen"]             (ReqArg oGenReqm "all|<nr>|(<nr> <aspect>*) (to be obsolete, renamed to --gen-reqm)")
          "generate for version, default=none"
     ,  Option "g"  ["gen-reqm"]        (ReqArg oGenReqm "all|<nr>|(<nr> <aspect>*)")
          "generate for version, default=none"
     ,  Option ""   ["compiler"]        (ReqArg oCompiler "<compiler version>")
          "Version of the GHC compiler, i.e. 6.6"
     ,  Option ""   ["hidedest"]        (ReqArg oHideDest "here|appx=<file>")
          "destination of text marked as 'hide', default=here"
     ,  Option ""   ["order"]           (ReqArg oVariantOrder "<order-spec> (to be obsolete, renamed to --variant-order)")
          "variant order"
     ,  Option ""   ["variant-order"]   (ReqArg oVariantOrder "<order-spec>")
          "variant order"
     ,  Option "b"  ["base"]            (ReqArg oBase "<name>")
          "base name, default=derived from filename"
     ,  Option ""   ["xref-except"]     (ReqArg oXRefExcept "<filename>")
          "file with list of strings not to be cross ref'd"
     ,  Option ""   ["help"]            (NoArg oHelp)
          "output this help"
     ,  Option ""   ["dep"]             (NoArg oDep)
          "output dependencies"
     ,  Option ""   ["depnameprefix"]   (OptArg oDepNamePrefix "<name>")
          "Prefix of generated makefile vars."
     ,  Option ""   ["depsrcvar"]       (OptArg oDepSrcVar "<name>")
          "Source base-directory"
     ,  Option ""   ["depdstvar"]       (OptArg oDepDstVar "<name>")
          "Destination base-directory"
     ,  Option ""   ["depmainvar"]      (OptArg oDepMainVar "<name>")
          "Varname for the list of main files"
     ,  Option ""   ["depdpdsvar"]      (OptArg oDepDpdsVar "<name>")
          "Varname for the list of dependencies"
     ,  Option ""   ["deporigdpdsvar"]  (OptArg oDepOrigDpdsVar "<name>")
          "Varname for the list of original dependencies"
     ,  Option ""   ["depbase"]         (OptArg oDepBaseDir "<dir>")
          "Root directory for the dependency generation"
     ,  Option ""   ["depign"]          (OptArg oDepIgn "(<file> )*")
          "Totally ignored dependencies"
     ,  Option ""   ["depterm"]         (OptArg oDepTerm "(<file> => <dep>+ ,)*")
          "Dependency ignore list (or terminals)"
     ,  Option ""   ["lhs2tex"]         (OptArg oLhs2tex "yes|no")
          "wrap chunks in lhs2tex's code environment, default=yes"
     ,  Option ""   ["agmodheader"]     (OptArg oAGModHeader "yes|no")
          "generate AG MODULE headers instead of Haskell module headers"
     ,  Option ""   ["def"]             (ReqArg oDef "key:value")
          "define key/value pair, alternate form: key=value"
     ]
  where  oAG             o =  o {optAG = True}
         oHS             o =  o {optHS = True}
         oPreamble   ms  o =  yesno (\f o -> o {optPreamble = f}) ms o
         oLinePragmas ms o =  yesno (\f o -> o {optLinePragmas = f}) ms o
         oLaTeX          o =  o {optLaTeX = True}
         oPlain          o =  o {optPlain = True}
         oGenT2T         o =  o {optGenText2Text = True}
         oIndex          o =  o {optIndex = True}
         oCompiler    s  o =  o {optCompiler = map read (words (map (\c -> if c == '.' then ' ' else c) s))}
         oLhs2tex    ms  o =  yesno' ChWrapCode ChWrapPlain (\f o -> o {optWrapLhs2tex = f}) ms o
         oBase        s  o =  o {optBaseName = Just s}
         oVariantOrder    s  o =  o {optVariantRefOrder = parseAndGetRes pVariantRefOrder s}
         oXRefExcept  s  o =  o {optMbXRefExcept = Just s}
         oGenReqm            s  o =  case dropWhile isSpace s of
                                "all"               -> o {optGenReqm = VReqmAll}
                                (c:_) | isDigit c   -> o {optGenReqm = parseAndGetRes pVariantReqmRef s}
                                      | c == '('    -> o {optGenReqm = parseAndGetRes pVariantReqm s}
                                _                   -> o {optGenReqm = VReqmAll}
         oHideDest    s  o =  case s of
                                "here"                  -> o
                                ('a':'p':'p':'x':'=':f) -> o {optChDest = (ChHide,f)}
                                _                       -> o
         oHelp           o =  o {optHelp = True}
         oDep            o =  o {optGenDeps = True}
         oDepNamePrefix ms o = o { optDepNamePrefix = maybe "FILE_" id ms }
         oDepSrcVar     ms o = o { optDepSrcVar = maybe "SRC_VAR" id ms }
         oDepDstVar     ms o = o { optDepDstVar = maybe "DST_VAR" id ms }
         oDepMainVar    ms o = o { optDepMainVar = maybe "FILES" id ms }
         oDepDpdsVar    ms o = o { optDepDpdsVar = maybe "DPDS" id ms }
         oDepOrigDpdsVar ms o = o { optDepOrigDpdsVar = maybe "ORIG_DPDS" id ms }
         oDepBaseDir ms o = o { optDepBaseDir = maybe "./" id ms }
         oDepTerm ms o = o { optDepTerm = maybe Map.empty (Map.fromList . parseDeps) ms }
         oDepIgn ms o = o { optDepIgn = maybe Set.empty (Set.fromList . words) ms }
         oAGModHeader ms o = yesno (\f o -> o {optAGModHeader = f}) ms o
         oDef         s  o =  case break (\c -> c == ':' || c == '=') s of
                                (k,(_:v)) -> o {optDefs = Map.insert k v (optDefs o)}
                                _         -> o
         yesno' y n updO  ms  o
                           =  case ms of
                                Just "yes"  -> updO y o
                                Just "no"   -> updO n o
                                _           -> o
         yesno             =  yesno' True False

         parseDeps "" = []
         parseDeps (',' : rest) = parseDeps rest
         parseDeps s
           = let (s',rest) = break (==',') s
              in parseDep s' : parseDeps rest

         parseDep s
           = let (term,_:deps) = break (=='>') s
              in (term, words deps)
