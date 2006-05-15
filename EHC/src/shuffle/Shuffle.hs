-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import System
import IO
import System.Console.GetOpt
import ParseUtils
import Common
import MainAG
import ChunkParser
import CDoc
import CDocSubst
-- import Data.List

-------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------

main :: IO ()
main
  = do { args <- getArgs
       ; let oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
             opts           = foldr ($) defaultOpts o
       ; if optHelp opts
         then putStrLn (usageInfo "Usage shuffle [options] [file|-]\n\noptions:" cmdLineOpts)
         else if null errs
              then  let (f,frest) = if null n then (emptyFPath,[]) else if head n == "-" then (emptyFPath,tail n) else (mkFPath (head n),tail n)
                    in  doCompile f (map mkFPath frest) opts
              else  putStr (head errs)
       }

readShFile :: FPath -> Opts -> IO (FPath,T_AGItf)
readShFile fp opts
  = do { (fp,fh)
             <- if fpathIsEmpty fp
                then return (mkFPath "<stdin>",stdin)
                else do { let fn = fpathToStr fp
                        ; h <- openFile fn ReadMode
                        ; return (fp,h)
                        }
       ; txt <- hGetContents fh
       ; let toks = scan shuffleScanOpts ScSkip txt
       ; let (pres,perrs) = parseToResMsgs pAGItf toks
       ; if null perrs
         then return (fp,pres)
         else do { mapM_ (hPutStrLn stderr . show) perrs
                 ; exitFailure
                 }
       }

doCompile :: FPath -> [FPath] -> Opts -> IO ()
doCompile fp fpRest opts
  = do { xrefExceptFileContent
           <- case optMbXRefExcept opts of
                Just f -> do c <- readFile f
                             return (Set.unions . map (Set.fromList . words) . lines $ c)
                Nothing -> return Set.empty
       ; allPRes@((fp',pres):restPRes) <- mapM (\f -> readShFile f opts) (fp:fpRest)
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
                       | (fp,pr) <- pres
                       , let mkN = mkFullNm (mkNm (fpathBase fp))
                             r = wrapSem fp Set.empty Map.empty pr
                             nMp = gathNmChMp_Syn_AGItf r
                             (bMpL,hdLL) = unzip [ (bldNmChMp b,[ (mkN n,h) | (n,h) <- bldHideCD b]) | b <- selBld opts r ]
                       ]

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
     ,  Option "p"  ["plain"]           (NoArg oPlain)
          "generate plain code, default=no"
     ,  Option ""   ["index"]           (NoArg oIndex)
          "combined with latex, generate index entries, default=no"
     ,  Option "g"  ["gen"]             (ReqArg oGen "all|<nr>")
          "generate for version, default=none"
     ,  Option ""   ["hidedest"]        (ReqArg oHideDest "here|appx=<file>")
          "destination of text marked as 'hide', default=here"
     ,  Option ""   ["order"]           (ReqArg oVerOrder "<order-spec>")
          "version order"
     ,  Option "b"  ["base"]            (ReqArg oBase "<name>")
          "base name, default=derived from filename"
     ,  Option ""   ["xref-except"]     (ReqArg oXRefExcept "<filename>")
          "file with list of strings not to be cross ref'd"
     ,  Option ""   ["help"]            (NoArg oHelp)
          "output this help"
     ,  Option ""   ["lhs2tex"]         (OptArg oLhs2tex "yes|no")
          "wrap chunks in lhs2tex's code environment, default=yes"
     ]
  where  oAG             o =  o {optAG = True}
         oHS             o =  o {optHS = True}
         oPreamble   ms  o =  yesno (\f o -> o {optPreamble = f}) ms o
         oLaTeX          o =  o {optLaTeX = True}
         oPlain          o =  o {optPlain = True}
         oIndex          o =  o {optIndex = True}
         oLhs2tex    ms  o =  yesno' ChWrapCode ChWrapPlain (\f o -> o {optWrapLhs2tex = f}) ms o
         oBase        s  o =  o {optBaseName = Just s}
         oVerOrder    s  o =  o {optVerOrder = parseAndGetRes pVerOrder s}
         oXRefExcept  s  o =  o {optMbXRefExcept = Just s}
         oGen         s  o =  case s of
                                "all"               -> o {optGenVersion = VAll}
                                (c:_) | isDigit c   -> o {optGenVersion = parseAndGetRes pVersion s}
                                _                   -> o {optGenVersion = VAll}
         oHideDest    s  o =  case s of
                                "here"                  -> o
                                ('a':'p':'p':'x':'=':f) -> o {optChDest = (ChHide,f)}
                                _                       -> o
         oHelp           o =  o {optHelp = True}
         yesno' y n updO  ms  o
                           =  case ms of
                                Just "yes"  -> updO y o
                                Just "no"   -> updO n o
                                _           -> o
         yesno             =  yesno' True False

