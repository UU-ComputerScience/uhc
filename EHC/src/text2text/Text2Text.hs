-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

{-
text2text converts a nested combination of typed text fragments to 1 typed output. A text fragment is delimited by '@@[<type>' and '@@]',
Text is processed in the following steps:
- parse/analyse the chunk structure to find out the types of chunks
- parse individual chunks according to their <type>
- this gives a representation in a common Text format
- which is then output into the requested representation.

The idea is to have the following formats supported, all in a restricted form appropriate for documentation and (relatively) easy mutual transformation
- doclatex: documentation LaTeX
- twiki:
- texinfo: 
- html: 
Currently supported/implemented:
- input : doclatex
- output: doclatex, twiki
-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import System
import System.Console.GetOpt
import IO

import EH.Util.FPath

import Common
import Text
import Text.Trf.UniformContent
import Text.Parser
import Plugin

-- for plugin: generation of output
import qualified Text.To.DocLaTeX       as O_DocLaTeX
import qualified Text.To.TWiki          as O_TWiki

-- for plugin: parsing input
import qualified Text.Parser.DocLaTeX   as P_DocLaTeX

-------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------

main :: IO ()
main
  = do { args <- getArgs
       ; let oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
             opts           = foldr ($) defaultOpts o
       ; if optHelp opts
         then putStrLn (usageInfo "Usage: text2text [options] [file|-]\n\noptions:" cmdLineOpts)
         else if null errs
              then  let (f,frest) = if null n then (emptyFPath,[]) else if head n == "-" then (emptyFPath,tail n) else (mkFPath (head n),tail n)
                    in  doCompile f opts
              else  putStr (head errs)
       }

readT2TFile :: FPath -> Opts -> IO AGItf
readT2TFile fp opts
  = do { (fp',fh) <- fpathOpenOrStdin fp
       ; txt <- hGetContents fh
       ; let toks = scan t2tScanOpts defaultScState infpStart [ScInput_Uninterpreted txt]
       -- ; putStrLn (show toks)
       ; let (pres,perrs) = parseToResMsgs pAGItf toks
       ; if null perrs
         then return pres
         else do { mapM_ (hPutStrLn stderr . show) perrs
                 ; exitFailure
                 }
       }

-------------------------------------------------------------------------
-- Plugins
-------------------------------------------------------------------------

pluginMp :: PluginMp
pluginMp
  = Map.fromList
      [ ( TextType_DocLaTeX
        , defaultPlugin
            { plgParseTextItems 	= Just P_DocLaTeX.pItf
            , plgScanOptsMp 		= P_DocLaTeX.doclatexScanOptsMp
            , plgScanInitState		= defaultScState { scstateType = ScTpContent TextType_DocLaTeX }
            , plgToOutDoc			= Just O_DocLaTeX.textToOutDoc
            }
        )
      , ( TextType_TWiki
        , defaultPlugin
            { plgToOutDoc			= Just O_TWiki.textToOutDoc
            }
        )
      ]

-------------------------------------------------------------------------
-- Cmdline opts
-------------------------------------------------------------------------

cmdLineOpts
  =  [ Option "" [s] (NoArg (\o -> o {optGenFor = t})) ("generate " ++ s) | (s,t) <- Map.toList texttypeMp
     ]
     ++
     [  Option ""   ["help"]            			(NoArg oHelp)
          "output this help"
     ,  Option ""   ["gen-header-numbering"]        (OptArg oGenHdrNr "yes|no")
          "generate header numbering, default=no"
{-
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
     ,  Option ""   ["index"]           (NoArg oIndex)
          "combined with latex, generate index entries, default=no"
     ,  Option ""   ["gen"]             (ReqArg oGenReqm "all|<nr>|(<nr> <aspect>*) (to be obsolete, renamed to --gen-reqm)")
          "generate for version, default=none"
     ,  Option "g"  ["gen-reqm"]        (ReqArg oGenReqm "all|<nr>|(<nr> <aspect>*)")
          "generate for version, default=none"
     ,  Option ""   ["compiler"]  (ReqArg oCompiler "<compiler version>")
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
-}
     ]
  where  oDocLaTeX       o =  o {optGenFor = TextType_DocLaTeX}
         oHelp           o =  o {optHelp = True}
         oGenHdrNr   ms  o =  yesno (\f o -> o {optGenHeaderNumbering = f}) ms o
{-
         oHS             o =  o {optHS = True}
         oPreamble   ms  o =  yesno (\f o -> o {optPreamble = f}) ms o
         oLinePragmas ms o =  yesno (\f o -> o {optLinePragmas = f}) ms o
         oLaTeX          o =  o {optLaTeX = True}
         oPlain          o =  o {optPlain = True}
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
-}
         yesno' y n updO  ms  o
                           =  case ms of
                                Just "yes"  -> updO y o
                                Just "no"   -> updO n o
                                _           -> o
         yesno             =  yesno' True False

{-
         parseDeps "" = []
         parseDeps (',' : rest) = parseDeps rest
         parseDeps s
           = let (s',rest) = break (==',') s
              in parseDep s' : parseDeps rest

         parseDep s
           = let (term,_:deps) = break (=='>') s
              in (term, words deps)
-}

-------------------------------------------------------------------------
-- The actual work
-------------------------------------------------------------------------

doCompile :: FPath -> Opts -> IO ()
doCompile f opts
  = do { pres <- readT2TFile f opts
       ; let (pres2,errs2) = textTrfUniformContent opts pluginMp pres
       ; mapM_ (hPutOutLn stderr . out) errs2
       ; case Map.lookup (optGenFor opts) pluginMp of
           Just plg | isJust mbToOut
             -> putOut $ fromJust mbToOut opts pres2
             where mbToOut = plgToOutDoc plg
           _ -> hPutOutLn stderr ("no output generator for " +++ show (optGenFor opts))
       }

