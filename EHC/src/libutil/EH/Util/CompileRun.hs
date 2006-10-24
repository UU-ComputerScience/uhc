-------------------------------------------------------------------------
-- Combinators for a compile run
-------------------------------------------------------------------------

module EH.Util.CompileRun
  ( CompileRunState(..)
  , CompileRun(..)
  , CompilePhase
  , CompileUnit(..)
  , CompileUnitState(..)
  , CompileRunError(..)
  , CompileModName(..)
  , CompileRunStateInfo(..)
  
  , mkEmptyCompileRun

  , crCU, crMbCU
  , ppCR
  
  , cpUpdCU
  , cpSetFail, cpSetOk, cpSetErrs, cpSetLimitErrs, cpSetLimitErrsWhen, cpSetInfos, cpSetCompileOrder
  , cpSeq
  , cpFindFileForFPath
  , cpImportGather
  , cpPP, cpPPMsg
  )
  where

import Maybe
import System.Exit
import Control.Monad
import Control.Monad.State
import IO
import qualified Data.Map as Map
import UU.Pretty
import UU.DData.Scc as Scc
import EH.Util.Utils( panicJust )
import EH.Util.PPUtils
import EH.Util.FPath


-------------------------------------------------------------------------
-- Interfacing with actual state info
-------------------------------------------------------------------------

class CompileModName n where
  mkCMNm      	:: String -> n

class CompileUnitState s where
  cusDefault  	:: s
  cusUnk      	:: s
  cusIsUnk      :: s -> Bool
  cusIsImpKnown	:: s -> Bool

class CompileUnit u n s | u -> n s where
  cuDefault 	:: u
  cuFPath   	:: u -> FPath
  cuUpdFPath    :: FPath -> u -> u
  cuKey     	:: u -> n
  cuUpdKey      :: n -> u -> u
  cuState   	:: u -> s
  cuUpdState    :: s -> u -> u
  cuImports     :: u -> [n]

class CompileRunError e p | e -> p where
  crePPErrL         :: [e] -> PP_Doc
  creMkNotFoundErrL :: p -> String -> [String] -> [e]
  creAreFatal       :: [e] -> Bool

class CompileRunStateInfo i n p where
  crsiImportPosOfCUKey :: n -> i -> p

-------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------

data CompileRunState err
  = CRSOk
  | CRSFail
  | CRSFailErrL String [err] (Maybe Int)
  | CRSErrInfoL String Bool [err]

data CompileRun nm unit info err
  = CompileRun
      { crCUCache       :: Map.Map nm unit
      , crCompileOrder  :: [[nm]]
      , crTopModNm      :: nm
      , crState         :: CompileRunState err
      , crStateInfo     :: info
      }

type CompilePhase n u i e a = StateT (CompileRun n u i e) IO a



mkEmptyCompileRun :: n -> i -> CompileRun n u i e
mkEmptyCompileRun nm info
  = CompileRun
      { crCUCache		= Map.empty
      , crCompileOrder	= []
      , crTopModNm      = nm
      , crState			= CRSOk
      , crStateInfo		= info
      }

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

ppCR :: (PP n,PP u) => CompileRun n u i e -> PP_Doc
ppCR cr
  = "CR:" >#<
      (   (ppListSepVV "[" "]" "," $ map (\(n,u) -> pp n >#< "->" >#< pp u) $ Map.toList $ crCUCache $ cr)
      >-< ppBracketsCommas (map ppBracketsCommas $ crCompileOrder $ cr)
      )

crPP :: (PP n,PP u) => String -> CompileRun n u i e -> IO (CompileRun n u i e)
crPP m cr = do { hPutStrLn stderr (m ++ ":") ; hPutPPLn stderr (ppCR cr) ; hFlush stderr ; return cr }

crPPMsg :: (PP m) => m -> CompileRun n u i e -> IO (CompileRun n u i e)
crPPMsg m cr = do { hPutPPLn stdout (pp m) ; return cr }

cpPP :: (PP n,PP u) => String -> CompilePhase n u i e ()
cpPP m
 = do { lift (hPutStrLn stderr (m ++ ":"))
      ; cr <- get
      ; lift (hPutPPLn stderr (ppCR cr))
      ; lift (hFlush stderr)
      ; return () 
      }

cpPPMsg :: (PP m) => m -> CompilePhase n u i e ()
cpPPMsg m 
 = do { lift (hPutPPLn stdout (pp m))
      ; return ()
      }



-------------------------------------------------------------------------
-- State manipulation, sequencing
-------------------------------------------------------------------------

crMbCU :: Ord n => n -> CompileRun n u i e -> Maybe u
crMbCU modNm cr = Map.lookup modNm (crCUCache cr)

crCU :: (Show n,Ord n) => n -> CompileRun n u i e -> u
crCU modNm = panicJust ("crCU: " ++ show modNm) . crMbCU modNm

crSetFail :: CompileRun n u i e -> CompileRun n u i e
crSetFail cr = cr {crState = CRSFail}

crSetErrs' :: Maybe Int -> String -> [e] -> CompileRun n u i e -> CompileRun n u i e
crSetErrs' limit about es cr
  = case es of
      [] -> cr
      _  -> cr {crState = CRSFailErrL about es limit}

crSetInfos' :: String -> Bool -> [e] -> CompileRun n u i e -> CompileRun n u i e
crSetInfos' msg dp is cr
  = case is of
      [] -> cr
      _  -> cr {crState = CRSErrInfoL msg dp is}

-------------------------------------------------------------------------
-- Compile unit observations
-------------------------------------------------------------------------

crCUState :: (Ord n,CompileUnit u n s,CompileUnitState s) => n -> CompileRun n u i e -> s
crCUState modNm cr = maybe cusUnk cuState (crMbCU modNm cr)

crCUFPath :: (Ord n,CompileUnit u n s) => n -> CompileRun n u i e -> FPath
crCUFPath modNm cr = maybe emptyFPath cuFPath (crMbCU modNm cr)

-------------------------------------------------------------------------
-- Find file for FPath
-------------------------------------------------------------------------

cpFindFileForFPath
  :: (Ord n,FPATH n,CompileUnitState s,CompileRunError e p,CompileUnit u n s,CompileModName n,CompileRunStateInfo i n p)
       => [(String,s)] -> [String] -> Maybe n -> Maybe FPath -> CompilePhase n u i e (Maybe FPath)
cpFindFileForFPath suffs sp mbModNm mbFp
  = do { cr <- get
       ; let cus = maybe cusUnk (flip crCUState cr) mbModNm
       ; if cusIsUnk cus
          then do { let fp = maybe (mkFPath $ panicJust ("cpFindFileForFPath") $ mbModNm) id mbFp
                        modNm = maybe (mkCMNm $ fpathBase $ fp) id mbModNm
                  ; mbFpFound <- lift (searchPathForReadableFile sp (map fst suffs) fp)
                  ; case mbFpFound of
                      Nothing
                        -> do { cpSetErrs (creMkNotFoundErrL (crsiImportPosOfCUKey modNm (crStateInfo cr)) (fpathToStr fp) sp)
                              ; return Nothing
                              }
                      Just ff
                        -> do { cpUpdCU modNm (cuUpdFPath ff . cuUpdState cus . cuUpdKey modNm)
                              ; return (Just ff)
                              }
                        where cus = case lookup (fpathSuff ff) suffs of
                                      Just c  -> c
                                      Nothing -> case lookup "*" suffs of
                                                   Just c  -> c
                                                   Nothing -> cusUnk
                  }
          else return (maybe Nothing (\nm -> Just (crCUFPath nm cr)) mbModNm)
       }

-------------------------------------------------------------------------
-- Gather all imports
-------------------------------------------------------------------------

cpImportGather
  :: (Show n,Ord n,CompileUnit u n s,CompileRunError e p,CompileUnitState s)
       => (n -> CompilePhase n u i e ()) -> n -> CompilePhase n u i e ()
cpImportGather imp1Mod modNm
  = do { cr <- get
       ; cpSeq [imp1Mod modNm, imps modNm, cpImportScc]
       }
  where imps m = do { cr <- get
                    ; let impL m = [ i | i <- cuImports (crCU m cr), not (cusIsImpKnown (crCUState i cr)) ]
                    ; cpSeq (map (\n -> cpSeq [imp1Mod n, imps n]) (impL m))
                    }

crImportDepL :: (CompileUnit u n s) => CompileRun n u i e -> [(n,[n])]
crImportDepL = map (\cu -> (cuKey cu,cuImports cu)) . Map.elems . crCUCache

cpImportScc :: (Ord n,CompileUnit u n s) => CompilePhase n u i e ()
cpImportScc = modify (\cr -> (cr {crCompileOrder = Scc.scc (crImportDepL cr)}))


-------------------------------------------------------------------------
-- State manipulation, sequencing (Monadic)
-------------------------------------------------------------------------

cpSetErrs :: [e] -> CompilePhase n u i e ()
cpSetErrs es
 = modify (crSetErrs' Nothing "" es)

cpSetInfos :: String -> Bool -> [e] -> CompilePhase n u i e ()
cpSetInfos msg dp is
 = modify (crSetInfos' msg dp is)

cpSetFail :: CompilePhase n u i e ()
cpSetFail
 = modify crSetFail

cpSetOk :: CompilePhase n u i e ()
cpSetOk
 = modify (\cr -> (cr {crState = CRSOk}))

cpSetCompileOrder :: [[n]] -> CompilePhase n u i e ()
cpSetCompileOrder nameLL
 = modify (\cr -> (cr {crCompileOrder = nameLL}))

cpSetLimitErrs, cpSetLimitErrsWhen :: Int -> String -> [e] -> CompilePhase n u i e ()
cpSetLimitErrs l a e
 = modify (crSetErrs' (Just l) a e)

cpSetLimitErrsWhen l a e
 = do { when (not (null e))
             (cpSetLimitErrs l a e)
      }

cpUpdCUM :: (Ord n,CompileUnit u n s) => n -> (u -> IO u) -> CompilePhase n u i e ()
cpUpdCUM modNm upd
  = do { cr <- get
       ; cu <- lift (maybe (upd cuDefault) upd (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }


cpUpdCU :: (Ord n,CompileUnit u n s) => n -> (u -> u) -> CompilePhase n u i e ()
cpUpdCU modNm upd
  = do { cr <- get
       ; let cu = (maybe (upd cuDefault) upd (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }

{-
cpUpdCU modNm upd
 = cpUpdCUM modNm (return . upd)
-}

cpSeq :: CompileRunError e p => [CompilePhase n u i e ()] -> CompilePhase n u i e ()
cpSeq []     = return ()
cpSeq (a:as) = cpHandle1 (do { cpSetOk
                             ; cpSeq as
                             })
                         a

cpHandle1 :: CompileRunError e p => CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
cpHandle1 rest first
  = do { _ <- first
       ; cr <- get
       ; case crState cr of
           CRSFailErrL about es (Just lim)
             -> do { let (showErrs,omitErrs) = splitAt lim es
                   ; lift (unless (null about) (hPutPPLn stderr (pp about)))
                   ; lift (putErr' (if null omitErrs then return () else hPutStrLn stderr "... and more errors") showErrs)
                   ; failOrNot es
                   }
           CRSFailErrL about es Nothing
             -> do { lift (unless (null about) (hPutPPLn stderr (pp about)))
                   ; lift (putErr' (return ()) es)
                   ; failOrNot es
                   }
           CRSErrInfoL about doPrint is
             -> do { if null is
                     then return ()
                     else lift (do { hFlush stdout
                                   ; hPutPPLn stderr (about >#< "found errors" >-< e)
                                   })
                   ; if not (null is) then cpSetFail else rest
                   }
             where e = empty -- if doPrint then crePPErrL is else empty
           CRSFail
             -> do { lift exitFailure
                   ; cpSetFail
                   }
           CRSOk
             -> rest
       }
  where putErr' m e   = if null e
                        then return ()
                        else
                             do { hPutPPLn stderr (crePPErrL e)
                                ; m
                                ; hFlush stderr
                                }
        failOrNot es = if creAreFatal es then cpSetFail else rest

