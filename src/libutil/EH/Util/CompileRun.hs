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
  
  , cpUpdStateInfo, cpUpdSI
  
  , cpUpdCU, cpUpdCUWithKey
  , cpSetFail, cpSetStop, cpSetStopSeq, cpSetStopAllSeq
  , cpSetOk, cpSetErrs, cpSetLimitErrs, cpSetLimitErrsWhen, cpSetInfos, cpSetCompileOrder
  , cpSeq, (>->), cpEmpty 
  , cpFindFilesForFPath, cpFindFileForFPath
  , cpImportGather, cpImportGatherFromMods
  , cpPP, cpPPMsg
  
  , forgetM
  )
  where

import Maybe
import System.Exit
import Control.Monad
import Control.Monad.State
import IO
import qualified Data.Map as Map
import EH.Util.Pretty
import UU.DData.Scc as Scc
import EH.Util.Utils( panicJust )
import EH.Util.FPath


-------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------

-- forget result
forgetM :: Monad m => m a -> m ()
forgetM m
  = do { _ <- m
       ; return ()
       }

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
  = CRSOk									-- continue
  | CRSFail									-- fail and stop
  | CRSStopSeq								-- stop current cpSeq
  | CRSStopAllSeq							-- stop current cpSeq, but also the surrounding ones
  | CRSStop									-- stop completely
  | CRSFailErrL String [err] (Maybe Int)	-- fail with errors and stop
  | CRSErrInfoL String Bool [err]			-- just errors, continue

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
-- State manipulation, sequencing: compile unit
-------------------------------------------------------------------------

crMbCU :: Ord n => n -> CompileRun n u i e -> Maybe u
crMbCU modNm cr = Map.lookup modNm (crCUCache cr)

crCU :: (Show n,Ord n) => n -> CompileRun n u i e -> u
crCU modNm = panicJust ("crCU: " ++ show modNm) . crMbCU modNm

-------------------------------------------------------------------------
-- State manipulation, sequencing: non monadic
-------------------------------------------------------------------------

crSetFail :: CompileRun n u i e -> CompileRun n u i e
crSetFail cr = cr {crState = CRSFail}

crSetStop :: CompileRun n u i e -> CompileRun n u i e
crSetStop cr = cr {crState = CRSStop}

crSetStopSeq :: CompileRun n u i e -> CompileRun n u i e
crSetStopSeq cr = cr {crState = CRSStopSeq}

crSetStopAllSeq :: CompileRun n u i e -> CompileRun n u i e
crSetStopAllSeq cr = cr {crState = CRSStopAllSeq}

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

cpFindFilesForFPath
  :: (Ord n,FPATH n,CompileUnitState s,CompileRunError e p,CompileUnit u n s,CompileModName n,CompileRunStateInfo i n p)
       => Bool -> [(String,s)] -> [String] -> Maybe n -> Maybe FPath -> CompilePhase n u i e [FPath]
cpFindFilesForFPath stopAtFirst suffs sp mbModNm mbFp
  = do { cr <- get
       ; let cus = maybe cusUnk (flip crCUState cr) mbModNm
       ; if cusIsUnk cus
          then do { let fp = maybe (mkFPath $ panicJust ("cpFindFileForFPath") $ mbModNm) id mbFp
                        modNm = maybe (mkCMNm $ fpathBase $ fp) id mbModNm
                  ; fpsFound <- lift (searchPathForReadableFiles stopAtFirst sp (map fst suffs) fp)
                  ; case fpsFound of
                      []
                        -> do { cpSetErrs (creMkNotFoundErrL (crsiImportPosOfCUKey modNm (crStateInfo cr)) (fpathToStr fp) sp)
                              ; return []
                              }
                      ffs@(ff:_)
                        -> do { cpUpdCU modNm (cuUpdFPath ff . cuUpdState cus . cuUpdKey modNm)
                              ; return ffs
                              }
                        where cus = case lookup (fpathSuff ff) suffs of
                                      Just c  -> c
                                      Nothing -> case lookup "*" suffs of
                                                   Just c  -> c
                                                   Nothing -> cusUnk
                  }
          else return (maybe [] (\nm -> [crCUFPath nm cr]) mbModNm)
       }

cpFindFileForFPath
  :: (Ord n,FPATH n,CompileUnitState s,CompileRunError e p,CompileUnit u n s,CompileModName n,CompileRunStateInfo i n p)
       => [(String,s)] -> [String] -> Maybe n -> Maybe FPath -> CompilePhase n u i e (Maybe FPath)
cpFindFileForFPath suffs sp mbModNm mbFp
  = do { fps <- cpFindFilesForFPath True suffs sp mbModNm mbFp
       ; return (listToMaybe fps)
       }

-------------------------------------------------------------------------
-- Gather all imports
-------------------------------------------------------------------------

cpImportGatherFromMods
  :: (Show n,Ord n,CompileUnit u n s,CompileRunError e p,CompileUnitState s)
       => (n -> CompilePhase n u i e x) -> [n] -> CompilePhase n u i e ()
cpImportGatherFromMods imp1Mod modNmL
  = do { cr <- get
       ; cpSeq (   concat [ [forgetM (imp1Mod modNm), imps modNm] | modNm <- modNmL ]
                ++ [cpImportScc]
               )
       }
  where imps m = do { cr <- get
                    ; let impL m = [ i | i <- cuImports (crCU m cr), not (cusIsImpKnown (crCUState i cr)) ]
                    ; cpSeq (map (\n -> cpSeq [forgetM (imp1Mod n), imps n]) (impL m))
                    }

cpImportGather
  :: (Show n,Ord n,CompileUnit u n s,CompileRunError e p,CompileUnitState s)
       => (n -> CompilePhase n u i e ()) -> n -> CompilePhase n u i e ()
cpImportGather imp1Mod modNm
  = cpImportGatherFromMods imp1Mod [modNm]

crImportDepL :: (CompileUnit u n s) => CompileRun n u i e -> [(n,[n])]
crImportDepL = map (\cu -> (cuKey cu,cuImports cu)) . Map.elems . crCUCache

cpImportScc :: (Ord n,CompileUnit u n s) => CompilePhase n u i e ()
cpImportScc = modify (\cr -> (cr {crCompileOrder = Scc.scc (crImportDepL cr)}))


-------------------------------------------------------------------------
-- State manipulation, state update (Monadic)
-------------------------------------------------------------------------

cpUpdStateInfo, cpUpdSI :: (i -> i) -> CompilePhase n u i e ()
cpUpdStateInfo upd
  = do { cr <- get
       ; put (cr {crStateInfo = upd (crStateInfo cr)})
       }

cpUpdSI = cpUpdStateInfo

-------------------------------------------------------------------------
-- State manipulation, compile unit update (Monadic)
-------------------------------------------------------------------------

cpUpdCUM :: (Ord n,CompileUnit u n s) => n -> (u -> IO u) -> CompilePhase n u i e ()
cpUpdCUM modNm upd
  = do { cr <- get
       ; cu <- lift (maybe (upd cuDefault) upd (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }


cpUpdCUWithKey :: (Ord n,CompileUnit u n s) => n -> (n -> u -> (n,u)) -> CompilePhase n u i e n
cpUpdCUWithKey modNm upd
  = do { cr <- get
       ; let (modNm',cu) = (maybe (upd modNm cuDefault) (upd modNm) (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm' cu $ Map.delete modNm $ crCUCache cr})
       ; return modNm'
       }

cpUpdCU :: (Ord n,CompileUnit u n s) => n -> (u -> u) -> CompilePhase n u i e ()
cpUpdCU modNm upd
  = do { cpUpdCUWithKey modNm (\k u -> (k, upd u))
       ; return ()
       }
{-
  = do { cr <- get
       ; let cu = (maybe (upd cuDefault) upd (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }
-}
{-
cpUpdCU modNm upd
 = cpUpdCUM modNm (return . upd)
-}

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

cpSetStop :: CompilePhase n u i e ()
cpSetStop
 = modify crSetStop

cpSetStopSeq :: CompilePhase n u i e ()
cpSetStopSeq
 = modify crSetStopSeq

cpSetStopAllSeq :: CompilePhase n u i e ()
cpSetStopAllSeq
 = modify crSetStopAllSeq

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

cpEmpty :: CompilePhase n u i e ()
cpEmpty = return ()

infixr 2 >->

(>->) :: CompileRunError e p => CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
this >-> next = cpHandle1 next this

cpSeq :: CompileRunError e p => [CompilePhase n u i e ()] -> CompilePhase n u i e ()
cpSeq []     = return ()
cpSeq (a:as) = cpHandle1 (do { cpSetOk
                             ; cpSeq as
                             })
                         a

cpHandleErr :: CompileRunError e p => CompilePhase n u i e ()
cpHandleErr
  = do { cr <- get
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
                   ; if not (null is) then lift exitFailure else return ()
                   }
             where e = empty -- if doPrint then crePPErrL is else empty
           CRSFail
             -> do { lift exitFailure
                   }
           CRSStop
             -> do { lift $ exitWith ExitSuccess
                   }
           _ -> return ()
       }
  where putErr' m e   = if null e
                        then return ()
                        else do { hPutPPLn stderr (crePPErrL e)
                                ; m
                                ; hFlush stderr
                                }
        failOrNot es = if creAreFatal es then lift exitFailure else cpSetOk

cpHandle1 :: CompileRunError e p => CompilePhase n u i e () -> CompilePhase n u i e () -> CompilePhase n u i e ()
cpHandle1 rest first
  = do { _ <- first
       ; cpHandleErr
       ; cr <- get
       ; case crState cr of
           CRSOk         -> rest
           CRSStopSeq    -> cpSetOk
           CRSStopAllSeq -> cpSetStopAllSeq
           _             -> return ()
       }
