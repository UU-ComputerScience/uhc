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
  
  , FileLocatable(..)
  
  , mkEmptyCompileRun

  , crCU, crMbCU
  , ppCR
  
  , cpUpdStateInfo, cpUpdSI
  
  , cpUpdCU, cpUpdCUWithKey
  , cpSetFail, cpSetStop, cpSetStopSeq, cpSetStopAllSeq
  , cpSetOk, cpSetErrs, cpSetLimitErrs, cpSetLimitErrsWhen, cpSetInfos, cpSetCompileOrder

  , cpSeq, cpSeqWhen
  , cpEmpty

  , cpFindFileForNameOrFPath
  , cpFindFilesForFPathInLocations, cpFindFilesForFPath, cpFindFileForFPath
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
import EH.Util.Utils(panicJust, scc)
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

class CompileUnit u n l s | u -> n l s where
  cuDefault 	:: u
  cuFPath   	:: u -> FPath
  cuUpdFPath    :: FPath -> u -> u
  cuLocation	:: u -> l
  cuUpdLocation :: l -> u -> u
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
-- Locatable
-------------------------------------------------------------------------

class FileLocatable x loc | loc -> x where		-- funcdep has unlogical direction, but well...
  fileLocation   :: x -> loc
  noFileLocation :: loc

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

instance Show (CompileRunState err) where
  show CRSOk				= "CRSOk"
  show CRSFail				= "CRSFail"
  show CRSStopSeq			= "CRSStopSeq"
  show CRSStopAllSeq		= "CRSStopAllSeq"
  show CRSStop				= "CRSStop"
  show (CRSFailErrL _ _ _)	= "CRSFailErrL"
  show (CRSErrInfoL _ _ _)	= "CRSErrInfoL"

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
  = "CR" >#< show (crState cr) >|< ":" >#<
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

crCUState :: (Ord n,CompileUnit u n l s,CompileUnitState s) => n -> CompileRun n u i e -> s
crCUState modNm cr = maybe cusUnk cuState (crMbCU modNm cr)

crCUFPath :: (Ord n,CompileUnit u n l s) => n -> CompileRun n u i e -> FPath
crCUFPath modNm cr = maybe emptyFPath cuFPath (crMbCU modNm cr)

crCULocation :: (Ord n,FileLocatable u loc) => n -> CompileRun n u i e -> loc
crCULocation modNm cr = maybe noFileLocation fileLocation (crMbCU modNm cr)

-------------------------------------------------------------------------
-- Find file for FPath
-------------------------------------------------------------------------

cpFindFileForNameOrFPath :: FPATH n => String -> n -> FPath -> [(String,FPath)]
cpFindFileForNameOrFPath loc _ fp = searchFPathFromLoc loc fp

cpFindFilesForFPathInLocations
  :: ( Ord n
     , FPATH n, FileLocatable u loc, Show loc
     , CompileUnitState s,CompileRunError e p,CompileUnit u n loc s,CompileModName n,CompileRunStateInfo i n p
     ) => (loc -> n -> FPath -> [(loc,FPath)]) -> (FPath -> loc -> res)
          -> Bool -> [(String,s)] -> [loc] -> Maybe n -> Maybe FPath -> CompilePhase n u i e [res]
cpFindFilesForFPathInLocations getfp putres stopAtFirst suffs locs mbModNm mbFp
  = do { cr <- get
       ; let cus = maybe cusUnk (flip crCUState cr) mbModNm
       ; if cusIsUnk cus
          then do { let fp = maybe (mkFPath $ panicJust ("cpFindFileForFPath") $ mbModNm) id mbFp
                        modNm = maybe (mkCMNm $ fpathBase $ fp) id mbModNm
                  ; fpsFound <- lift (searchLocationsForReadableFiles (\l f -> getfp l modNm f) stopAtFirst locs (map fst suffs) fp)
                  ; case fpsFound of
                      []
                        -> do { cpSetErrs (creMkNotFoundErrL (crsiImportPosOfCUKey modNm (crStateInfo cr)) (fpathToStr fp) (map show locs))
                              ; return []
                              }
                      ffs@((ff,loc):_)
                        -> do { cpUpdCU modNm (cuUpdLocation loc . cuUpdFPath ff . cuUpdState cus . cuUpdKey modNm)
                              ; return (map (uncurry putres) ffs)
                              }
                        where cus = case lookup (fpathSuff ff) suffs of
                                      Just c  -> c
                                      Nothing -> case lookup "*" suffs of
                                                   Just c  -> c
                                                   Nothing -> cusUnk
                  }
          else return (maybe [] (\nm -> [putres (crCUFPath nm cr) (crCULocation nm cr)]) mbModNm)
       }

cpFindFilesForFPath
  :: ( Ord n
     , FPATH n, FileLocatable u String
     , CompileUnitState s,CompileRunError e p,CompileUnit u n String s,CompileModName n,CompileRunStateInfo i n p
     ) => Bool -> [(String,s)] -> [String] -> Maybe n -> Maybe FPath -> CompilePhase n u i e [FPath]
cpFindFilesForFPath
  = cpFindFilesForFPathInLocations cpFindFileForNameOrFPath (\fp _ -> fp)

cpFindFileForFPath
  :: ( Ord n
     , FPATH n, FileLocatable u String
     , CompileUnitState s,CompileRunError e p,CompileUnit u n String s,CompileModName n,CompileRunStateInfo i n p
     ) => [(String,s)] -> [String] -> Maybe n -> Maybe FPath -> CompilePhase n u i e (Maybe FPath)
cpFindFileForFPath suffs sp mbModNm mbFp
  = do { fps <- cpFindFilesForFPath True suffs sp mbModNm mbFp
       ; return (listToMaybe fps)
       }

-------------------------------------------------------------------------
-- Gather all imports
-------------------------------------------------------------------------

cpImportGatherFromMods
  :: (Show n,Ord n,CompileUnit u n l s,CompileRunError e p,CompileUnitState s)
       => (Maybe prev -> n -> CompilePhase n u i e (x,Maybe prev)) -> [n] -> CompilePhase n u i e ()
cpImportGatherFromMods imp1Mod modNmL
  = do { cr <- get
       ; cpSeq (   [ one Nothing modNm | modNm <- modNmL ]
                ++ [ cpImportScc ]
               )
       }
  where one prev modNm
          = do { (_,new) <- imp1Mod prev modNm
               ; cpHandleErr
               ; imps new modNm
               }
        imps prev m
          = do { cr <- get
               ; let impL m = [ i | i <- cuImports (crCU m cr), not (cusIsImpKnown (crCUState i cr)) ]
               ; cpSeq (map (\n -> one prev n) (impL m))
               }

cpImportGather
  :: (Show n,Ord n,CompileUnit u n l s,CompileRunError e p,CompileUnitState s)
       => (n -> CompilePhase n u i e ()) -> n -> CompilePhase n u i e ()
cpImportGather imp1Mod modNm
  = cpImportGatherFromMods
      (\_ n -> do { r <- imp1Mod n
                  ; return (r,Nothing)
                  }
      )
      [modNm]

crImportDepL :: (CompileUnit u n l s) => CompileRun n u i e -> [(n,[n])]
crImportDepL = map (\cu -> (cuKey cu,cuImports cu)) . Map.elems . crCUCache

cpImportScc :: (Ord n,CompileUnit u n l s) => CompilePhase n u i e ()
cpImportScc = modify (\cr -> (cr {crCompileOrder = scc (crImportDepL cr)}))


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

cpUpdCUM :: (Ord n,CompileUnit u n l s) => n -> (u -> IO u) -> CompilePhase n u i e ()
cpUpdCUM modNm upd
  = do { cr <- get
       ; cu <- lift (maybe (upd cuDefault) upd (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }


cpUpdCUWithKey :: (Ord n,CompileUnit u n l s) => n -> (n -> u -> (n,u)) -> CompilePhase n u i e n
cpUpdCUWithKey modNm upd
  = do { cr <- get
       ; let (modNm',cu) = (maybe (upd modNm cuDefault) (upd modNm) (crMbCU modNm cr))
       ; put (cr {crCUCache = Map.insert modNm' cu $ Map.delete modNm $ crCUCache cr})
       ; return modNm'
       }

cpUpdCU :: (Ord n,CompileUnit u n l s) => n -> (u -> u) -> CompilePhase n u i e ()
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

-- sequence of phases, each may stop the whole sequencing
cpSeq :: CompileRunError e p => [CompilePhase n u i e ()] -> CompilePhase n u i e ()
cpSeq []     = return ()
cpSeq (a:as) = do { a
                  ; cpHandleErr
                  ; cr <- get
                  ; case crState cr of
                      CRSOk         -> cpSeq as
                      CRSStopSeq    -> cpSetOk
                      CRSStopAllSeq -> cpSetStopAllSeq
                      _             -> return ()
                  }

-- conditional sequence
cpSeqWhen :: CompileRunError e p => Bool -> [CompilePhase n u i e ()] -> CompilePhase n u i e ()
cpSeqWhen True as = cpSeq as
cpSeqWhen _    _  = return ()

-- handle possible error in sequence
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

