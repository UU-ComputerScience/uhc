-------------------------------------------------------------------------
-- Combinators for a compile run
-------------------------------------------------------------------------

module CompileRun
  ( CompileRunState(..)
  , CompileRun(..)
  , CompileUnit(..)
  , CompileUnitState(..)
  , CompileRunError(..)
  , CompileModName(..),
  , CompileRunStateInfo(..)
  
  , mkEmptyCompileRun

  , liftCR
  , crCU, crMbCU, crUpdCU
  , crSetFail, crSetErrs, crSetLimitErrs, crSetInfos
  , crHandle1, crSeq

  , crCUState, crCUFPath
  , crFindFileForFPath
  
  , crImportGather
  
  , crPP, crPPMsg
  , ppCR
  )
  where

import Maybe
import System.Exit
import IO
import qualified Data.Map as Map
import UU.Pretty
import UU.DData.Scc as Scc
import Utils( panicJust )
import PPUtils
import FPath

-------------------------------------------------------------------------
-- Interfacing with actual state info
-------------------------------------------------------------------------

class CompileModName n where
  mkCMNm      	:: String -> n

class CompileUnitState s where
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
  | CRSFailErrL [err] (Maybe Int)
  | CRSErrInfoL String Bool [err]

data CompileRun nm unit info err
  = CompileRun
      { crCUCache       :: Map.Map nm unit
      , crCompileOrder  :: [[nm]]
      , crTopModNm      :: nm
      , crState         :: CompileRunState err
      , crStateInfo     :: info
      }

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
      >-< ppCommaList (map ppCommaList $ crCompileOrder $ cr)
      )

crPP :: (PP n,PP u) => String -> CompileRun n u i e -> IO (CompileRun n u i e)
crPP m cr = do { putStrLn (m ++ ":") ; hPutPPLn stdout (ppCR cr) ; return cr }

crPPMsg :: (PP m) => m -> CompileRun n u i e -> IO (CompileRun n u i e)
crPPMsg m cr = do { hPutPPLn stdout (pp m) ; return cr }

-------------------------------------------------------------------------
-- State manipulation, sequencing
-------------------------------------------------------------------------

liftCR :: (a -> b) -> a -> IO b
liftCR f = \a -> return (f a)

crHandle1 :: CompileRunError e p => (a -> IO b) -> (a -> b) -> (a -> CompileRunState e) -> IO a -> IO b
crHandle1 action err errs it
  = do { v <- it
       ; case errs v of
           CRSFailErrL es (Just lim)
             -> do { let (showErrs,omitErrs) = splitAt lim es
                   ; putErr' (if null omitErrs then return () else hPutStrLn stderr "... and more errors") showErrs
                   ; failOrNot es v
                   }
           CRSFailErrL es Nothing
             -> do { putErr' (return ()) es
                   ; failOrNot es v
                   }
           CRSErrInfoL about doPrint is
             -> do { if null is then return () else hPutPPLn stderr (about >#< "found errors" >-< e)
                   ; if not (null is) then return (err v) else action v
                   }
             where e = if doPrint then crePPErrL is else empty
           CRSFail
             -> do { exitFailure 
                   ; return (err v)
                   }
           CRSOk
             -> action v
       }
  where putErr' m e   = {- if null e
                        then return ()
                        else -}
                             do { hPutPPLn stderr (crePPErrL e)
                                ; m
                                -- ; if errLIsFatal e then exitFailure else return ()
                                }
        failOrNot es v = if creAreFatal es then return (err v) else action v


crSetFail :: CompileRun n u i e -> CompileRun n u i e
crSetFail cr = cr {crState = CRSFail}

crSetOk :: CompileRun n u i e -> CompileRun n u i e
crSetOk cr = cr {crState = CRSOk}

crSetErrs' :: Maybe Int -> [e] -> CompileRun n u i e -> CompileRun n u i e
crSetErrs' limit es cr
  = case es of
      [] -> cr
      _  -> cr {crState = CRSFailErrL es limit}

crSetErrs :: [e] -> CompileRun n u i e -> IO (CompileRun n u i e)
crSetErrs e = liftCR (crSetErrs' Nothing e)

crSetLimitErrs :: Int -> [e] -> CompileRun n u i e -> IO (CompileRun n u i e)
crSetLimitErrs l e = liftCR (crSetErrs' (Just l) e)

crSetInfos' :: String -> Bool -> [e] -> CompileRun n u i e -> CompileRun n u i e
crSetInfos' msg dp is cr
  = case is of
      [] -> cr
      _  -> cr {crState = CRSErrInfoL msg dp is}

crSetInfos :: String -> Bool -> [e] -> CompileRun n u i e -> IO (CompileRun n u i e)
crSetInfos msg dp is = liftCR (crSetInfos' msg dp is)

crMbCU :: Ord n => n -> CompileRun n u i e -> Maybe u
crMbCU modNm cr = Map.lookup modNm (crCUCache cr)

crCU :: (Show n,Ord n) => n -> CompileRun n u i e -> u
crCU modNm = panicJust ("crCU: " ++ show modNm) . crMbCU modNm

crUpdCU :: (Ord n,CompileUnit u n s) => n -> (u -> IO u) -> CompileRun n u i e -> IO (CompileRun n u i e)
crUpdCU modNm upd cr
  = do { cu <- maybe (upd cuDefault) upd (crMbCU modNm cr)
       ; return (cr {crCUCache = Map.insert modNm cu (crCUCache cr)})
       }

crSeq :: CompileRunError e p => [CompileRun n u i e -> IO (CompileRun n u i e)] -> CompileRun n u i e -> IO (CompileRun n u i e)
crSeq []      cr = return cr
crSeq (a:as)  cr = crHandle1 (\cr -> crSeq as (crSetOk cr)) crSetFail crState (a cr)

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

crFindFileForFPath
  :: (Ord n,Show n,CompileUnitState s,CompileRunError e p,CompileUnit u n s,CompileModName n,CompileRunStateInfo i n p)
       => Map.Map String s -> [String] -> Maybe n -> Maybe FPath -> CompileRun n u i e -> IO (CompileRun n u i e,Maybe FPath)
crFindFileForFPath suffs sp mbModNm mbFp cr
  = if cusIsUnk cus
    then do { let fp = maybe (mkFPath $ show $ panicJust ("crFindFileForFPath") $ mbModNm) id mbFp
                  modNm = maybe (mkCMNm $ fpathBase $ fp) id mbModNm
            ; mbFpFound <- searchPathForReadableFile sp (Map.keys suffs) fp
            ; case mbFpFound of
                Nothing
                  -> do { cr' <- crSetErrs (creMkNotFoundErrL (crsiImportPosOfCUKey modNm (crStateInfo cr)) (fpathToStr fp) sp) cr
                        ; return (cr',Nothing)
                        }
                Just ff
                  -> do { cr' <- crUpdCU modNm (\cu -> return (cuUpdFPath ff $ cuUpdState cus $ cuUpdKey modNm $ cu)) cr
                        ; return (cr',Just ff)
                        }
                  where cus = Map.findWithDefault cusUnk (fpathSuff ff) suffs
                        
            }
    else return (cr,maybe Nothing (\nm -> Just (crCUFPath nm cr)) mbModNm)
  where cus = maybe cusUnk (flip crCUState cr) mbModNm

-------------------------------------------------------------------------
-- Gather all imports
-------------------------------------------------------------------------

crImportGather
  :: (Show n,Ord n,CompileUnit u n s,CompileRunError e p,CompileUnitState s)
       => (n -> CompileRun n u i e -> IO (CompileRun n u i e)) -> n -> CompileRun n u i e -> IO (CompileRun n u i e)
crImportGather imp1Mod modNm
  = crSeq [imp1Mod modNm, imps modNm, crImportScc]
  where imps m cr
          = crSeq (map (\n -> crSeq [imp1Mod n, imps n]) impL) cr
          where impL = [ i | i <- cuImports (crCU m cr), not (cusIsImpKnown (crCUState i cr)) ]

crImportDepL :: (CompileUnit u n s) => CompileRun n u i e -> [(n,[n])]
crImportDepL = map (\cu -> (cuKey cu,cuImports cu)) . Map.elems . crCUCache

crImportScc :: (Ord n,CompileUnit u n s) => CompileRun n u i e -> IO (CompileRun n u i e)
crImportScc cr = return (cr {crCompileOrder = Scc.scc (crImportDepL cr)})
