

-- UUAGC 0.9.39.1 (src/shuffle/CDocInline.ag)
module CDocInline(cdocInlineCDocIO, InlineCDocSt, InlineCDocIO) where

import qualified Data.Map as Map
import Network.URI
import System.Process
import System.Exit
-- import System.Posix.Temp( mkstemp )
import System.Directory( removeFile )
import System.IO
import CDoc
import CDocCommon


wrapAGCDoc_T :: NmChMp -> T_AGCDocItf -> Syn_AGCDocItf
wrapAGCDoc_T nmChMp d
  = wrap_AGCDocItf d
      (Inh_AGCDocItf
         { nmChMp_Inh_AGCDocItf = nmChMp
         })

wrapCDoc :: NmChMp -> CDoc -> Syn_AGCDocItf
wrapCDoc m d = wrapAGCDoc_T m (sem_AGCDocItf (AGCDocItf_AGItf d))

cdocInlineCDocIO :: CDoc -> (CDoc,InlineCDocIO)
cdocInlineCDocIO d
  = (ilRepl_Syn_AGCDocItf r,ilIO_Syn_AGCDocItf r)
  where r = wrapCDoc Map.empty d

type InlineCDocSt = (NmChMp,ErrM)
type InlineCDocIO = InlineCDocSt -> IO InlineCDocSt

inlineCDocEmp :: InlineCDocIO
inlineCDocEmp = return

inlineCDocAdd :: InlineCDocIO -> InlineCDocIO -> InlineCDocIO
inlineCDocAdd i1 i2 s
  = do { s1@(m1,e1) <- i1 s
       ; if Map.null e1
         then i2 (m1,Map.empty)
         else i2 (Map.empty,e1)
       }
-- AGCDocItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmChMp               : NmChMp
      synthesized attributes:
         ilIO                 : InlineCDocIO
         ilRepl               : CDoc 
   alternatives:
      alternative AGItf:
         child cdoc           : CDoc 
-}
-- cata
sem_AGCDocItf :: AGCDocItf  ->
                 T_AGCDocItf 
sem_AGCDocItf (AGCDocItf_AGItf _cdoc )  =
    (sem_AGCDocItf_AGItf (sem_CDoc _cdoc ) )
-- semantic domain
type T_AGCDocItf  = NmChMp ->
                    ( InlineCDocIO,CDoc )
data Inh_AGCDocItf  = Inh_AGCDocItf {nmChMp_Inh_AGCDocItf :: NmChMp}
data Syn_AGCDocItf  = Syn_AGCDocItf {ilIO_Syn_AGCDocItf :: InlineCDocIO,ilRepl_Syn_AGCDocItf :: CDoc }
wrap_AGCDocItf :: T_AGCDocItf  ->
                  Inh_AGCDocItf  ->
                  Syn_AGCDocItf 
wrap_AGCDocItf sem (Inh_AGCDocItf _lhsInmChMp )  =
    (let ( _lhsOilIO,_lhsOilRepl) = sem _lhsInmChMp 
     in  (Syn_AGCDocItf _lhsOilIO _lhsOilRepl ))
sem_AGCDocItf_AGItf :: T_CDoc  ->
                       T_AGCDocItf 
sem_AGCDocItf_AGItf cdoc_  =
    (\ _lhsInmChMp ->
         (let _cdocOcpos :: CPos
              _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              _cdocOnmChMp :: NmChMp
              _cdocIilIO :: InlineCDocIO
              _cdocIilRepl :: CDoc 
              -- "src/shuffle/CDocCommonAG.ag"(line 12, column 17)
              _cdocOcpos =
                  CPos emptyFPath 0
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  _cdocIilIO
              -- copy rule (up)
              _lhsOilRepl =
                  _cdocIilRepl
              -- copy rule (down)
              _cdocOnmChMp =
                  _lhsInmChMp
              ( _cdocIilIO,_cdocIilRepl) =
                  cdoc_ _cdocOcpos _cdocOnmChMp 
          in  ( _lhsOilIO,_lhsOilRepl)))
-- CDoc --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cpos                 : CPos
         nmChMp               : NmChMp
      synthesized attributes:
         ilIO                 : InlineCDocIO
         ilRepl               : SELF 
   alternatives:
      alternative Emp:
         visit 0:
            local ilRepl      : _
      alternative Hor:
         child ldoc           : CDoc 
         child rdoc           : CDoc 
         visit 0:
            local ilRepl      : _
      alternative Inl:
         child uref           : {URef}
         visit 0:
            local ilNm        : _
            local ilRepl      : _
      alternative Pos:
         child cpos           : {CPos}
         child cdoc           : CDoc 
         visit 0:
            local ilRepl      : _
      alternative Ref:
         child cref           : {CRef}
         child mbVariantReqm  : {Maybe VariantReqm}
         child chDest         : {ChDest}
         visit 0:
            local ilRepl      : _
      alternative Str:
         child str            : {String}
         visit 0:
            local ilRepl      : _
      alternative Ver:
         child ldoc           : CDoc 
         child rdoc           : CDoc 
         visit 0:
            local ilRepl      : _
-}
-- cata
sem_CDoc :: CDoc  ->
            T_CDoc 
sem_CDoc (CDoc_Emp )  =
    (sem_CDoc_Emp )
sem_CDoc (CDoc_Hor _ldoc _rdoc )  =
    (sem_CDoc_Hor (sem_CDoc _ldoc ) (sem_CDoc _rdoc ) )
sem_CDoc (CDoc_Inl _uref )  =
    (sem_CDoc_Inl _uref )
sem_CDoc (CDoc_Pos _cpos _cdoc )  =
    (sem_CDoc_Pos _cpos (sem_CDoc _cdoc ) )
sem_CDoc (CDoc_Ref _cref _mbVariantReqm _chDest )  =
    (sem_CDoc_Ref _cref _mbVariantReqm _chDest )
sem_CDoc (CDoc_Str _str )  =
    (sem_CDoc_Str _str )
sem_CDoc (CDoc_Ver _ldoc _rdoc )  =
    (sem_CDoc_Ver (sem_CDoc _ldoc ) (sem_CDoc _rdoc ) )
-- semantic domain
type T_CDoc  = CPos ->
               NmChMp ->
               ( InlineCDocIO,CDoc )
sem_CDoc_Emp :: T_CDoc 
sem_CDoc_Emp  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  inlineCDocEmp
              -- self rule
              _ilRepl =
                  CDoc_Emp
              -- self rule
              _lhsOilRepl =
                  _ilRepl
          in  ( _lhsOilIO,_lhsOilRepl)))
sem_CDoc_Hor :: T_CDoc  ->
                T_CDoc  ->
                T_CDoc 
sem_CDoc_Hor ldoc_ rdoc_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              _ldocOcpos :: CPos
              _ldocOnmChMp :: NmChMp
              _rdocOcpos :: CPos
              _rdocOnmChMp :: NmChMp
              _ldocIilIO :: InlineCDocIO
              _ldocIilRepl :: CDoc 
              _rdocIilIO :: InlineCDocIO
              _rdocIilRepl :: CDoc 
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  _ldocIilIO `inlineCDocAdd` _rdocIilIO
              -- self rule
              _ilRepl =
                  CDoc_Hor _ldocIilRepl _rdocIilRepl
              -- self rule
              _lhsOilRepl =
                  _ilRepl
              -- copy rule (down)
              _ldocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _ldocOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _rdocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _rdocOnmChMp =
                  _lhsInmChMp
              ( _ldocIilIO,_ldocIilRepl) =
                  ldoc_ _ldocOcpos _ldocOnmChMp 
              ( _rdocIilIO,_rdocIilRepl) =
                  rdoc_ _rdocOcpos _rdocOnmChMp 
          in  ( _lhsOilIO,_lhsOilRepl)))
sem_CDoc_Inl :: URef ->
                T_CDoc 
sem_CDoc_Inl uref_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOilRepl :: CDoc 
              _lhsOilIO :: InlineCDocIO
              -- "src/shuffle/CDocInline.ag"(line 63, column 17)
              _ilNm =
                  Nm uref_
              -- "src/shuffle/CDocInline.ag"(line 64, column 17)
              _lhsOilRepl =
                  CDoc_Ref _ilNm Nothing ChHere
              -- "src/shuffle/CDocInline.ag"(line 64, column 17)
              _lhsOilIO =
                  \(m,e)
                      -> let dflt = (m,Map.insert _lhsIcpos (Err_UndefURI _lhsIcpos uref_) e)
                             mkc n c m
                                  = Map.insert n (NmChInfo n ChHere res (const res)) m
                                  where res = Just (cd c)
                         in  case takeWhile (/= ':') uref_ of
                               "file"
                                 -> do { let mu = parseURIReference uref_
                                       ; mh <- maybe (return Nothing) openURI mu
                                       ; case mh of
                                           Just h -> do { c <- hGetContents h
                                                        ; return (mkc _ilNm c m,e)
                                                        }
                                           _      -> return dflt
                                       }
                               "exec"
                                 -> do { let cmd = drop 5 uref_
                                       ; let tmpF = "shuffleXXXXXX.tmp"
                                       ; exitCode <- system (cmd ++ " > " ++ tmpF)
                                       ; case exitCode of
                                           ExitSuccess -> do { h <- openFile tmpF ReadMode
                                                             ; c <- hGetContents h
                                                             ; removeFile tmpF
                                                             ; return (mkc _ilNm c m,e)
                                                             }
                                           _           -> return (m,Map.insert _lhsIcpos (Err_Exec _lhsIcpos cmd (show exitCode)) e)
                                       }
                               _ -> return dflt
              -- self rule
              _ilRepl =
                  CDoc_Inl uref_
          in  ( _lhsOilIO,_lhsOilRepl)))
sem_CDoc_Pos :: CPos ->
                T_CDoc  ->
                T_CDoc 
sem_CDoc_Pos cpos_ cdoc_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _cdocOcpos :: CPos
              _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              _cdocOnmChMp :: NmChMp
              _cdocIilIO :: InlineCDocIO
              _cdocIilRepl :: CDoc 
              -- "src/shuffle/CDocCommonAG.ag"(line 15, column 17)
              _cdocOcpos =
                  cpos_
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  _cdocIilIO
              -- self rule
              _ilRepl =
                  CDoc_Pos cpos_ _cdocIilRepl
              -- self rule
              _lhsOilRepl =
                  _ilRepl
              -- copy rule (down)
              _cdocOnmChMp =
                  _lhsInmChMp
              ( _cdocIilIO,_cdocIilRepl) =
                  cdoc_ _cdocOcpos _cdocOnmChMp 
          in  ( _lhsOilIO,_lhsOilRepl)))
sem_CDoc_Ref :: CRef ->
                (Maybe VariantReqm) ->
                ChDest ->
                T_CDoc 
sem_CDoc_Ref cref_ mbVariantReqm_ chDest_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  inlineCDocEmp
              -- self rule
              _ilRepl =
                  CDoc_Ref cref_ mbVariantReqm_ chDest_
              -- self rule
              _lhsOilRepl =
                  _ilRepl
          in  ( _lhsOilIO,_lhsOilRepl)))
sem_CDoc_Str :: String ->
                T_CDoc 
sem_CDoc_Str str_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  inlineCDocEmp
              -- self rule
              _ilRepl =
                  CDoc_Str str_
              -- self rule
              _lhsOilRepl =
                  _ilRepl
          in  ( _lhsOilIO,_lhsOilRepl)))
sem_CDoc_Ver :: T_CDoc  ->
                T_CDoc  ->
                T_CDoc 
sem_CDoc_Ver ldoc_ rdoc_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOilIO :: InlineCDocIO
              _lhsOilRepl :: CDoc 
              _ldocOcpos :: CPos
              _ldocOnmChMp :: NmChMp
              _rdocOcpos :: CPos
              _rdocOnmChMp :: NmChMp
              _ldocIilIO :: InlineCDocIO
              _ldocIilRepl :: CDoc 
              _rdocIilIO :: InlineCDocIO
              _rdocIilRepl :: CDoc 
              -- use rule "src/shuffle/CDocInline.ag"(line 60, column 32)
              _lhsOilIO =
                  _ldocIilIO `inlineCDocAdd` _rdocIilIO
              -- self rule
              _ilRepl =
                  CDoc_Ver _ldocIilRepl _rdocIilRepl
              -- self rule
              _lhsOilRepl =
                  _ilRepl
              -- copy rule (down)
              _ldocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _ldocOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _rdocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _rdocOnmChMp =
                  _lhsInmChMp
              ( _ldocIilIO,_ldocIilRepl) =
                  ldoc_ _ldocOcpos _ldocOnmChMp 
              ( _rdocIilIO,_rdocIilRepl) =
                  rdoc_ _rdocOcpos _rdocOnmChMp 
          in  ( _lhsOilIO,_lhsOilRepl)))