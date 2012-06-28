

-- UUAGC 0.9.39.1 (src/shuffle/MainAG.ag)
module MainAG where

import Network.URI
import System.IO
import Control.Monad
import Data.Array
import Data.List
import qualified Data.Map as Map
import Common
import CDoc
import CDocCommon
import CDocSubst
import CDocInline
import qualified Data.Set as Set
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils(initlast)

-- for debugging:
-- import EH.Util.Utils (tr, trp, wordsBy)
-- import EH.Util.Pretty

wrapAG_T :: Opts -> FPath -> XRefExcept -> NmChMp -> T_AGItf -> Syn_AGItf
wrapAG_T opts fp xr nmChMp pres
  = wrap_AGItf pres
      (Inh_AGItf
         { opts_Inh_AGItf = opts {optBaseName = mbBaseName, optBaseFPath = fp, optDefs = mbDefs `Map.union` optDefs opts}
         , xrefExcept_Inh_AGItf = xr
         , nmChMp_Inh_AGItf = nmChMp
         })
  where mbBaseName = maybe (Just (fpathBase fp)) Just (optBaseName opts)
        mbDefs = maybe Map.empty (\n -> Map.fromList [("basename",n),("base",n)]) mbBaseName


cdocSubstInline :: NmChMp -> CDoc -> IO (CDoc,Set.Set Nm,ErrM)
cdocSubstInline m d
  = do { let (d2,s,e) = cdocSubst m d
       ; if Map.null e
         then do { let (d3,il) = cdocInlineCDocIO d2
                 ; (im,ie) <- il (Map.empty,Map.empty)
                 ; if Map.null ie
                   then do { let (d4,_,es) = cdocSubst im d3
                           ; return (d4,s,es)
                           }
                   else return (d3,s,ie)
                 }
         else return (d2,s,e)
       }


instance CD VariantOffer where
  cd = cd . mkNm

cmpByVariantRefOrder :: VariantRefOrder -> VariantRef -> VariantRef -> Ordering
cmpByVariantRefOrder vo v1 v2
  = maybe EQ id $ listToMaybe $ catMaybes $ map c vo
  where  c o = do { i1 <- elemIndex v1 o
                  ; i2 <- elemIndex v2 o
                  ; return (compare i1 i2)
                  }

variantOfferAllLE_3 :: VariantRefOrder -> VariantReqm -> [VariantRef]
variantOfferAllLE_3 vo v
  = let allN = nub $ sort $ concat vo
        nrN = length allN
        nsN = [(0::Int) .. nrN-1]
        ixOf' v = elemIndex v allN
        ixOfV v = maybe Nothing ixOf' (mbVariantReqmRef v)
        ixOf v = maybe 0 id (ixOf' v)
        voPrefixes
          = map (\p@((v,_):_) -> (ixOf v,map ixOf $ nub $ sort $ (v:) $ concat $ map snd p))
          $ groupBy (\(v1,_) (v2,_) -> v1 == v2)
          $ sortBy (\(v1,_) (v2,_) -> compare v1 v2)
          $ concat
          $ map (\o -> zip o (inits o))
          $ vo
        m1 = map
                (\(n,ns)
                    -> map snd . sort $ (zip (ns) (repeat True) ++ zip (nsN \\ ns) (repeat False))
                )
                voPrefixes
        m2 = array (0,nrN-1) (zip nsN (map (\r -> array (0,nrN-1) (zip nsN r)) m1))
        m3 = foldr
                (\n m
                    -> foldr
                        (\i m -> m // [(i,m ! i // [ (j,m ! i ! n && m ! n ! j || m ! i ! j) | j <- nsN ])])
                        m nsN
                )
                m2 nsN
        nsV = maybe [] (\i -> assocs (m3 ! i)) (ixOfV v)
        allN' = case v of
                  VReqmAll -> allN
                  _        -> [ allN !! i | (i,b) <- nsV, b ]
     in sortBy (cmpByVariantRefOrder vo) $ nub $ sort allN'

variantOfferAllLE_4 :: Opts -> VariantRefOrderMp
variantOfferAllLE_4 opts
  = vm
  where vo = if optsHasNoVariantRefOrder opts then variantRefOrderDefault else optVariantRefOrder opts
        vs = variantOfferAllLE_3 vo (optGenReqm opts)
        vm = Map.fromList $ zip (sortBy (cmpByVariantRefOrder vo) vs) [1..]

variantRefOrderDefault :: VariantRefOrder
variantRefOrderDefault = [take 1 (map variantRefFromTop [1..])]

isAllowedCompilerVariant :: CompilerRestriction -> [Int] -> Bool
isAllowedCompilerVariant (Restricted mLower mUpper) v
  = mLower `leq` (Just v) && (Just v) `leq` mUpper
  where
    leq Nothing _ = True
    leq _ Nothing = True
    leq (Just p) (Just q) = p <= q


data XRefKind = XRHsDef | XRAgAttrDef | XRAgAltDef | XRAgSemDef | XRHsUse | XRAgAttrUse deriving Show
data XRef = XRef { xrKind :: XRefKind, xrKeyL :: [String] } deriving Show

xrMainKey :: XRef -> String
xrMainKey = head . xrKeyL

xrKindIsDefining :: XRefKind -> Bool
xrKindIsDefining XRHsDef = True
xrKindIsDefining XRAgAttrDef = True
xrKindIsDefining XRAgAltDef = True
xrKindIsDefining XRAgSemDef = True
xrKindIsDefining _ = False

xrIsDefining :: XRef -> Bool
xrIsDefining = xrKindIsDefining . xrKind

type XRefL = Seq.FastSeq XRef
type XRefExcept = Set.Set String

passXR :: XRefExcept -> String -> ([XRef],Int) -> ([XRef],Int)
passXR exc r xr = if Set.member r exc then ([],0) else xr


data HideInfo
  = HideInfo
      { hiNm        :: Nm
      , hiDescr     :: CDoc
      , hiSeqNr     :: Int
      , hiChDest    :: ChDest
      , hiMbCD      :: Maybe CDoc
      , hiChFullNm  :: Nm
      }

type HideMp = Map.Map Nm HideInfo


mbCDocCmb :: Maybe CDoc -> Maybe CDoc -> Maybe CDoc
mbCDocCmb c1 c2 = maybe c1 (Just . (maybe CDoc_Emp id c1 `CDoc_Ver`)) c2


mkCDocCmb :: MkCDoc -> MkCDoc -> MkCDoc
mkCDocCmb c1 c2 = \sel -> maybe (c1 sel) (Just . (maybe CDoc_Emp id (c1 sel) `CDoc_Ver`)) (c2 sel)

mkCDocEmpty :: MkCDoc
mkCDocEmpty = const Nothing


data VariantChunkInfo
  = VariantChunkInfo
      { vciLineNr   		:: Int
      , vciSeqNr    		:: Int
      , vciVariantOffer     :: VariantOffer
      , vciChunkRef         :: ChunkRef
      , vciMinusL   		:: [ChunkRef]
      , vciChKind   		:: ChKind
      , vciChDest   		:: ChDest
      , vciMbModNm  		:: Maybe String
      , vciImps     		:: [String]
      , vciExps     		:: [String]
      , vciMbCD     		:: Maybe CDoc
      , vciMkCD     		:: MkCDoc
      , vciXRefL    		:: [XRef]
      } deriving Show

type VariantChunkInfoM = [(VariantOffer,[VariantChunkInfo])]

vciMToL :: VariantChunkInfoM -> [VariantChunkInfo]
vciMToL = concat . map snd

vciFullNm :: VariantChunkInfo -> Nm
vciFullNm i = mkNm (vciChunkRef i)

instance Eq VariantChunkInfo where
  i1 == i2 = vciVariantOffer i1 == vciVariantOffer i2

instance Ord VariantChunkInfo where
  compare i1 i2 = vciVariantOffer i1 `compare` vciVariantOffer i2

vciSortBySeqNr :: [VariantChunkInfo] -> [VariantChunkInfo]
vciSortBySeqNr = sortBy (\v1 v2 -> vciSeqNr v1 `compare` vciSeqNr v2)

vciVariantOfferFilter :: (VariantOffer -> Bool) -> [VariantChunkInfo] -> [VariantChunkInfo]
vciVariantOfferFilter f = filter (f . vciVariantOffer)

vciVariantOfferGroup :: [VariantChunkInfo] -> [[VariantChunkInfo]]
vciVariantOfferGroup = groupBy (\i1 i2 -> vciVariantOffer i1 == vciVariantOffer i2)

vciHasImpExp :: VariantChunkInfo -> Bool
vciHasImpExp i = not (null (vciImps i) && null (vciExps i))

vciIsPre :: VariantChunkInfo -> Bool
vciIsPre = (==VOfferPre) . vciVariantOffer

vciIsHS :: VariantChunkInfo -> Bool
vciIsHS = (==ChHS) . vciChKind

vciCD :: VariantChunkInfo -> CDoc
vciCD = maybe CDoc_Emp id . vciMbCD

vciHasCD :: VariantChunkInfo -> Bool
vciHasCD = isJust . vciMbCD

vciSplitPre :: [VariantChunkInfo] -> ([VariantChunkInfo],[VariantChunkInfo])
vciSplitPre = partition vciIsPre

vciTakePre :: VariantChunkInfoM -> ([VariantChunkInfo],VariantChunkInfoM)
vciTakePre is
  = case is of
        ((VOfferPre,p):r) -> (p,r)
        _            -> ([],is)

selectChunks :: Bool -> VariantReqm -> VariantRefOrderMp -> [VariantChunkInfo] -> [(VariantOffer,[VariantChunkInfo])]
selectChunks appMinus variantReqm allowedVariants agl
  = let (pre,nonPre)    = vciSplitPre agl
        vAndVciL        = map (\v -> let vreqm = variantReqmUpdRef variantReqm (variantOfferRef v)
                                     in  ( v
                                         , vciSortBySeqNr
                                           $ vciVariantOfferFilter
                                               (\offer -> variantReqmMatchOffer Nothing vreqm offer)
                                               nonPre
                              )          )
                              [variantOfferFromRef r | r <- Map.keys allowedVariants]
        isNotMinused
          = let minuses = if appMinus then [ m | (_,vciL) <- vAndVciL, ml <- map vciMinusL vciL, m <- ml ] else []
             in \i -> vciChunkRef i `notElem` minuses
     in filter
          ( not.null.snd )
          ( (VOfferPre,pre)
          : map (\(v,vciL) -> (v,filter isNotMinused vciL)) vAndVciL
          )

data Build
  = Build
      { bldBase     		:: String
      , bldVariantReqm      :: VariantReqm
      , bldCD       		:: CDoc
      , bldHideCD   		:: [(Nm,CDoc)]
      , bldNmChMp   		:: NmChMp
      }


chKindCmb ChPlain o = o
chKindCmb o       _ = o

chDestCmb ChHere  o = o
chDestCmb o       _ = o

chWrapCmb ChWrapPlain  o = o
chWrapCmb o            _ = o



haddockize :: CDoc -> CDoc
haddockize d
  = "{-|" .-. d .-. "-}"


linePragma :: String -> Opts -> String -> Int -> CDoc -> CDoc
linePragma pragcmt opts filename n c
  = if not (optLinePragmas opts) || cdIsEmpty c
    then c
    else CDoc_Ver (CDoc_Str ("{-" ++ pragcmt ++ " LINE "++show (n+1)++" \"" ++ filename ++ "\" " ++ pragcmt ++ "-}")) c

hsLinePragma
  = linePragma "#"

agLinePragma
  = linePragma ""


buildAGImps :: VariantChunkInfo -> CDoc
buildAGImps = cdVer . map (\imp -> "INCLUDE \"" .|. imp .|. ".ag\"") . vciImps

buildAG :: Opts -> (Bool -> CDoc -> CDoc) -> String -> VariantChunkInfoM -> CDoc
buildAG opts wrap fileBase is
  = let (pre,noPre) = vciTakePre is
        noPre' = vciMToL noPre
        h p  = "{" .-. p .-. "}"
        mk i = case vciChKind i of
                 ChHaddock -> h (haddockize (vciCD i))
                 ChHS      -> agLinePragma opts (fpathToStr $ optBaseFPath opts) (vciLineNr i) (h (vciCD i))
                 _         -> buildAGImps i .-. agLinePragma opts (fpathToStr $ optBaseFPath opts) (vciLineNr i) (vciCD i)
        ish  = filter vciIsHS noPre'
        buildImpExp = if optAGModHeader opts
                      then buildAGHSModImpExp fileBase ish
                      else h (buildHSModImpExp fileBase ish)
        pph  = if any vciHasImpExp ish
               then wrap True buildImpExp
               else CDoc_Emp
        cds  =   map (\i -> let vnm = vciFullNm i
                            in  (wrap (not (vciIsPre i)) (mk i))
                     )
               $ vciSortBySeqNr
               $ noPre'
        d = (if optPreamble opts then cdVer (map (wrap False . vciCD) pre) else CDoc_Emp)
            .-. pph
            .-. (cdVer cds)
     in d

buildAGHSModImpExp :: String -> [VariantChunkInfo] -> CDoc
buildAGHSModImpExp fileBase is = buildAGHSModuleHead fileBase is .-. "{" .-. buildHSImps is .-. "}"

buildAGHSModuleHead :: String -> [VariantChunkInfo] -> CDoc
buildAGHSModuleHead fileBase is
  = let ismie = [ i | i <- is   , isJust (vciMbModNm i) || not (null (vciExps i) && null (vciImps i)) ]
        isie  = [ i | i <- ismie, not (null (vciExps i) && null (vciImps i)) ]
        e = [ vciExps i | i <- isie, not (null (vciExps i)) ]
        m = catMaybes . map vciMbModNm $ ismie
        exps = cdListSepV "{" "}" ", " . map (cdListSep "" "" ", ") $ e
        modNm = if null m then fileBase else head m
     in "MODULE" .#. ("{" ++ modNm ++ "}") .#. exps


mkCmdNmDef :: CDoc -> CDoc -> CDoc
mkCmdNmDef = mkTexCmdDef "chunkCmdDef"

mkHideNmDef :: CDoc -> CDoc -> CDoc -> CDoc
mkHideNmDef = mkTexCmd3 "chunkHideDef"

mkHideNmRef :: CDoc -> CDoc
mkHideNmRef = mkTexCmdUse "chunkHideRef"

mkCmdNmUse :: CDoc -> CDoc
mkCmdNmUse = mkTexCmdUse' "chunkCmdUse"

mkCmdInx :: CDoc -> CDoc
mkCmdInx = mkTexCmdUse' "chunkIndex"

mkLabel :: CDoc -> CDoc
mkLabel = mkTexCmdUse' "label"

mkMetaInfo :: CDoc -> String -> CDoc
mkMetaInfo lab fileBase = mkLabel lab -- .-. mkTexCmdDef "chunkMetaDef" lab (cd fileBase)

buildLaTeX :: Opts -> (Bool -> CDoc -> CDoc) -> String -> VariantChunkInfoM -> CDoc
buildLaTeX opts wrap fileBase is
  = let (pre,noPre) = vciTakePre is
        noPre' = vciMToL noPre
        versions = nub $ map (variantOfferRefTop.vciVariantOffer) $ noPre'
        missing = if null versions then []
                                   else [minimum versions .. maximum versions] \\ versions
        mkInx
          = let styleFmt = if optWrapLhs2tex opts == ChWrapCode then "||" else "|"
             in \ix
                 -> let n = foldr1 (\x y -> y ++ "!" ++ x) . xrKeyL $ ix
                        dfmt = CDoc_Emp -- if xrIsDefining ix then text (styleFmt ++ "emph") else CDoc_Emp
                     in mkCmdInx (n .|. dfmt)
        mkContent = let mk = wrap True . vciCD
                     in if optIndex opts
                        then \i -> cdVer (map mkInx (vciXRefL i)) .-. mk i
                        else mk
        ppNoPreL
          =   map (\is -> let vnm = mkNm (vciVariantOffer (head is))
                              nm = mkNm fileBase `nmApd` vnm
                              cnm = cd nm
                              (nms,pps)
                                  = unzip
                                    . map (\(nr,i) -> let cn = cd (nm `nmApd` mkNm nr)
                                                          content = mkContent i
                                                       in ( cn
                                                          , mkCmdNmDef cn (mkMetaInfo cn fileBase .-. content)
                                                            .-. (let cna = cd (nm `nmApd` chunkRefNm (vciChunkRef i))
                                                                 in  mkCmdNmDef cna (mkMetaInfo cna fileBase .-. mkCmdNmUse cn)
                                                                )
                                                          )
                                          )
                                    . zip [(0::Int)..]
                                    $ is
                              content = cdVer pps .-. mkCmdNmDef cnm (mkMetaInfo cnm fileBase .-. cdVer (map mkCmdNmUse nms))
                           in (content)
                  )
            . vciVariantOfferGroup
            $ noPre'
        d = (if optPreamble opts then cdVer (map (wrap False . vciCD) pre) else CDoc_Emp)
            .-. cdVer ppNoPreL
            .-. cdVer (map (\v -> mkCmdNmDef (cdDots [cd fileBase,cd v]) CDoc_Emp) missing)
     in d


mkModNm :: [CDoc] -> CDoc
mkModNm = cdHor

buildHSImps :: [VariantChunkInfo] -> CDoc
buildHSImps = cdVer . map (cdVer . map ("import" .#.) . vciImps)

buildHSModuleHead :: String -> [VariantChunkInfo] -> CDoc
buildHSModuleHead fileBase is
  = let ismie = [ i | i <- is   , isJust (vciMbModNm i) || not (null (vciExps i) && null (vciImps i)) ]
        isie  = [ i | i <- ismie, not (null (vciExps i) && null (vciImps i)) ]
        -- e = filter (not.null) . map vciExps $ isie
        e = [ vciExps i | i <- isie, not (null (vciExps i)) ]
        m = catMaybes . map vciMbModNm $ ismie
        exps = if null e then CDoc_Emp
                         else cdListSepV "( " " )" ", " . map (cdListSep "" "" ", ") $ e
        modNm = if null m then fileBase else head m
     in "module" .#. modNm .-. {- indent 2 -} (exps .-. "where")

buildHSModImpExp :: String -> [VariantChunkInfo] -> CDoc
buildHSModImpExp fileBase is = buildHSModuleHead fileBase is .-. buildHSImps is

buildHS :: Opts -> (Bool -> CDoc -> CDoc) -> String -> VariantChunkInfoM -> CDoc
buildHS opts wrap fileBase is
  = let mk i = case vciChKind i of
                 ChHaddock -> haddockize (vciCD i)
                 _         -> vciCD i
        (pre,noPre) = vciTakePre is
        noPre' = vciMToL noPre
        ppMod = buildHSModImpExp fileBase (vciMToL is)
        ppNoPreL
          =   map  ( cdVer
                   . (map  (\i
                             -> hsLinePragma opts (fpathToStr $ optBaseFPath opts) (vciLineNr i)
                                             (wrap (vciHasCD i) (mk i))
                   ) )     )
            $ vciVariantOfferGroup
            $ vciSortBySeqNr
            $ noPre'
        isEmpty = all (isNothing.vciMbCD) noPre'
        ppNoPre = cdVer ppNoPreL
     in if isEmpty
         then CDoc_Emp
         else if optPlain opts
         then ppNoPre
         else (if optPreamble opts then cdVer (map (wrap False . vciCD) pre) else CDoc_Emp)
              .-. wrap True ppMod
              .-. ppNoPre
-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      synthesized attributes:
         bldAG                : [Build]
         bldHS                : [Build]
         bldLaTeX             : [Build]
         deps                 : [String]
         gathNmChMp           : NmChMp
   alternatives:
      alternative AGItf:
         child dumLines       : Lines 
         child chunks         : Chunks 
         visit 0:
            local allowedVariants : _
            local allowedLaTeXVariants : _
            local chFullNm    : _
            local selChunks   : _
            local selLaTeXChunks : _
            local wrapLhs2tex : _
            local build       : _
-}
data AGItf  = AGItf_AGItf (Lines ) (Chunks ) 
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _dumLines _chunks )  =
    (sem_AGItf_AGItf (sem_Lines _dumLines ) (sem_Chunks _chunks ) )
-- semantic domain
type T_AGItf  = NmChMp ->
                Opts ->
                XRefExcept ->
                ( ([Build]),([Build]),([Build]),([String]),NmChMp)
data Inh_AGItf  = Inh_AGItf {nmChMp_Inh_AGItf :: NmChMp,opts_Inh_AGItf :: Opts,xrefExcept_Inh_AGItf :: XRefExcept}
data Syn_AGItf  = Syn_AGItf {bldAG_Syn_AGItf :: ([Build]),bldHS_Syn_AGItf :: ([Build]),bldLaTeX_Syn_AGItf :: ([Build]),deps_Syn_AGItf :: ([String]),gathNmChMp_Syn_AGItf :: NmChMp}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsInmChMp _lhsIopts _lhsIxrefExcept )  =
    (let ( _lhsObldAG,_lhsObldHS,_lhsObldLaTeX,_lhsOdeps,_lhsOgathNmChMp) = sem _lhsInmChMp _lhsIopts _lhsIxrefExcept 
     in  (Syn_AGItf _lhsObldAG _lhsObldHS _lhsObldLaTeX _lhsOdeps _lhsOgathNmChMp ))
sem_AGItf_AGItf :: T_Lines  ->
                   T_Chunks  ->
                   T_AGItf 
sem_AGItf_AGItf dumLines_ chunks_  =
    (\ _lhsInmChMp
       _lhsIopts
       _lhsIxrefExcept ->
         (let _dumLinesOseqNr :: Int
              _dumLinesOlineNr :: Int
              _chunksOnmChMp :: NmChMp
              _dumLinesOnmChMp :: NmChMp
              _lhsObldAG :: ([Build])
              _lhsObldLaTeX :: ([Build])
              _lhsObldHS :: ([Build])
              _lhsOdeps :: ([String])
              _lhsOgathNmChMp :: NmChMp
              _dumLinesOallowedVariants :: VariantRefOrderMp
              _dumLinesOchFullNm :: Nm
              _dumLinesOopts :: Opts
              _dumLinesOxrefExcept :: XRefExcept
              _chunksOallowedVariants :: VariantRefOrderMp
              _chunksOchFullNm :: Nm
              _chunksOlineNr :: Int
              _chunksOopts :: Opts
              _chunksOseqNr :: Int
              _chunksOxrefExcept :: XRefExcept
              _dumLinesIgathHideMp :: HideMp
              _dumLinesIgathNmChMp :: NmChMp
              _dumLinesIlineNr :: Int
              _dumLinesImbCDoc :: (Maybe CDoc)
              _dumLinesImkCDoc :: MkCDoc
              _dumLinesIseqNr :: Int
              _dumLinesIxrefL :: XRefL
              _chunksIdeps :: ([String])
              _chunksIgathHideMp :: HideMp
              _chunksIgathNmChMp :: NmChMp
              _chunksIlineNr :: Int
              _chunksIseqNr :: Int
              _chunksIverChInfoL :: ([VariantChunkInfo])
              -- "src/shuffle/MainAG.ag"(line 143, column 17)
              _allowedVariants =
                  variantOfferAllLE_4 _lhsIopts
              -- "src/shuffle/MainAG.ag"(line 143, column 17)
              _allowedLaTeXVariants =
                  variantOfferAllLE_4 (_lhsIopts {optGenReqm=VReqmAll})
              -- "src/shuffle/MainAG.ag"(line 155, column 17)
              _dumLinesOseqNr =
                  1
              -- "src/shuffle/MainAG.ag"(line 171, column 17)
              _dumLinesOlineNr =
                  1
              -- "src/shuffle/MainAG.ag"(line 334, column 17)
              _chunksOnmChMp =
                  _chunksIgathNmChMp `Map.union` _lhsInmChMp
              -- "src/shuffle/MainAG.ag"(line 335, column 17)
              _dumLinesOnmChMp =
                  Map.empty
              -- "src/shuffle/MainAG.ag"(line 602, column 17)
              _chFullNm =
                  NmEmp
              -- "src/shuffle/MainAG.ag"(line 609, column 17)
              _selChunks =
                  selectChunks True (optGenReqm _lhsIopts) _allowedVariants _chunksIverChInfoL
              -- "src/shuffle/MainAG.ag"(line 609, column 17)
              _selLaTeXChunks =
                  selectChunks False VReqmAll _allowedLaTeXVariants _chunksIverChInfoL
              -- "src/shuffle/MainAG.ag"(line 609, column 17)
              _wrapLhs2tex =
                  \doWr -> if doWr then chWrap (optWrapLhs2tex _lhsIopts) else id
              -- "src/shuffle/MainAG.ag"(line 609, column 17)
              _build =
                  \bld chunks
                       -> let fileBase = fromJust (optBaseName _lhsIopts)
                              v = optGenReqm _lhsIopts
                              d = bld _lhsIopts _wrapLhs2tex fileBase chunks
                              m = Map.fromList [ (vciFullNm i,NmChInfo (vciFullNm i) (vciChDest i) (vciMbCD i) (vciMkCD i)) | (_,l) <- chunks, i <- l ]
                              h = [ (hiChFullNm h,mkHideNmDef (cd n) (hiDescr h) (cd (hiMbCD h)))
                                  | (n,h) <- sortBy (\(_,h1) (_,h2) -> hiSeqNr h1 `compare` hiSeqNr h2) . Map.toList $ _chunksIgathHideMp
                                  ]
                           in [Build fileBase v d h m]
              -- "src/shuffle/MainAG.ag"(line 704, column 17)
              _lhsObldAG =
                  _build buildAG _selChunks
              -- "src/shuffle/MainAG.ag"(line 780, column 17)
              _lhsObldLaTeX =
                  _build buildLaTeX _selLaTeXChunks
              -- "src/shuffle/MainAG.ag"(line 839, column 17)
              _lhsObldHS =
                  _build buildHS _selChunks
              -- use rule "src/shuffle/MainAG.ag"(line 584, column 32)
              _lhsOdeps =
                  _chunksIdeps
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  _dumLinesIgathNmChMp `Map.union` _chunksIgathNmChMp
              -- copy rule (from local)
              _dumLinesOallowedVariants =
                  _allowedVariants
              -- copy rule (from local)
              _dumLinesOchFullNm =
                  _chFullNm
              -- copy rule (down)
              _dumLinesOopts =
                  _lhsIopts
              -- copy rule (down)
              _dumLinesOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (from local)
              _chunksOallowedVariants =
                  _allowedVariants
              -- copy rule (from local)
              _chunksOchFullNm =
                  _chFullNm
              -- copy rule (chain)
              _chunksOlineNr =
                  _dumLinesIlineNr
              -- copy rule (down)
              _chunksOopts =
                  _lhsIopts
              -- copy rule (chain)
              _chunksOseqNr =
                  _dumLinesIseqNr
              -- copy rule (down)
              _chunksOxrefExcept =
                  _lhsIxrefExcept
              ( _dumLinesIgathHideMp,_dumLinesIgathNmChMp,_dumLinesIlineNr,_dumLinesImbCDoc,_dumLinesImkCDoc,_dumLinesIseqNr,_dumLinesIxrefL) =
                  dumLines_ _dumLinesOallowedVariants _dumLinesOchFullNm _dumLinesOlineNr _dumLinesOnmChMp _dumLinesOopts _dumLinesOseqNr _dumLinesOxrefExcept 
              ( _chunksIdeps,_chunksIgathHideMp,_chunksIgathNmChMp,_chunksIlineNr,_chunksIseqNr,_chunksIverChInfoL) =
                  chunks_ _chunksOallowedVariants _chunksOchFullNm _chunksOlineNr _chunksOnmChMp _chunksOopts _chunksOseqNr _chunksOxrefExcept 
          in  ( _lhsObldAG,_lhsObldHS,_lhsObldLaTeX,_lhsOdeps,_lhsOgathNmChMp)))
-- Chunk -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowedVariants      : VariantRefOrderMp
         chFullNm             : Nm
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         lineNr               : Int
         seqNr                : Int
      synthesized attributes:
         deps                 : [String]
         gathHideMp           : HideMp
         gathNmChMp           : NmChMp
         verChInfoL           : [VariantChunkInfo]
   alternatives:
      alternative Named:
         child cref           : {CRef}
         child chKind         : {ChKind}
         child lines          : Lines 
         child dumLines       : Lines 
         visit 0:
            local chunkLineNr : _
            local nmChInfo    : _
            local chFullNm    : _
      alternative Ver:
         child variantOffer   : {VariantOffer}
         child subNm          : {Nm}
         child minusL         : {[ChunkRef]}
         child chOptions      : ChunkOptions 
         child compRestrict   : {CompilerRestriction}
         child mbModNm        : MbStrExpr 
         child imports        : StrExprs 
         child exports        : StrExprs 
         child lines          : Lines 
         child dumLines       : Lines 
         visit 0:
            local chunkLineNr : _
            local dumIsOnlyNl : _
            local nmChInfo    : _
            local addBlankLine : _
            local chInfo      : _
            local isAllowed   : _
            local chFullNm    : _
-}
data Chunk  = Chunk_Named (CRef) (ChKind) (Lines ) (Lines ) 
            | Chunk_Ver (VariantOffer) (Nm) (([ChunkRef])) (ChunkOptions ) (CompilerRestriction) (MbStrExpr ) (StrExprs ) (StrExprs ) (Lines ) (Lines ) 
-- cata
sem_Chunk :: Chunk  ->
             T_Chunk 
sem_Chunk (Chunk_Named _cref _chKind _lines _dumLines )  =
    (sem_Chunk_Named _cref _chKind (sem_Lines _lines ) (sem_Lines _dumLines ) )
sem_Chunk (Chunk_Ver _variantOffer _subNm _minusL _chOptions _compRestrict _mbModNm _imports _exports _lines _dumLines )  =
    (sem_Chunk_Ver _variantOffer _subNm _minusL (sem_ChunkOptions _chOptions ) _compRestrict (sem_MbStrExpr _mbModNm ) (sem_StrExprs _imports ) (sem_StrExprs _exports ) (sem_Lines _lines ) (sem_Lines _dumLines ) )
-- semantic domain
type T_Chunk  = VariantRefOrderMp ->
                Nm ->
                Int ->
                NmChMp ->
                Opts ->
                Int ->
                XRefExcept ->
                ( ([String]),HideMp,NmChMp,Int,Int,([VariantChunkInfo]))
sem_Chunk_Named :: CRef ->
                   ChKind ->
                   T_Lines  ->
                   T_Lines  ->
                   T_Chunk 
sem_Chunk_Named cref_ chKind_ lines_ dumLines_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _linesOlineNr :: Int
              _dumLinesOlineNr :: Int
              _lhsOgathNmChMp :: NmChMp
              _lhsOdeps :: ([String])
              _lhsOgathHideMp :: HideMp
              _lhsOverChInfoL :: ([VariantChunkInfo])
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              _linesOallowedVariants :: VariantRefOrderMp
              _linesOchFullNm :: Nm
              _linesOnmChMp :: NmChMp
              _linesOopts :: Opts
              _linesOseqNr :: Int
              _linesOxrefExcept :: XRefExcept
              _dumLinesOallowedVariants :: VariantRefOrderMp
              _dumLinesOchFullNm :: Nm
              _dumLinesOnmChMp :: NmChMp
              _dumLinesOopts :: Opts
              _dumLinesOseqNr :: Int
              _dumLinesOxrefExcept :: XRefExcept
              _linesIgathHideMp :: HideMp
              _linesIgathNmChMp :: NmChMp
              _linesIlineNr :: Int
              _linesImbCDoc :: (Maybe CDoc)
              _linesImkCDoc :: MkCDoc
              _linesIseqNr :: Int
              _linesIxrefL :: XRefL
              _dumLinesIgathHideMp :: HideMp
              _dumLinesIgathNmChMp :: NmChMp
              _dumLinesIlineNr :: Int
              _dumLinesImbCDoc :: (Maybe CDoc)
              _dumLinesImkCDoc :: MkCDoc
              _dumLinesIseqNr :: Int
              _dumLinesIxrefL :: XRefL
              -- "src/shuffle/MainAG.ag"(line 174, column 17)
              _chunkLineNr =
                  _lhsIlineNr
              -- "src/shuffle/MainAG.ag"(line 176, column 17)
              _linesOlineNr =
                  _chunkLineNr + 1
              -- "src/shuffle/MainAG.ag"(line 177, column 17)
              _dumLinesOlineNr =
                  _linesIlineNr
              -- "src/shuffle/MainAG.ag"(line 324, column 17)
              _nmChInfo =
                  NmChInfo cref_ ChHere _linesImbCDoc _linesImkCDoc
              -- "src/shuffle/MainAG.ag"(line 325, column 17)
              _lhsOgathNmChMp =
                  Map.insert cref_ _nmChInfo _linesIgathNmChMp
              -- "src/shuffle/MainAG.ag"(line 599, column 17)
              _chFullNm =
                  nciNm _nmChInfo
              -- use rule "src/shuffle/MainAG.ag"(line 584, column 32)
              _lhsOdeps =
                  []
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  _linesIgathHideMp `Map.union` _dumLinesIgathHideMp
              -- use rule "src/shuffle/MainAG.ag"(line 567, column 32)
              _lhsOverChInfoL =
                  []
              -- copy rule (up)
              _lhsOlineNr =
                  _dumLinesIlineNr
              -- copy rule (up)
              _lhsOseqNr =
                  _dumLinesIseqNr
              -- copy rule (down)
              _linesOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (from local)
              _linesOchFullNm =
                  _chFullNm
              -- copy rule (down)
              _linesOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _linesOopts =
                  _lhsIopts
              -- copy rule (down)
              _linesOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _linesOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _dumLinesOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (from local)
              _dumLinesOchFullNm =
                  _chFullNm
              -- copy rule (down)
              _dumLinesOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _dumLinesOopts =
                  _lhsIopts
              -- copy rule (chain)
              _dumLinesOseqNr =
                  _linesIseqNr
              -- copy rule (down)
              _dumLinesOxrefExcept =
                  _lhsIxrefExcept
              ( _linesIgathHideMp,_linesIgathNmChMp,_linesIlineNr,_linesImbCDoc,_linesImkCDoc,_linesIseqNr,_linesIxrefL) =
                  lines_ _linesOallowedVariants _linesOchFullNm _linesOlineNr _linesOnmChMp _linesOopts _linesOseqNr _linesOxrefExcept 
              ( _dumLinesIgathHideMp,_dumLinesIgathNmChMp,_dumLinesIlineNr,_dumLinesImbCDoc,_dumLinesImkCDoc,_dumLinesIseqNr,_dumLinesIxrefL) =
                  dumLines_ _dumLinesOallowedVariants _dumLinesOchFullNm _dumLinesOlineNr _dumLinesOnmChMp _dumLinesOopts _dumLinesOseqNr _dumLinesOxrefExcept 
          in  ( _lhsOdeps,_lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOseqNr,_lhsOverChInfoL)))
sem_Chunk_Ver :: VariantOffer ->
                 Nm ->
                 ([ChunkRef]) ->
                 T_ChunkOptions  ->
                 CompilerRestriction ->
                 T_MbStrExpr  ->
                 T_StrExprs  ->
                 T_StrExprs  ->
                 T_Lines  ->
                 T_Lines  ->
                 T_Chunk 
sem_Chunk_Ver variantOffer_ subNm_ minusL_ chOptions_ compRestrict_ mbModNm_ imports_ exports_ lines_ dumLines_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _linesOlineNr :: Int
              _dumLinesOlineNr :: Int
              _lhsOgathNmChMp :: NmChMp
              _lhsOverChInfoL :: ([VariantChunkInfo])
              _lhsOdeps :: ([String])
              _lhsOgathHideMp :: HideMp
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              _mbModNmOopts :: Opts
              _mbModNmOxrefExcept :: XRefExcept
              _importsOopts :: Opts
              _importsOxrefExcept :: XRefExcept
              _exportsOopts :: Opts
              _exportsOxrefExcept :: XRefExcept
              _linesOallowedVariants :: VariantRefOrderMp
              _linesOchFullNm :: Nm
              _linesOnmChMp :: NmChMp
              _linesOopts :: Opts
              _linesOseqNr :: Int
              _linesOxrefExcept :: XRefExcept
              _dumLinesOallowedVariants :: VariantRefOrderMp
              _dumLinesOchFullNm :: Nm
              _dumLinesOnmChMp :: NmChMp
              _dumLinesOopts :: Opts
              _dumLinesOseqNr :: Int
              _dumLinesOxrefExcept :: XRefExcept
              _chOptionsIchDest :: ChDest
              _chOptionsIchKind :: ChKind
              _chOptionsIchWrap :: ChWrap
              _mbModNmImbStr :: (Maybe String)
              _importsIstrL :: ([String])
              _exportsIstrL :: ([String])
              _linesIgathHideMp :: HideMp
              _linesIgathNmChMp :: NmChMp
              _linesIlineNr :: Int
              _linesImbCDoc :: (Maybe CDoc)
              _linesImkCDoc :: MkCDoc
              _linesIseqNr :: Int
              _linesIxrefL :: XRefL
              _dumLinesIgathHideMp :: HideMp
              _dumLinesIgathNmChMp :: NmChMp
              _dumLinesIlineNr :: Int
              _dumLinesImbCDoc :: (Maybe CDoc)
              _dumLinesImkCDoc :: MkCDoc
              _dumLinesIseqNr :: Int
              _dumLinesIxrefL :: XRefL
              -- "src/shuffle/MainAG.ag"(line 174, column 17)
              _chunkLineNr =
                  _lhsIlineNr
              -- "src/shuffle/MainAG.ag"(line 176, column 17)
              _linesOlineNr =
                  _chunkLineNr + 1
              -- "src/shuffle/MainAG.ag"(line 177, column 17)
              _dumLinesOlineNr =
                  _linesIlineNr
              -- "src/shuffle/MainAG.ag"(line 180, column 17)
              _dumIsOnlyNl =
                  _linesIlineNr + 1 == _dumLinesIlineNr
              -- "src/shuffle/MainAG.ag"(line 322, column 17)
              _nmChInfo =
                  NmChInfo _chFullNm ChHere _linesImbCDoc _linesImkCDoc
              -- "src/shuffle/MainAG.ag"(line 323, column 17)
              _lhsOgathNmChMp =
                  Map.insert _chFullNm _nmChInfo _linesIgathNmChMp
              -- "src/shuffle/MainAG.ag"(line 561, column 17)
              _addBlankLine =
                  if _dumIsOnlyNl then id else (.-. CDoc_Str "")
              -- "src/shuffle/MainAG.ag"(line 570, column 17)
              _chInfo =
                  VariantChunkInfo
                      _chunkLineNr _lhsIseqNr
                      variantOffer_ (chunkRefFromOfferNm variantOffer_ subNm_) minusL_
                      _chOptionsIchKind _chOptionsIchDest _mbModNmImbStr _importsIstrL _exportsIstrL
                      (fmap (_addBlankLine . chWrap (chWrapT2T _lhsIopts _chOptionsIchKind)) _linesImbCDoc)
                      _linesImkCDoc
                      (Seq.toList _linesIxrefL)
              -- "src/shuffle/MainAG.ag"(line 577, column 17)
              _isAllowed =
                  isAllowedCompilerVariant compRestrict_ (optCompiler _lhsIopts)
              -- "src/shuffle/MainAG.ag"(line 578, column 17)
              _lhsOverChInfoL =
                  if _isAllowed then [_chInfo] else []
              -- "src/shuffle/MainAG.ag"(line 587, column 17)
              _lhsOdeps =
                  if _chOptionsIchKind == ChAG
                  then _importsIstrL
                  else []
              -- "src/shuffle/MainAG.ag"(line 598, column 17)
              _chFullNm =
                  vciFullNm _chInfo
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  _linesIgathHideMp `Map.union` _dumLinesIgathHideMp
              -- copy rule (up)
              _lhsOlineNr =
                  _dumLinesIlineNr
              -- copy rule (up)
              _lhsOseqNr =
                  _dumLinesIseqNr
              -- copy rule (down)
              _mbModNmOopts =
                  _lhsIopts
              -- copy rule (down)
              _mbModNmOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _importsOopts =
                  _lhsIopts
              -- copy rule (down)
              _importsOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _exportsOopts =
                  _lhsIopts
              -- copy rule (down)
              _exportsOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _linesOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (from local)
              _linesOchFullNm =
                  _chFullNm
              -- copy rule (down)
              _linesOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _linesOopts =
                  _lhsIopts
              -- copy rule (down)
              _linesOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _linesOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _dumLinesOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (from local)
              _dumLinesOchFullNm =
                  _chFullNm
              -- copy rule (down)
              _dumLinesOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _dumLinesOopts =
                  _lhsIopts
              -- copy rule (chain)
              _dumLinesOseqNr =
                  _linesIseqNr
              -- copy rule (down)
              _dumLinesOxrefExcept =
                  _lhsIxrefExcept
              ( _chOptionsIchDest,_chOptionsIchKind,_chOptionsIchWrap) =
                  chOptions_ 
              ( _mbModNmImbStr) =
                  mbModNm_ _mbModNmOopts _mbModNmOxrefExcept 
              ( _importsIstrL) =
                  imports_ _importsOopts _importsOxrefExcept 
              ( _exportsIstrL) =
                  exports_ _exportsOopts _exportsOxrefExcept 
              ( _linesIgathHideMp,_linesIgathNmChMp,_linesIlineNr,_linesImbCDoc,_linesImkCDoc,_linesIseqNr,_linesIxrefL) =
                  lines_ _linesOallowedVariants _linesOchFullNm _linesOlineNr _linesOnmChMp _linesOopts _linesOseqNr _linesOxrefExcept 
              ( _dumLinesIgathHideMp,_dumLinesIgathNmChMp,_dumLinesIlineNr,_dumLinesImbCDoc,_dumLinesImkCDoc,_dumLinesIseqNr,_dumLinesIxrefL) =
                  dumLines_ _dumLinesOallowedVariants _dumLinesOchFullNm _dumLinesOlineNr _dumLinesOnmChMp _dumLinesOopts _dumLinesOseqNr _dumLinesOxrefExcept 
          in  ( _lhsOdeps,_lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOseqNr,_lhsOverChInfoL)))
-- ChunkOption -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         chDest               : ChDest
         chKind               : ChKind
         chWrap               : ChWrap
   alternatives:
      alternative Dest:
         child chDest         : {ChDest}
      alternative Kind:
         child chKind         : {ChKind}
      alternative Wrap:
         child chWrap         : {ChWrap}
-}
data ChunkOption  = ChunkOption_Dest (ChDest) 
                  | ChunkOption_Kind (ChKind) 
                  | ChunkOption_Wrap (ChWrap) 
-- cata
sem_ChunkOption :: ChunkOption  ->
                   T_ChunkOption 
sem_ChunkOption (ChunkOption_Dest _chDest )  =
    (sem_ChunkOption_Dest _chDest )
sem_ChunkOption (ChunkOption_Kind _chKind )  =
    (sem_ChunkOption_Kind _chKind )
sem_ChunkOption (ChunkOption_Wrap _chWrap )  =
    (sem_ChunkOption_Wrap _chWrap )
-- semantic domain
type T_ChunkOption  = ( ChDest,ChKind,ChWrap)
sem_ChunkOption_Dest :: ChDest ->
                        T_ChunkOption 
sem_ChunkOption_Dest chDest_  =
    (let _lhsOchDest :: ChDest
         _lhsOchKind :: ChKind
         _lhsOchWrap :: ChWrap
         -- "src/shuffle/MainAG.ag"(line 553, column 17)
         _lhsOchDest =
             chDest_
         -- use rule "src/shuffle/MainAG.ag"(line 547, column 34)
         _lhsOchKind =
             ChPlain
         -- use rule "src/shuffle/MainAG.ag"(line 549, column 34)
         _lhsOchWrap =
             ChWrapPlain
     in  ( _lhsOchDest,_lhsOchKind,_lhsOchWrap))
sem_ChunkOption_Kind :: ChKind ->
                        T_ChunkOption 
sem_ChunkOption_Kind chKind_  =
    (let _lhsOchKind :: ChKind
         _lhsOchDest :: ChDest
         _lhsOchWrap :: ChWrap
         -- "src/shuffle/MainAG.ag"(line 552, column 17)
         _lhsOchKind =
             chKind_
         -- use rule "src/shuffle/MainAG.ag"(line 548, column 34)
         _lhsOchDest =
             ChHere
         -- use rule "src/shuffle/MainAG.ag"(line 549, column 34)
         _lhsOchWrap =
             ChWrapPlain
     in  ( _lhsOchDest,_lhsOchKind,_lhsOchWrap))
sem_ChunkOption_Wrap :: ChWrap ->
                        T_ChunkOption 
sem_ChunkOption_Wrap chWrap_  =
    (let _lhsOchWrap :: ChWrap
         _lhsOchDest :: ChDest
         _lhsOchKind :: ChKind
         -- "src/shuffle/MainAG.ag"(line 554, column 17)
         _lhsOchWrap =
             chWrap_
         -- use rule "src/shuffle/MainAG.ag"(line 548, column 34)
         _lhsOchDest =
             ChHere
         -- use rule "src/shuffle/MainAG.ag"(line 547, column 34)
         _lhsOchKind =
             ChPlain
     in  ( _lhsOchDest,_lhsOchKind,_lhsOchWrap))
-- ChunkOptions ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         chDest               : ChDest
         chKind               : ChKind
         chWrap               : ChWrap
   alternatives:
      alternative Cons:
         child hd             : ChunkOption 
         child tl             : ChunkOptions 
      alternative Nil:
-}
type ChunkOptions  = [ChunkOption ]
-- cata
sem_ChunkOptions :: ChunkOptions  ->
                    T_ChunkOptions 
sem_ChunkOptions list  =
    (Prelude.foldr sem_ChunkOptions_Cons sem_ChunkOptions_Nil (Prelude.map sem_ChunkOption list) )
-- semantic domain
type T_ChunkOptions  = ( ChDest,ChKind,ChWrap)
sem_ChunkOptions_Cons :: T_ChunkOption  ->
                         T_ChunkOptions  ->
                         T_ChunkOptions 
sem_ChunkOptions_Cons hd_ tl_  =
    (let _lhsOchDest :: ChDest
         _lhsOchKind :: ChKind
         _lhsOchWrap :: ChWrap
         _hdIchDest :: ChDest
         _hdIchKind :: ChKind
         _hdIchWrap :: ChWrap
         _tlIchDest :: ChDest
         _tlIchKind :: ChKind
         _tlIchWrap :: ChWrap
         -- use rule "src/shuffle/MainAG.ag"(line 548, column 34)
         _lhsOchDest =
             _hdIchDest `chDestCmb` _tlIchDest
         -- use rule "src/shuffle/MainAG.ag"(line 547, column 34)
         _lhsOchKind =
             _hdIchKind `chKindCmb` _tlIchKind
         -- use rule "src/shuffle/MainAG.ag"(line 549, column 34)
         _lhsOchWrap =
             _hdIchWrap `chWrapCmb` _tlIchWrap
         ( _hdIchDest,_hdIchKind,_hdIchWrap) =
             hd_ 
         ( _tlIchDest,_tlIchKind,_tlIchWrap) =
             tl_ 
     in  ( _lhsOchDest,_lhsOchKind,_lhsOchWrap))
sem_ChunkOptions_Nil :: T_ChunkOptions 
sem_ChunkOptions_Nil  =
    (let _lhsOchDest :: ChDest
         _lhsOchKind :: ChKind
         _lhsOchWrap :: ChWrap
         -- use rule "src/shuffle/MainAG.ag"(line 548, column 34)
         _lhsOchDest =
             ChHere
         -- use rule "src/shuffle/MainAG.ag"(line 547, column 34)
         _lhsOchKind =
             ChPlain
         -- use rule "src/shuffle/MainAG.ag"(line 549, column 34)
         _lhsOchWrap =
             ChWrapPlain
     in  ( _lhsOchDest,_lhsOchKind,_lhsOchWrap))
-- Chunks ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowedVariants      : VariantRefOrderMp
         chFullNm             : Nm
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         lineNr               : Int
         seqNr                : Int
      synthesized attributes:
         deps                 : [String]
         gathHideMp           : HideMp
         gathNmChMp           : NmChMp
         verChInfoL           : [VariantChunkInfo]
   alternatives:
      alternative Cons:
         child hd             : Chunk 
         child tl             : Chunks 
      alternative Nil:
-}
type Chunks  = [Chunk ]
-- cata
sem_Chunks :: Chunks  ->
              T_Chunks 
sem_Chunks list  =
    (Prelude.foldr sem_Chunks_Cons sem_Chunks_Nil (Prelude.map sem_Chunk list) )
-- semantic domain
type T_Chunks  = VariantRefOrderMp ->
                 Nm ->
                 Int ->
                 NmChMp ->
                 Opts ->
                 Int ->
                 XRefExcept ->
                 ( ([String]),HideMp,NmChMp,Int,Int,([VariantChunkInfo]))
sem_Chunks_Cons :: T_Chunk  ->
                   T_Chunks  ->
                   T_Chunks 
sem_Chunks_Cons hd_ tl_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _hdOseqNr :: Int
              _lhsOdeps :: ([String])
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOverChInfoL :: ([VariantChunkInfo])
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              _hdOallowedVariants :: VariantRefOrderMp
              _hdOchFullNm :: Nm
              _hdOlineNr :: Int
              _hdOnmChMp :: NmChMp
              _hdOopts :: Opts
              _hdOxrefExcept :: XRefExcept
              _tlOallowedVariants :: VariantRefOrderMp
              _tlOchFullNm :: Nm
              _tlOlineNr :: Int
              _tlOnmChMp :: NmChMp
              _tlOopts :: Opts
              _tlOseqNr :: Int
              _tlOxrefExcept :: XRefExcept
              _hdIdeps :: ([String])
              _hdIgathHideMp :: HideMp
              _hdIgathNmChMp :: NmChMp
              _hdIlineNr :: Int
              _hdIseqNr :: Int
              _hdIverChInfoL :: ([VariantChunkInfo])
              _tlIdeps :: ([String])
              _tlIgathHideMp :: HideMp
              _tlIgathNmChMp :: NmChMp
              _tlIlineNr :: Int
              _tlIseqNr :: Int
              _tlIverChInfoL :: ([VariantChunkInfo])
              -- "src/shuffle/MainAG.ag"(line 158, column 17)
              _hdOseqNr =
                  _lhsIseqNr + 1
              -- use rule "src/shuffle/MainAG.ag"(line 584, column 32)
              _lhsOdeps =
                  _hdIdeps ++ _tlIdeps
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  _hdIgathHideMp `Map.union` _tlIgathHideMp
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  _hdIgathNmChMp `Map.union` _tlIgathNmChMp
              -- use rule "src/shuffle/MainAG.ag"(line 567, column 32)
              _lhsOverChInfoL =
                  _hdIverChInfoL ++ _tlIverChInfoL
              -- copy rule (up)
              _lhsOlineNr =
                  _tlIlineNr
              -- copy rule (up)
              _lhsOseqNr =
                  _tlIseqNr
              -- copy rule (down)
              _hdOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _hdOchFullNm =
                  _lhsIchFullNm
              -- copy rule (down)
              _hdOlineNr =
                  _lhsIlineNr
              -- copy rule (down)
              _hdOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _tlOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _tlOchFullNm =
                  _lhsIchFullNm
              -- copy rule (chain)
              _tlOlineNr =
                  _hdIlineNr
              -- copy rule (down)
              _tlOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOseqNr =
                  _hdIseqNr
              -- copy rule (down)
              _tlOxrefExcept =
                  _lhsIxrefExcept
              ( _hdIdeps,_hdIgathHideMp,_hdIgathNmChMp,_hdIlineNr,_hdIseqNr,_hdIverChInfoL) =
                  hd_ _hdOallowedVariants _hdOchFullNm _hdOlineNr _hdOnmChMp _hdOopts _hdOseqNr _hdOxrefExcept 
              ( _tlIdeps,_tlIgathHideMp,_tlIgathNmChMp,_tlIlineNr,_tlIseqNr,_tlIverChInfoL) =
                  tl_ _tlOallowedVariants _tlOchFullNm _tlOlineNr _tlOnmChMp _tlOopts _tlOseqNr _tlOxrefExcept 
          in  ( _lhsOdeps,_lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOseqNr,_lhsOverChInfoL)))
sem_Chunks_Nil :: T_Chunks 
sem_Chunks_Nil  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _lhsOdeps :: ([String])
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOverChInfoL :: ([VariantChunkInfo])
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              -- use rule "src/shuffle/MainAG.ag"(line 584, column 32)
              _lhsOdeps =
                  []
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 567, column 32)
              _lhsOverChInfoL =
                  []
              -- copy rule (chain)
              _lhsOlineNr =
                  _lhsIlineNr
              -- copy rule (chain)
              _lhsOseqNr =
                  _lhsIseqNr
          in  ( _lhsOdeps,_lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOseqNr,_lhsOverChInfoL)))
-- Group -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowedVariants      : VariantRefOrderMp
         chFullNm             : Nm
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         lineNr               : Int
         seqNr                : Int
      synthesized attributes:
         gathHideMp           : HideMp
         gathNmChMp           : NmChMp
         mbCDoc               : Maybe CDoc
         mbCDocL              : [(VariantOffer,Maybe CDoc)]
         mkCDoc               : MkCDoc
         mkCDocL              : [(VariantOffer,MkCDoc)]
   alternatives:
      alternative Group:
         child variantOffer   : {VariantOffer}
         child chOptions      : ChunkOptions 
         child userRef        : {Maybe (Nm,Maybe String)}
         child lines          : Lines 
         visit 0:
            local gathNmChMp  : _
            local mbCDocbase  : _
            local isAllowed   : _
            local _tup1       : _
            local mbCDoc      : _
            local gathHideMp  : _
            local mkCDoc      : _
-}
data Group  = Group_Group (VariantOffer) (ChunkOptions ) ((Maybe (Nm,Maybe String))) (Lines ) 
-- cata
sem_Group :: Group  ->
             T_Group 
sem_Group (Group_Group _variantOffer _chOptions _userRef _lines )  =
    (sem_Group_Group _variantOffer (sem_ChunkOptions _chOptions ) _userRef (sem_Lines _lines ) )
-- semantic domain
type T_Group  = VariantRefOrderMp ->
                Nm ->
                Int ->
                NmChMp ->
                Opts ->
                Int ->
                XRefExcept ->
                ( HideMp,NmChMp,Int,(Maybe CDoc),([(VariantOffer,Maybe CDoc)]),MkCDoc,([(VariantOffer,MkCDoc)]),Int)
sem_Group_Group :: VariantOffer ->
                   T_ChunkOptions  ->
                   (Maybe (Nm,Maybe String)) ->
                   T_Lines  ->
                   T_Group 
sem_Group_Group variantOffer_ chOptions_ userRef_ lines_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _linesOseqNr :: Int
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDocL :: ([(VariantOffer,Maybe CDoc)])
              _lhsOmkCDocL :: ([(VariantOffer,MkCDoc)])
              _lhsOgathHideMp :: HideMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOmkCDoc :: MkCDoc
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              _linesOallowedVariants :: VariantRefOrderMp
              _linesOchFullNm :: Nm
              _linesOlineNr :: Int
              _linesOnmChMp :: NmChMp
              _linesOopts :: Opts
              _linesOxrefExcept :: XRefExcept
              _chOptionsIchDest :: ChDest
              _chOptionsIchKind :: ChKind
              _chOptionsIchWrap :: ChWrap
              _linesIgathHideMp :: HideMp
              _linesIgathNmChMp :: NmChMp
              _linesIlineNr :: Int
              _linesImbCDoc :: (Maybe CDoc)
              _linesImkCDoc :: MkCDoc
              _linesIseqNr :: Int
              _linesIxrefL :: XRefL
              -- "src/shuffle/MainAG.ag"(line 161, column 17)
              _linesOseqNr =
                  _lhsIseqNr + 1
              -- "src/shuffle/MainAG.ag"(line 328, column 17)
              _gathNmChMp =
                  case userRef_ of
                    Just (r,_) -> Map.singleton r (NmChInfo r ChHere _mbCDoc _mkCDoc)
                    _          -> Map.empty
              -- "src/shuffle/MainAG.ag"(line 331, column 17)
              _lhsOgathNmChMp =
                  Map.union _linesIgathNmChMp _gathNmChMp
              -- "src/shuffle/MainAG.ag"(line 374, column 17)
              _mbCDocbase =
                  fmap (chWrap _chOptionsIchWrap) _linesImbCDoc
              -- "src/shuffle/MainAG.ag"(line 374, column 17)
              _isAllowed =
                  variantReqmMatchOffer (Just _lhsIallowedVariants) (optGenReqm _lhsIopts) variantOffer_
              -- "src/shuffle/MainAG.ag"(line 376, column 17)
              __tup1 =
                  case _chOptionsIchDest of
                    _ | not _isAllowed
                           -> (Nothing,Map.empty)
                    ChHere -> (_mbCDocbase,Map.empty)
                    h      -> (Just (mkHideNmRef (cd n)),Map.singleton n (HideInfo n i _lhsIseqNr _chOptionsIchDest _mbCDocbase _lhsIchFullNm))
                           where (n,i) = case userRef_ of
                                           Just (r,Just i ) -> (r,cd i)
                                           Just (r,Nothing) -> (r,cd r)
                                           _                -> (mkNm (show h) `nmApd` mkNm _lhsIseqNr,CDoc_Emp)
              -- "src/shuffle/MainAG.ag"(line 376, column 17)
              (_mbCDoc,_) =
                  __tup1
              -- "src/shuffle/MainAG.ag"(line 376, column 17)
              (_,_gathHideMp) =
                  __tup1
              -- "src/shuffle/MainAG.ag"(line 385, column 17)
              _lhsOmbCDocL =
                  if _isAllowed then [(variantOffer_,_mbCDoc)] else []
              -- "src/shuffle/MainAG.ag"(line 418, column 17)
              _mkCDoc =
                  \sel -> case _chOptionsIchDest of
                            ChHere -> fmap (chWrap _chOptionsIchWrap) (_linesImkCDoc sel)
                            _      -> Nothing
              -- "src/shuffle/MainAG.ag"(line 421, column 17)
              _lhsOmkCDocL =
                  [(variantOffer_,_mkCDoc)]
              -- "src/shuffle/MainAG.ag"(line 430, column 17)
              _lhsOgathHideMp =
                  _gathHideMp `Map.union` _linesIgathHideMp
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  _mbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 408, column 36)
              _lhsOmkCDoc =
                  _mkCDoc
              -- copy rule (up)
              _lhsOlineNr =
                  _linesIlineNr
              -- copy rule (up)
              _lhsOseqNr =
                  _linesIseqNr
              -- copy rule (down)
              _linesOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _linesOchFullNm =
                  _lhsIchFullNm
              -- copy rule (down)
              _linesOlineNr =
                  _lhsIlineNr
              -- copy rule (down)
              _linesOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _linesOopts =
                  _lhsIopts
              -- copy rule (down)
              _linesOxrefExcept =
                  _lhsIxrefExcept
              ( _chOptionsIchDest,_chOptionsIchKind,_chOptionsIchWrap) =
                  chOptions_ 
              ( _linesIgathHideMp,_linesIgathNmChMp,_linesIlineNr,_linesImbCDoc,_linesImkCDoc,_linesIseqNr,_linesIxrefL) =
                  lines_ _linesOallowedVariants _linesOchFullNm _linesOlineNr _linesOnmChMp _linesOopts _linesOseqNr _linesOxrefExcept 
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmbCDocL,_lhsOmkCDoc,_lhsOmkCDocL,_lhsOseqNr)))
-- Groups ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowedVariants      : VariantRefOrderMp
         chFullNm             : Nm
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         lineNr               : Int
         seqNr                : Int
      synthesized attributes:
         gathHideMp           : HideMp
         gathNmChMp           : NmChMp
         mbCDoc               : Maybe CDoc
         mbCDocL              : [(VariantOffer,Maybe CDoc)]
         mkCDoc               : MkCDoc
         mkCDocL              : [(VariantOffer,MkCDoc)]
   alternatives:
      alternative Cons:
         child hd             : Group 
         child tl             : Groups 
      alternative Nil:
-}
type Groups  = [Group ]
-- cata
sem_Groups :: Groups  ->
              T_Groups 
sem_Groups list  =
    (Prelude.foldr sem_Groups_Cons sem_Groups_Nil (Prelude.map sem_Group list) )
-- semantic domain
type T_Groups  = VariantRefOrderMp ->
                 Nm ->
                 Int ->
                 NmChMp ->
                 Opts ->
                 Int ->
                 XRefExcept ->
                 ( HideMp,NmChMp,Int,(Maybe CDoc),([(VariantOffer,Maybe CDoc)]),MkCDoc,([(VariantOffer,MkCDoc)]),Int)
sem_Groups_Cons :: T_Group  ->
                   T_Groups  ->
                   T_Groups 
sem_Groups_Cons hd_ tl_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _tlOlineNr :: Int
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOmbCDocL :: ([(VariantOffer,Maybe CDoc)])
              _lhsOmkCDoc :: MkCDoc
              _lhsOmkCDocL :: ([(VariantOffer,MkCDoc)])
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              _hdOallowedVariants :: VariantRefOrderMp
              _hdOchFullNm :: Nm
              _hdOlineNr :: Int
              _hdOnmChMp :: NmChMp
              _hdOopts :: Opts
              _hdOseqNr :: Int
              _hdOxrefExcept :: XRefExcept
              _tlOallowedVariants :: VariantRefOrderMp
              _tlOchFullNm :: Nm
              _tlOnmChMp :: NmChMp
              _tlOopts :: Opts
              _tlOseqNr :: Int
              _tlOxrefExcept :: XRefExcept
              _hdIgathHideMp :: HideMp
              _hdIgathNmChMp :: NmChMp
              _hdIlineNr :: Int
              _hdImbCDoc :: (Maybe CDoc)
              _hdImbCDocL :: ([(VariantOffer,Maybe CDoc)])
              _hdImkCDoc :: MkCDoc
              _hdImkCDocL :: ([(VariantOffer,MkCDoc)])
              _hdIseqNr :: Int
              _tlIgathHideMp :: HideMp
              _tlIgathNmChMp :: NmChMp
              _tlIlineNr :: Int
              _tlImbCDoc :: (Maybe CDoc)
              _tlImbCDocL :: ([(VariantOffer,Maybe CDoc)])
              _tlImkCDoc :: MkCDoc
              _tlImkCDocL :: ([(VariantOffer,MkCDoc)])
              _tlIseqNr :: Int
              -- "src/shuffle/MainAG.ag"(line 193, column 17)
              _tlOlineNr =
                  _hdIlineNr + 1
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  _hdIgathHideMp `Map.union` _tlIgathHideMp
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  _hdIgathNmChMp `Map.union` _tlIgathNmChMp
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  _hdImbCDoc `mbCDocCmb` _tlImbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 365, column 29)
              _lhsOmbCDocL =
                  _hdImbCDocL ++ _tlImbCDocL
              -- use rule "src/shuffle/MainAG.ag"(line 408, column 36)
              _lhsOmkCDoc =
                  _hdImkCDoc `mkCDocCmb` _tlImkCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 409, column 29)
              _lhsOmkCDocL =
                  _hdImkCDocL ++ _tlImkCDocL
              -- copy rule (up)
              _lhsOlineNr =
                  _tlIlineNr
              -- copy rule (up)
              _lhsOseqNr =
                  _tlIseqNr
              -- copy rule (down)
              _hdOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _hdOchFullNm =
                  _lhsIchFullNm
              -- copy rule (down)
              _hdOlineNr =
                  _lhsIlineNr
              -- copy rule (down)
              _hdOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _hdOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _tlOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _tlOchFullNm =
                  _lhsIchFullNm
              -- copy rule (down)
              _tlOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOseqNr =
                  _hdIseqNr
              -- copy rule (down)
              _tlOxrefExcept =
                  _lhsIxrefExcept
              ( _hdIgathHideMp,_hdIgathNmChMp,_hdIlineNr,_hdImbCDoc,_hdImbCDocL,_hdImkCDoc,_hdImkCDocL,_hdIseqNr) =
                  hd_ _hdOallowedVariants _hdOchFullNm _hdOlineNr _hdOnmChMp _hdOopts _hdOseqNr _hdOxrefExcept 
              ( _tlIgathHideMp,_tlIgathNmChMp,_tlIlineNr,_tlImbCDoc,_tlImbCDocL,_tlImkCDoc,_tlImkCDocL,_tlIseqNr) =
                  tl_ _tlOallowedVariants _tlOchFullNm _tlOlineNr _tlOnmChMp _tlOopts _tlOseqNr _tlOxrefExcept 
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmbCDocL,_lhsOmkCDoc,_lhsOmkCDocL,_lhsOseqNr)))
sem_Groups_Nil :: T_Groups 
sem_Groups_Nil  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _lhsOlineNr :: Int
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOmbCDocL :: ([(VariantOffer,Maybe CDoc)])
              _lhsOmkCDoc :: MkCDoc
              _lhsOmkCDocL :: ([(VariantOffer,MkCDoc)])
              _lhsOseqNr :: Int
              -- "src/shuffle/MainAG.ag"(line 194, column 17)
              _lhsOlineNr =
                  _lhsIlineNr - 1
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  Nothing
              -- use rule "src/shuffle/MainAG.ag"(line 365, column 29)
              _lhsOmbCDocL =
                  []
              -- use rule "src/shuffle/MainAG.ag"(line 408, column 36)
              _lhsOmkCDoc =
                  mkCDocEmpty
              -- use rule "src/shuffle/MainAG.ag"(line 409, column 29)
              _lhsOmkCDocL =
                  []
              -- copy rule (chain)
              _lhsOseqNr =
                  _lhsIseqNr
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmbCDocL,_lhsOmkCDoc,_lhsOmkCDocL,_lhsOseqNr)))
-- Inline ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attribute:
         colNr                : Int
      synthesized attributes:
         cdoc                 : CDoc
         xrefL                : XRefL
   alternatives:
      alternative URI:
         child str            : {String}
-}
data Inline  = Inline_URI (String) 
-- cata
sem_Inline :: Inline  ->
              T_Inline 
sem_Inline (Inline_URI _str )  =
    (sem_Inline_URI _str )
-- semantic domain
type T_Inline  = Int ->
                 Opts ->
                 XRefExcept ->
                 ( CDoc,Int,XRefL)
sem_Inline_URI :: String ->
                  T_Inline 
sem_Inline_URI str_  =
    (\ _lhsIcolNr
       _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOcdoc :: CDoc
              _lhsOxrefL :: XRefL
              _lhsOcolNr :: Int
              -- "src/shuffle/MainAG.ag"(line 394, column 17)
              _lhsOcdoc =
                  CDoc_Inl str_
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (chain)
              _lhsOcolNr =
                  _lhsIcolNr
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOxrefL)))
-- Line --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowedVariants      : VariantRefOrderMp
         chFullNm             : Nm
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         lineNr               : Int
         seqNr                : Int
      synthesized attributes:
         gathHideMp           : HideMp
         gathNmChMp           : NmChMp
         mbCDoc               : Maybe CDoc
         mkCDoc               : MkCDoc
         xrefL                : XRefL
   alternatives:
      alternative AsIs:
         child words          : Words 
         visit 0:
            local cdoc        : _
            local mbCDoc      : _
      alternative Groups:
         child extraLine      : {Int}
         child groups         : Groups 
         visit 0:
            local groupsLineNr : _
      alternative Named:
         child cref           : {CRef}
         child mbVariantReqm  : {Maybe VariantReqm}
         visit 0:
            local cdoc        : _
            local mbCDoc      : _
-}
data Line  = Line_AsIs (Words ) 
           | Line_Groups (Int) (Groups ) 
           | Line_Named (CRef) ((Maybe VariantReqm)) 
-- cata
sem_Line :: Line  ->
            T_Line 
sem_Line (Line_AsIs _words )  =
    (sem_Line_AsIs (sem_Words _words ) )
sem_Line (Line_Groups _extraLine _groups )  =
    (sem_Line_Groups _extraLine (sem_Groups _groups ) )
sem_Line (Line_Named _cref _mbVariantReqm )  =
    (sem_Line_Named _cref _mbVariantReqm )
-- semantic domain
type T_Line  = VariantRefOrderMp ->
               Nm ->
               Int ->
               NmChMp ->
               Opts ->
               Int ->
               XRefExcept ->
               ( HideMp,NmChMp,Int,(Maybe CDoc),MkCDoc,Int,XRefL)
sem_Line_AsIs :: T_Words  ->
                 T_Line 
sem_Line_AsIs words_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _wordsOcolNr :: Int
              _lhsOlineNr :: Int
              _wordsOlCtxt :: ([String])
              _wordsOlAllCtxt :: ([String])
              _wordsOrCtxtUsed :: Int
              _lhsOmkCDoc :: MkCDoc
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOxrefL :: XRefL
              _lhsOseqNr :: Int
              _wordsOopts :: Opts
              _wordsOxrefExcept :: XRefExcept
              _wordsIcdoc :: CDoc
              _wordsIcolNr :: Int
              _wordsIrCtxt :: ([String])
              _wordsIxrefL :: XRefL
              -- "src/shuffle/MainAG.ag"(line 184, column 17)
              _wordsOcolNr =
                  1
              -- "src/shuffle/MainAG.ag"(line 185, column 17)
              _lhsOlineNr =
                  _lhsIlineNr + 1
              -- "src/shuffle/MainAG.ag"(line 233, column 17)
              _wordsOlCtxt =
                  []
              -- "src/shuffle/MainAG.ag"(line 233, column 17)
              _wordsOlAllCtxt =
                  []
              -- "src/shuffle/MainAG.ag"(line 312, column 17)
              _wordsOrCtxtUsed =
                  0
              -- "src/shuffle/MainAG.ag"(line 368, column 17)
              _cdoc =
                  if cdIsEmpty _wordsIcdoc then CDoc_Str "" else _wordsIcdoc
              -- "src/shuffle/MainAG.ag"(line 370, column 17)
              _mbCDoc =
                  Just (CDoc_Pos (CPos (optBaseFPath _lhsIopts) _lhsIlineNr) _cdoc)
              -- "src/shuffle/MainAG.ag"(line 412, column 17)
              _lhsOmkCDoc =
                  const _mbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  _mbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  _wordsIxrefL
              -- copy rule (chain)
              _lhsOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _wordsOopts =
                  _lhsIopts
              -- copy rule (down)
              _wordsOxrefExcept =
                  _lhsIxrefExcept
              ( _wordsIcdoc,_wordsIcolNr,_wordsIrCtxt,_wordsIxrefL) =
                  words_ _wordsOcolNr _wordsOlAllCtxt _wordsOlCtxt _wordsOopts _wordsOrCtxtUsed _wordsOxrefExcept 
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmkCDoc,_lhsOseqNr,_lhsOxrefL)))
sem_Line_Groups :: Int ->
                   T_Groups  ->
                   T_Line 
sem_Line_Groups extraLine_ groups_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _groupsOlineNr :: Int
              _lhsOlineNr :: Int
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOmkCDoc :: MkCDoc
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOxrefL :: XRefL
              _lhsOseqNr :: Int
              _groupsOallowedVariants :: VariantRefOrderMp
              _groupsOchFullNm :: Nm
              _groupsOnmChMp :: NmChMp
              _groupsOopts :: Opts
              _groupsOseqNr :: Int
              _groupsOxrefExcept :: XRefExcept
              _groupsIgathHideMp :: HideMp
              _groupsIgathNmChMp :: NmChMp
              _groupsIlineNr :: Int
              _groupsImbCDoc :: (Maybe CDoc)
              _groupsImbCDocL :: ([(VariantOffer,Maybe CDoc)])
              _groupsImkCDoc :: MkCDoc
              _groupsImkCDocL :: ([(VariantOffer,MkCDoc)])
              _groupsIseqNr :: Int
              -- "src/shuffle/MainAG.ag"(line 187, column 17)
              _groupsLineNr =
                  _lhsIlineNr
              -- "src/shuffle/MainAG.ag"(line 189, column 17)
              _groupsOlineNr =
                  _groupsLineNr + extraLine_
              -- "src/shuffle/MainAG.ag"(line 190, column 17)
              _lhsOlineNr =
                  _groupsIlineNr + extraLine_
              -- "src/shuffle/MainAG.ag"(line 371, column 17)
              _lhsOmbCDoc =
                  maybe Nothing snd $ initlast $ sortOnVariantRefOrderMp _lhsIallowedVariants _groupsImbCDocL
              -- "src/shuffle/MainAG.ag"(line 413, column 17)
              _lhsOmkCDoc =
                  \sel -> let mkCDocSortL = sortOnVariantRefOrderMp' (variantOfferAllLE_4 (_lhsIopts {optGenReqm=sel})) _groupsImkCDocL
                          in  maybe Nothing (\(_,((_,valid),mk)) -> if valid then mk sel else Nothing)
                              $ initlast mkCDocSortL
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  _groupsIgathHideMp
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  _groupsIgathNmChMp
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (up)
              _lhsOseqNr =
                  _groupsIseqNr
              -- copy rule (down)
              _groupsOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _groupsOchFullNm =
                  _lhsIchFullNm
              -- copy rule (down)
              _groupsOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _groupsOopts =
                  _lhsIopts
              -- copy rule (down)
              _groupsOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _groupsOxrefExcept =
                  _lhsIxrefExcept
              ( _groupsIgathHideMp,_groupsIgathNmChMp,_groupsIlineNr,_groupsImbCDoc,_groupsImbCDocL,_groupsImkCDoc,_groupsImkCDocL,_groupsIseqNr) =
                  groups_ _groupsOallowedVariants _groupsOchFullNm _groupsOlineNr _groupsOnmChMp _groupsOopts _groupsOseqNr _groupsOxrefExcept 
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmkCDoc,_lhsOseqNr,_lhsOxrefL)))
sem_Line_Named :: CRef ->
                  (Maybe VariantReqm) ->
                  T_Line 
sem_Line_Named cref_ mbVariantReqm_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _lhsOlineNr :: Int
              _lhsOmkCDoc :: MkCDoc
              _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOxrefL :: XRefL
              _lhsOseqNr :: Int
              -- "src/shuffle/MainAG.ag"(line 186, column 17)
              _lhsOlineNr =
                  _lhsIlineNr + 1
              -- "src/shuffle/MainAG.ag"(line 369, column 17)
              _cdoc =
                  CDoc_Ref cref_ mbVariantReqm_ ChHere
              -- "src/shuffle/MainAG.ag"(line 370, column 17)
              _mbCDoc =
                  Just (CDoc_Pos (CPos (optBaseFPath _lhsIopts) _lhsIlineNr) _cdoc)
              -- "src/shuffle/MainAG.ag"(line 412, column 17)
              _lhsOmkCDoc =
                  const _mbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  _mbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (chain)
              _lhsOseqNr =
                  _lhsIseqNr
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmkCDoc,_lhsOseqNr,_lhsOxrefL)))
-- Lines -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowedVariants      : VariantRefOrderMp
         chFullNm             : Nm
         nmChMp               : NmChMp
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         lineNr               : Int
         seqNr                : Int
      synthesized attributes:
         gathHideMp           : HideMp
         gathNmChMp           : NmChMp
         mbCDoc               : Maybe CDoc
         mkCDoc               : MkCDoc
         xrefL                : XRefL
   alternatives:
      alternative Cons:
         child hd             : Line 
         child tl             : Lines 
      alternative Nil:
-}
type Lines  = [Line ]
-- cata
sem_Lines :: Lines  ->
             T_Lines 
sem_Lines list  =
    (Prelude.foldr sem_Lines_Cons sem_Lines_Nil (Prelude.map sem_Line list) )
-- semantic domain
type T_Lines  = VariantRefOrderMp ->
                Nm ->
                Int ->
                NmChMp ->
                Opts ->
                Int ->
                XRefExcept ->
                ( HideMp,NmChMp,Int,(Maybe CDoc),MkCDoc,Int,XRefL)
sem_Lines_Cons :: T_Line  ->
                  T_Lines  ->
                  T_Lines 
sem_Lines_Cons hd_ tl_  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOmkCDoc :: MkCDoc
              _lhsOxrefL :: XRefL
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              _hdOallowedVariants :: VariantRefOrderMp
              _hdOchFullNm :: Nm
              _hdOlineNr :: Int
              _hdOnmChMp :: NmChMp
              _hdOopts :: Opts
              _hdOseqNr :: Int
              _hdOxrefExcept :: XRefExcept
              _tlOallowedVariants :: VariantRefOrderMp
              _tlOchFullNm :: Nm
              _tlOlineNr :: Int
              _tlOnmChMp :: NmChMp
              _tlOopts :: Opts
              _tlOseqNr :: Int
              _tlOxrefExcept :: XRefExcept
              _hdIgathHideMp :: HideMp
              _hdIgathNmChMp :: NmChMp
              _hdIlineNr :: Int
              _hdImbCDoc :: (Maybe CDoc)
              _hdImkCDoc :: MkCDoc
              _hdIseqNr :: Int
              _hdIxrefL :: XRefL
              _tlIgathHideMp :: HideMp
              _tlIgathNmChMp :: NmChMp
              _tlIlineNr :: Int
              _tlImbCDoc :: (Maybe CDoc)
              _tlImkCDoc :: MkCDoc
              _tlIseqNr :: Int
              _tlIxrefL :: XRefL
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  _hdIgathHideMp `Map.union` _tlIgathHideMp
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  _hdIgathNmChMp `Map.union` _tlIgathNmChMp
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  _hdImbCDoc `mbCDocCmb` _tlImbCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 408, column 36)
              _lhsOmkCDoc =
                  _hdImkCDoc `mkCDocCmb` _tlImkCDoc
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  _hdIxrefL `Seq.union` _tlIxrefL
              -- copy rule (up)
              _lhsOlineNr =
                  _tlIlineNr
              -- copy rule (up)
              _lhsOseqNr =
                  _tlIseqNr
              -- copy rule (down)
              _hdOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _hdOchFullNm =
                  _lhsIchFullNm
              -- copy rule (down)
              _hdOlineNr =
                  _lhsIlineNr
              -- copy rule (down)
              _hdOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOseqNr =
                  _lhsIseqNr
              -- copy rule (down)
              _hdOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _tlOallowedVariants =
                  _lhsIallowedVariants
              -- copy rule (down)
              _tlOchFullNm =
                  _lhsIchFullNm
              -- copy rule (chain)
              _tlOlineNr =
                  _hdIlineNr
              -- copy rule (down)
              _tlOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOseqNr =
                  _hdIseqNr
              -- copy rule (down)
              _tlOxrefExcept =
                  _lhsIxrefExcept
              ( _hdIgathHideMp,_hdIgathNmChMp,_hdIlineNr,_hdImbCDoc,_hdImkCDoc,_hdIseqNr,_hdIxrefL) =
                  hd_ _hdOallowedVariants _hdOchFullNm _hdOlineNr _hdOnmChMp _hdOopts _hdOseqNr _hdOxrefExcept 
              ( _tlIgathHideMp,_tlIgathNmChMp,_tlIlineNr,_tlImbCDoc,_tlImkCDoc,_tlIseqNr,_tlIxrefL) =
                  tl_ _tlOallowedVariants _tlOchFullNm _tlOlineNr _tlOnmChMp _tlOopts _tlOseqNr _tlOxrefExcept 
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmkCDoc,_lhsOseqNr,_lhsOxrefL)))
sem_Lines_Nil :: T_Lines 
sem_Lines_Nil  =
    (\ _lhsIallowedVariants
       _lhsIchFullNm
       _lhsIlineNr
       _lhsInmChMp
       _lhsIopts
       _lhsIseqNr
       _lhsIxrefExcept ->
         (let _lhsOgathHideMp :: HideMp
              _lhsOgathNmChMp :: NmChMp
              _lhsOmbCDoc :: (Maybe CDoc)
              _lhsOmkCDoc :: MkCDoc
              _lhsOxrefL :: XRefL
              _lhsOlineNr :: Int
              _lhsOseqNr :: Int
              -- use rule "src/shuffle/MainAG.ag"(line 427, column 49)
              _lhsOgathHideMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 318, column 55)
              _lhsOgathNmChMp =
                  Map.empty
              -- use rule "src/shuffle/MainAG.ag"(line 364, column 36)
              _lhsOmbCDoc =
                  Nothing
              -- use rule "src/shuffle/MainAG.ag"(line 408, column 36)
              _lhsOmkCDoc =
                  mkCDocEmpty
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (chain)
              _lhsOlineNr =
                  _lhsIlineNr
              -- copy rule (chain)
              _lhsOseqNr =
                  _lhsIseqNr
          in  ( _lhsOgathHideMp,_lhsOgathNmChMp,_lhsOlineNr,_lhsOmbCDoc,_lhsOmkCDoc,_lhsOseqNr,_lhsOxrefL)))
-- MbStrExpr ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         xrefExcept           : XRefExcept
      synthesized attribute:
         mbStr                : Maybe String
   alternatives:
      alternative Just:
         child just           : StrExpr 
      alternative Nothing:
-}
type MbStrExpr  = Maybe StrExpr 
-- cata
sem_MbStrExpr :: MbStrExpr  ->
                 T_MbStrExpr 
sem_MbStrExpr (Prelude.Just x )  =
    (sem_MbStrExpr_Just (sem_StrExpr x ) )
sem_MbStrExpr Prelude.Nothing  =
    sem_MbStrExpr_Nothing
-- semantic domain
type T_MbStrExpr  = Opts ->
                    XRefExcept ->
                    ( (Maybe String))
sem_MbStrExpr_Just :: T_StrExpr  ->
                      T_MbStrExpr 
sem_MbStrExpr_Just just_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOmbStr :: (Maybe String)
              _justOopts :: Opts
              _justOxrefExcept :: XRefExcept
              _justIstr :: String
              -- "src/shuffle/MainAG.ag"(line 222, column 17)
              _lhsOmbStr =
                  Just _justIstr
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOxrefExcept =
                  _lhsIxrefExcept
              ( _justIstr) =
                  just_ _justOopts _justOxrefExcept 
          in  ( _lhsOmbStr)))
sem_MbStrExpr_Nothing :: T_MbStrExpr 
sem_MbStrExpr_Nothing  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOmbStr :: (Maybe String)
              -- "src/shuffle/MainAG.ag"(line 223, column 17)
              _lhsOmbStr =
                  Nothing
          in  ( _lhsOmbStr)))
-- StrExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         xrefExcept           : XRefExcept
      synthesized attribute:
         str                  : String
   alternatives:
      alternative Concat:
         child e1             : StrExpr 
         child e2             : StrExpr 
      alternative Group:
         child e              : StrExpr 
      alternative Seq:
         child es             : StrExprs 
      alternative Str:
         child str            : {String}
      alternative Var:
         child nm             : {String}
      alternative White:
         child e1             : StrExpr 
         child e2             : StrExpr 
-}
data StrExpr  = StrExpr_Concat (StrExpr ) (StrExpr ) 
              | StrExpr_Group (StrExpr ) 
              | StrExpr_Seq (StrExprs ) 
              | StrExpr_Str (String) 
              | StrExpr_Var (String) 
              | StrExpr_White (StrExpr ) (StrExpr ) 
-- cata
sem_StrExpr :: StrExpr  ->
               T_StrExpr 
sem_StrExpr (StrExpr_Concat _e1 _e2 )  =
    (sem_StrExpr_Concat (sem_StrExpr _e1 ) (sem_StrExpr _e2 ) )
sem_StrExpr (StrExpr_Group _e )  =
    (sem_StrExpr_Group (sem_StrExpr _e ) )
sem_StrExpr (StrExpr_Seq _es )  =
    (sem_StrExpr_Seq (sem_StrExprs _es ) )
sem_StrExpr (StrExpr_Str _str )  =
    (sem_StrExpr_Str _str )
sem_StrExpr (StrExpr_Var _nm )  =
    (sem_StrExpr_Var _nm )
sem_StrExpr (StrExpr_White _e1 _e2 )  =
    (sem_StrExpr_White (sem_StrExpr _e1 ) (sem_StrExpr _e2 ) )
-- semantic domain
type T_StrExpr  = Opts ->
                  XRefExcept ->
                  ( String)
sem_StrExpr_Concat :: T_StrExpr  ->
                      T_StrExpr  ->
                      T_StrExpr 
sem_StrExpr_Concat e1_ e2_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstr :: String
              _e1Oopts :: Opts
              _e1OxrefExcept :: XRefExcept
              _e2Oopts :: Opts
              _e2OxrefExcept :: XRefExcept
              _e1Istr :: String
              _e2Istr :: String
              -- "src/shuffle/MainAG.ag"(line 209, column 17)
              _lhsOstr =
                  _e1Istr ++ _e2Istr
              -- copy rule (down)
              _e1Oopts =
                  _lhsIopts
              -- copy rule (down)
              _e1OxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _e2Oopts =
                  _lhsIopts
              -- copy rule (down)
              _e2OxrefExcept =
                  _lhsIxrefExcept
              ( _e1Istr) =
                  e1_ _e1Oopts _e1OxrefExcept 
              ( _e2Istr) =
                  e2_ _e2Oopts _e2OxrefExcept 
          in  ( _lhsOstr)))
sem_StrExpr_Group :: T_StrExpr  ->
                     T_StrExpr 
sem_StrExpr_Group e_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstr :: String
              _eOopts :: Opts
              _eOxrefExcept :: XRefExcept
              _eIstr :: String
              -- copy rule (up)
              _lhsOstr =
                  _eIstr
              -- copy rule (down)
              _eOopts =
                  _lhsIopts
              -- copy rule (down)
              _eOxrefExcept =
                  _lhsIxrefExcept
              ( _eIstr) =
                  e_ _eOopts _eOxrefExcept 
          in  ( _lhsOstr)))
sem_StrExpr_Seq :: T_StrExprs  ->
                   T_StrExpr 
sem_StrExpr_Seq es_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstr :: String
              _esOopts :: Opts
              _esOxrefExcept :: XRefExcept
              _esIstrL :: ([String])
              -- "src/shuffle/MainAG.ag"(line 211, column 17)
              _lhsOstr =
                  "(" ++ concat (intersperse "," _esIstrL) ++ ")"
              -- copy rule (down)
              _esOopts =
                  _lhsIopts
              -- copy rule (down)
              _esOxrefExcept =
                  _lhsIxrefExcept
              ( _esIstrL) =
                  es_ _esOopts _esOxrefExcept 
          in  ( _lhsOstr)))
sem_StrExpr_Str :: String ->
                   T_StrExpr 
sem_StrExpr_Str str_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstr :: String
              -- "src/shuffle/MainAG.ag"(line 207, column 17)
              _lhsOstr =
                  str_
          in  ( _lhsOstr)))
sem_StrExpr_Var :: String ->
                   T_StrExpr 
sem_StrExpr_Var nm_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstr :: String
              -- "src/shuffle/MainAG.ag"(line 208, column 17)
              _lhsOstr =
                  Map.findWithDefault "" nm_ (optDefs _lhsIopts)
          in  ( _lhsOstr)))
sem_StrExpr_White :: T_StrExpr  ->
                     T_StrExpr  ->
                     T_StrExpr 
sem_StrExpr_White e1_ e2_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstr :: String
              _e1Oopts :: Opts
              _e1OxrefExcept :: XRefExcept
              _e2Oopts :: Opts
              _e2OxrefExcept :: XRefExcept
              _e1Istr :: String
              _e2Istr :: String
              -- "src/shuffle/MainAG.ag"(line 210, column 17)
              _lhsOstr =
                  _e1Istr ++ " " ++ _e2Istr
              -- copy rule (down)
              _e1Oopts =
                  _lhsIopts
              -- copy rule (down)
              _e1OxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _e2Oopts =
                  _lhsIopts
              -- copy rule (down)
              _e2OxrefExcept =
                  _lhsIxrefExcept
              ( _e1Istr) =
                  e1_ _e1Oopts _e1OxrefExcept 
              ( _e2Istr) =
                  e2_ _e2Oopts _e2OxrefExcept 
          in  ( _lhsOstr)))
-- StrExprs ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         xrefExcept           : XRefExcept
      synthesized attribute:
         strL                 : [String]
   alternatives:
      alternative Cons:
         child hd             : StrExpr 
         child tl             : StrExprs 
      alternative Nil:
-}
type StrExprs  = [StrExpr ]
-- cata
sem_StrExprs :: StrExprs  ->
                T_StrExprs 
sem_StrExprs list  =
    (Prelude.foldr sem_StrExprs_Cons sem_StrExprs_Nil (Prelude.map sem_StrExpr list) )
-- semantic domain
type T_StrExprs  = Opts ->
                   XRefExcept ->
                   ( ([String]))
sem_StrExprs_Cons :: T_StrExpr  ->
                     T_StrExprs  ->
                     T_StrExprs 
sem_StrExprs_Cons hd_ tl_  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstrL :: ([String])
              _hdOopts :: Opts
              _hdOxrefExcept :: XRefExcept
              _tlOopts :: Opts
              _tlOxrefExcept :: XRefExcept
              _hdIstr :: String
              _tlIstrL :: ([String])
              -- "src/shuffle/MainAG.ag"(line 217, column 17)
              _lhsOstrL =
                  _hdIstr : _tlIstrL
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOxrefExcept =
                  _lhsIxrefExcept
              ( _hdIstr) =
                  hd_ _hdOopts _hdOxrefExcept 
              ( _tlIstrL) =
                  tl_ _tlOopts _tlOxrefExcept 
          in  ( _lhsOstrL)))
sem_StrExprs_Nil :: T_StrExprs 
sem_StrExprs_Nil  =
    (\ _lhsIopts
       _lhsIxrefExcept ->
         (let _lhsOstrL :: ([String])
              -- "src/shuffle/MainAG.ag"(line 216, column 17)
              _lhsOstrL =
                  []
          in  ( _lhsOstrL)))
-- Word --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         opts                 : Opts
         xrefExcept           : XRefExcept
      chained attributes:
         colNr                : Int
         lAllCtxt             : [String]
         lCtxt                : [String]
         rCtxt                : [String]
         rCtxtUsed            : Int
      synthesized attributes:
         cdoc                 : CDoc
         xrefL                : XRefL
   alternatives:
      alternative Black:
         child chars          : {String}
         visit 0:
            local wordColNr   : _
            local _tup2       : _
            local xrefL       : _
      alternative Expand:
         child exp            : StrExpr 
      alternative Inline:
         child inl            : Inline 
      alternative White:
         child chars          : {String}
         visit 0:
            local wordColNr   : _
-}
data Word  = Word_Black (String) 
           | Word_Expand (StrExpr ) 
           | Word_Inline (Inline ) 
           | Word_White (String) 
-- cata
sem_Word :: Word  ->
            T_Word 
sem_Word (Word_Black _chars )  =
    (sem_Word_Black _chars )
sem_Word (Word_Expand _exp )  =
    (sem_Word_Expand (sem_StrExpr _exp ) )
sem_Word (Word_Inline _inl )  =
    (sem_Word_Inline (sem_Inline _inl ) )
sem_Word (Word_White _chars )  =
    (sem_Word_White _chars )
-- semantic domain
type T_Word  = Int ->
               ([String]) ->
               ([String]) ->
               Opts ->
               ([String]) ->
               Int ->
               XRefExcept ->
               ( CDoc,Int,([String]),([String]),([String]),Int,XRefL)
sem_Word_Black :: String ->
                  T_Word 
sem_Word_Black chars_  =
    (\ _lhsIcolNr
       _lhsIlAllCtxt
       _lhsIlCtxt
       _lhsIopts
       _lhsIrCtxt
       _lhsIrCtxtUsed
       _lhsIxrefExcept ->
         (let _lhsOcolNr :: Int
              _lhsOrCtxt :: ([String])
              _lhsOlCtxt :: ([String])
              _lhsOlAllCtxt :: ([String])
              _lhsOrCtxtUsed :: Int
              _lhsOxrefL :: XRefL
              _lhsOcdoc :: CDoc
              -- "src/shuffle/MainAG.ag"(line 197, column 17)
              _wordColNr =
                  _lhsIcolNr
              -- "src/shuffle/MainAG.ag"(line 198, column 17)
              _lhsOcolNr =
                  _lhsIcolNr + length chars_
              -- "src/shuffle/MainAG.ag"(line 242, column 17)
              _lhsOrCtxt =
                  chars_ : _lhsIrCtxt
              -- "src/shuffle/MainAG.ag"(line 242, column 17)
              _lhsOlCtxt =
                  chars_ : _lhsIlCtxt
              -- "src/shuffle/MainAG.ag"(line 242, column 17)
              _lhsOlAllCtxt =
                  chars_ : _lhsIlAllCtxt
              -- "src/shuffle/MainAG.ag"(line 281, column 17)
              __tup2 =
                  let ctxtHuge = 10000000
                      loclhs = ["lhs","loc"]
                      none = ([],_lhsIrCtxtUsed - 1)
                      def nms k cUsed
                           = if any (flip Set.member _lhsIxrefExcept) nms then ([],0) else ([XRef k nms],cUsed)
                   in if _lhsIrCtxtUsed <= 0 && isAlpha (head chars_)
                      then case (_lhsIlAllCtxt,_lhsIlCtxt,chars_,_lhsIrCtxt) of
                             (_,("@":_),nm1,(".":nm2:_))
                               | nm1 `elem` loclhs          -> def [nm2] XRAgAttrUse 2
                             (_,("@":_),nm1,(".":nm2:_))    -> def [nm2,nm1] XRAgAttrUse 2
                             (_,("@":_),nm1,_)              -> def [nm1] XRAgAttrUse 0
                             (_,("|":_),nm1,_)              -> def [nm1] XRAgAltDef 0
                             (_,(".":ll:_),nm1,("=":_))
                               | ll `elem` loclhs           -> def [nm1] XRAgAttrDef 1
                             (_,(".":nm2:_),nm1,("=":_))    -> def [nm1,nm2] XRAgAttrDef 1
                             (_,_,nm1,([sep]:nm2:_))
                               | sep `elem` "._"            -> def [nm2,nm1] XRHsUse 2
                               | otherwise                  -> none
                             (_,["SEM"],nm1,_)              -> def [nm1] XRAgSemDef ctxtHuge
                             (_,["data"],nm1,_)             -> def [nm1] XRHsDef 0
                             (_,["type"],nm1,_)             -> def [nm1] XRHsDef 0
                             ([],_,nm1,_)                   -> def [nm1] XRHsDef ctxtHuge
                             (_,_,nm1,_)
                               | nm1 `notElem` loclhs       -> def [nm1] XRHsUse 0
                             _                              -> none
                      else none
              -- "src/shuffle/MainAG.ag"(line 281, column 17)
              (_xrefL,_) =
                  __tup2
              -- "src/shuffle/MainAG.ag"(line 281, column 17)
              (_,_lhsOrCtxtUsed) =
                  __tup2
              -- "src/shuffle/MainAG.ag"(line 308, column 17)
              _lhsOxrefL =
                  Seq.fromList _xrefL
              -- "src/shuffle/MainAG.ag"(line 390, column 17)
              _lhsOcdoc =
                  cd chars_
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOlAllCtxt,_lhsOlCtxt,_lhsOrCtxt,_lhsOrCtxtUsed,_lhsOxrefL)))
sem_Word_Expand :: T_StrExpr  ->
                   T_Word 
sem_Word_Expand exp_  =
    (\ _lhsIcolNr
       _lhsIlAllCtxt
       _lhsIlCtxt
       _lhsIopts
       _lhsIrCtxt
       _lhsIrCtxtUsed
       _lhsIxrefExcept ->
         (let _lhsOcdoc :: CDoc
              _lhsOxrefL :: XRefL
              _lhsOcolNr :: Int
              _lhsOlAllCtxt :: ([String])
              _lhsOlCtxt :: ([String])
              _lhsOrCtxt :: ([String])
              _lhsOrCtxtUsed :: Int
              _expOopts :: Opts
              _expOxrefExcept :: XRefExcept
              _expIstr :: String
              -- "src/shuffle/MainAG.ag"(line 391, column 17)
              _lhsOcdoc =
                  cd _expIstr
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (chain)
              _lhsOcolNr =
                  _lhsIcolNr
              -- copy rule (chain)
              _lhsOlAllCtxt =
                  _lhsIlAllCtxt
              -- copy rule (chain)
              _lhsOlCtxt =
                  _lhsIlCtxt
              -- copy rule (chain)
              _lhsOrCtxt =
                  _lhsIrCtxt
              -- copy rule (chain)
              _lhsOrCtxtUsed =
                  _lhsIrCtxtUsed
              -- copy rule (down)
              _expOopts =
                  _lhsIopts
              -- copy rule (down)
              _expOxrefExcept =
                  _lhsIxrefExcept
              ( _expIstr) =
                  exp_ _expOopts _expOxrefExcept 
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOlAllCtxt,_lhsOlCtxt,_lhsOrCtxt,_lhsOrCtxtUsed,_lhsOxrefL)))
sem_Word_Inline :: T_Inline  ->
                   T_Word 
sem_Word_Inline inl_  =
    (\ _lhsIcolNr
       _lhsIlAllCtxt
       _lhsIlCtxt
       _lhsIopts
       _lhsIrCtxt
       _lhsIrCtxtUsed
       _lhsIxrefExcept ->
         (let _lhsOcdoc :: CDoc
              _lhsOxrefL :: XRefL
              _lhsOcolNr :: Int
              _lhsOlAllCtxt :: ([String])
              _lhsOlCtxt :: ([String])
              _lhsOrCtxt :: ([String])
              _lhsOrCtxtUsed :: Int
              _inlOcolNr :: Int
              _inlOopts :: Opts
              _inlOxrefExcept :: XRefExcept
              _inlIcdoc :: CDoc
              _inlIcolNr :: Int
              _inlIxrefL :: XRefL
              -- use rule "src/shuffle/MainAG.ag"(line 387, column 25)
              _lhsOcdoc =
                  _inlIcdoc
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  _inlIxrefL
              -- copy rule (up)
              _lhsOcolNr =
                  _inlIcolNr
              -- copy rule (chain)
              _lhsOlAllCtxt =
                  _lhsIlAllCtxt
              -- copy rule (chain)
              _lhsOlCtxt =
                  _lhsIlCtxt
              -- copy rule (chain)
              _lhsOrCtxt =
                  _lhsIrCtxt
              -- copy rule (chain)
              _lhsOrCtxtUsed =
                  _lhsIrCtxtUsed
              -- copy rule (down)
              _inlOcolNr =
                  _lhsIcolNr
              -- copy rule (down)
              _inlOopts =
                  _lhsIopts
              -- copy rule (down)
              _inlOxrefExcept =
                  _lhsIxrefExcept
              ( _inlIcdoc,_inlIcolNr,_inlIxrefL) =
                  inl_ _inlOcolNr _inlOopts _inlOxrefExcept 
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOlAllCtxt,_lhsOlCtxt,_lhsOrCtxt,_lhsOrCtxtUsed,_lhsOxrefL)))
sem_Word_White :: String ->
                  T_Word 
sem_Word_White chars_  =
    (\ _lhsIcolNr
       _lhsIlAllCtxt
       _lhsIlCtxt
       _lhsIopts
       _lhsIrCtxt
       _lhsIrCtxtUsed
       _lhsIxrefExcept ->
         (let _lhsOcolNr :: Int
              _lhsOlAllCtxt :: ([String])
              _lhsOrCtxtUsed :: Int
              _lhsOcdoc :: CDoc
              _lhsOxrefL :: XRefL
              _lhsOlCtxt :: ([String])
              _lhsOrCtxt :: ([String])
              -- "src/shuffle/MainAG.ag"(line 197, column 17)
              _wordColNr =
                  _lhsIcolNr
              -- "src/shuffle/MainAG.ag"(line 198, column 17)
              _lhsOcolNr =
                  _lhsIcolNr + length chars_
              -- "src/shuffle/MainAG.ag"(line 245, column 17)
              _lhsOlAllCtxt =
                  chars_ : _lhsIlAllCtxt
              -- "src/shuffle/MainAG.ag"(line 309, column 17)
              _lhsOrCtxtUsed =
                  0
              -- "src/shuffle/MainAG.ag"(line 390, column 17)
              _lhsOcdoc =
                  cd chars_
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (chain)
              _lhsOlCtxt =
                  _lhsIlCtxt
              -- copy rule (chain)
              _lhsOrCtxt =
                  _lhsIrCtxt
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOlAllCtxt,_lhsOlCtxt,_lhsOrCtxt,_lhsOrCtxtUsed,_lhsOxrefL)))
-- Words -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lAllCtxt             : [String]
         lCtxt                : [String]
         opts                 : Opts
         rCtxtUsed            : Int
         xrefExcept           : XRefExcept
      chained attribute:
         colNr                : Int
      synthesized attributes:
         cdoc                 : CDoc
         rCtxt                : [String]
         xrefL                : XRefL
   alternatives:
      alternative Cons:
         child hd             : Word 
         child tl             : Words 
      alternative Nil:
-}
type Words  = [Word ]
-- cata
sem_Words :: Words  ->
             T_Words 
sem_Words list  =
    (Prelude.foldr sem_Words_Cons sem_Words_Nil (Prelude.map sem_Word list) )
-- semantic domain
type T_Words  = Int ->
                ([String]) ->
                ([String]) ->
                Opts ->
                Int ->
                XRefExcept ->
                ( CDoc,Int,([String]),XRefL)
sem_Words_Cons :: T_Word  ->
                  T_Words  ->
                  T_Words 
sem_Words_Cons hd_ tl_  =
    (\ _lhsIcolNr
       _lhsIlAllCtxt
       _lhsIlCtxt
       _lhsIopts
       _lhsIrCtxtUsed
       _lhsIxrefExcept ->
         (let _hdOrCtxt :: ([String])
              _lhsOrCtxt :: ([String])
              _lhsOcdoc :: CDoc
              _lhsOxrefL :: XRefL
              _lhsOcolNr :: Int
              _hdOcolNr :: Int
              _hdOlAllCtxt :: ([String])
              _hdOlCtxt :: ([String])
              _hdOopts :: Opts
              _hdOrCtxtUsed :: Int
              _hdOxrefExcept :: XRefExcept
              _tlOcolNr :: Int
              _tlOlAllCtxt :: ([String])
              _tlOlCtxt :: ([String])
              _tlOopts :: Opts
              _tlOrCtxtUsed :: Int
              _tlOxrefExcept :: XRefExcept
              _hdIcdoc :: CDoc
              _hdIcolNr :: Int
              _hdIlAllCtxt :: ([String])
              _hdIlCtxt :: ([String])
              _hdIrCtxt :: ([String])
              _hdIrCtxtUsed :: Int
              _hdIxrefL :: XRefL
              _tlIcdoc :: CDoc
              _tlIcolNr :: Int
              _tlIrCtxt :: ([String])
              _tlIxrefL :: XRefL
              -- "src/shuffle/MainAG.ag"(line 238, column 17)
              _hdOrCtxt =
                  _tlIrCtxt
              -- "src/shuffle/MainAG.ag"(line 239, column 17)
              _lhsOrCtxt =
                  _hdIrCtxt
              -- use rule "src/shuffle/MainAG.ag"(line 387, column 25)
              _lhsOcdoc =
                  _hdIcdoc .|. _tlIcdoc
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  _hdIxrefL `Seq.union` _tlIxrefL
              -- copy rule (up)
              _lhsOcolNr =
                  _tlIcolNr
              -- copy rule (down)
              _hdOcolNr =
                  _lhsIcolNr
              -- copy rule (down)
              _hdOlAllCtxt =
                  _lhsIlAllCtxt
              -- copy rule (down)
              _hdOlCtxt =
                  _lhsIlCtxt
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrCtxtUsed =
                  _lhsIrCtxtUsed
              -- copy rule (down)
              _hdOxrefExcept =
                  _lhsIxrefExcept
              -- copy rule (chain)
              _tlOcolNr =
                  _hdIcolNr
              -- copy rule (chain)
              _tlOlAllCtxt =
                  _hdIlAllCtxt
              -- copy rule (chain)
              _tlOlCtxt =
                  _hdIlCtxt
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOrCtxtUsed =
                  _hdIrCtxtUsed
              -- copy rule (down)
              _tlOxrefExcept =
                  _lhsIxrefExcept
              ( _hdIcdoc,_hdIcolNr,_hdIlAllCtxt,_hdIlCtxt,_hdIrCtxt,_hdIrCtxtUsed,_hdIxrefL) =
                  hd_ _hdOcolNr _hdOlAllCtxt _hdOlCtxt _hdOopts _hdOrCtxt _hdOrCtxtUsed _hdOxrefExcept 
              ( _tlIcdoc,_tlIcolNr,_tlIrCtxt,_tlIxrefL) =
                  tl_ _tlOcolNr _tlOlAllCtxt _tlOlCtxt _tlOopts _tlOrCtxtUsed _tlOxrefExcept 
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOrCtxt,_lhsOxrefL)))
sem_Words_Nil :: T_Words 
sem_Words_Nil  =
    (\ _lhsIcolNr
       _lhsIlAllCtxt
       _lhsIlCtxt
       _lhsIopts
       _lhsIrCtxtUsed
       _lhsIxrefExcept ->
         (let _lhsOrCtxt :: ([String])
              _lhsOcdoc :: CDoc
              _lhsOxrefL :: XRefL
              _lhsOcolNr :: Int
              -- "src/shuffle/MainAG.ag"(line 237, column 17)
              _lhsOrCtxt =
                  []
              -- use rule "src/shuffle/MainAG.ag"(line 387, column 25)
              _lhsOcdoc =
                  CDoc_Emp
              -- use rule "src/shuffle/MainAG.ag"(line 276, column 34)
              _lhsOxrefL =
                  Seq.empty
              -- copy rule (chain)
              _lhsOcolNr =
                  _lhsIcolNr
          in  ( _lhsOcdoc,_lhsOcolNr,_lhsOrCtxt,_lhsOxrefL)))