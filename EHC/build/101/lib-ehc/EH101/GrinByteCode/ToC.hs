

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinByteCode/ToC.ag)
module EH101.GrinByteCode.ToC(gbmod2C) where

import EH.Util.Pretty
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Opts
import EH101.GrinByteCode
import EH.Util.Pretty
import EH101.Base.Common
import EH101.Opts
import qualified EH101.Config as Cfg
import EH101.GrinByteCode
import EH101.LamInfo
import EH101.Base.GenC
import Data.Maybe
import Data.Char
import Data.Bits
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils
import qualified Data.Map as Map











gbmod2C
  :: EHCOpts
     -> LookupGrinByteCodeLamInfo
    -> Module
    -> ( GenC
       , GenC
       , FunctionInfoExportMp
       )
gbmod2C opts lkupGrinByteCodeLamInfo m
  = let t = wrap_AGItf (sem_AGItf (AGItf_AGItf m))
                       (Inh_AGItf
                          { opts_Inh_AGItf = opts
                          , lkupGrinByteCodeLamInfo_Inh_AGItf = lkupGrinByteCodeLamInfo
                          }
                       )
    in  ( pp_Syn_AGItf t
        , ppMain_Syn_AGItf t
        , functionInfoExportMp_Syn_AGItf t
        )



type LookupFunctionInfoInx     = HsName -> Maybe Int



type LookupGrinByteCodeLamInfo = HsName -> Maybe GrinByteCodeLamInfo



ppCmtC :: PP a => EHCOpts -> a -> GenC
ppCmtC opts x | ehcOptGenCmt opts = gencCmt x
              | otherwise         = empty

ppHex :: (Show a, Integral a) => Int -> a -> GenC
ppHex prec x = head $ ppHex' Nothing prec x

ppHex' :: (Show a, Integral a) => Maybe Int -> Int -> a -> [GenC]
ppHex' = ppHex'' "0x"

{-
ppHex'' :: (Show a, Integral a) => String -> Maybe Int -> Int -> a -> [GenC]
ppHex'' prefix split prec x
  = case split of
      Nothing  -> [prefix >|< h]
      Just spl -> map (prefix >|<) $ s h
               where s l = case splitAt spl l of
                             ([],_ ) -> []
                             (l1,l2) -> l1 : s l2
  where h = strHex prec x
-}

ppHex'' :: (Show a, Integral a) => String -> Maybe Int -> Int -> a -> [GenC]
ppHex'' prefix split prec x
  = case split of
      Nothing  -> [prefix >|< strHex prec x]
      Just spl -> map ((prefix >|<) . strHex spl) l
               where (_,l) = splitByRadix (prec `div` spl) (16 ^ spl) x



type LocRefLoc = (LocRef,CodeAbsLoc)



mkLabelLocMp :: CodeAbsLoc -> LinkChainEntry -> LabelLocMp
mkLabelLocMp cloc lce = maybe Map.empty (\l -> Map.singleton l cloc) $ lckeyMbLbl $ lcentryKey lce



mkLinkChainKeyMp :: LinkChainEntry -> LinkChainKeyMp
mkLinkChainKeyMp lce = Map.singleton (lcentryKey lce) (lcentryInfo lce)



updLinkChainResolvedIndInfoSet :: LinkChainEntry -> LinkChainResolvedMp -> LinkChainResolvedIndInfoSet -> (Integer,LinkChainResolvedIndInfoSet)
updLinkChainResolvedIndInfoSet lce mp lcSet
  = maybe (0,lcSet) (linkChainResolvedInfoEncoding lcSet) $ linkChainResolvedLookup (lcentryKey lce) mp



data BytePoolEntry
  = BytePool_String !String
  | BytePool_Bytes  ![Int]
  | BytePool_None
  deriving(Eq,Ord)

bytePoolEntrySize :: BytePoolEntry -> Int
bytePoolEntrySize (BytePool_String s) = length s + 1
bytePoolEntrySize (BytePool_Bytes  b) = length b
bytePoolEntrySize _                   = 0

bytePoolEntryEncoding :: BytePoolEntry -> [GenC]
bytePoolEntryEncoding (BytePool_String s) = [ppCommas $ ppStringAsHex' s]
bytePoolEntryEncoding (BytePool_Bytes  b) = [ppCommas $ ppIntsAsHex' b]
bytePoolEntryEncoding _                   = []

bytePoolEntryComment :: BytePoolEntry -> GenC
bytePoolEntryComment (BytePool_String s) = genc s
bytePoolEntryComment _                   = empty

type BytePoolLoc = (Int,Int)    -- mp key, bytepool offset

emptyBytePoolLoc :: BytePoolLoc
emptyBytePoolLoc = (-1, -1)

bytePoolLocOff :: BytePoolLoc -> Int
bytePoolLocOff = snd

type BytePoolMp = (BytePoolLoc,Map.Map BytePoolEntry BytePoolLoc)

emptyBytePoolMp :: BytePoolMp
emptyBytePoolMp = ((0,0),Map.empty)

bytePoolMpAdd :: BytePoolEntry -> BytePoolMp -> (BytePoolMp,BytePoolLoc)
bytePoolMpAdd e mp@((sz,bpoff),m)
  = case Map.lookup e m of
      Just x -> (mp,x)
      _      -> (((sz+1,bpoff + bytePoolEntrySize e),Map.insert e e' m),e')
             where e' = (sz,bpoff)



type BPEBasedMp mkinfo extra = (Int,Map.Map (mkinfo (extra, BytePoolLoc)) Int)

emptyBPEBasedMp :: BPEBasedMp mkinfo extra
emptyBPEBasedMp = (0,Map.empty)

bpeBasedMpMp (_,m) = m

bpeBasedMpAdd
  :: Ord (mkinfo (extra, BytePoolLoc))
     => (mkinfo orig -> Bool)
     -> (mkinfo orig -> Maybe BytePoolEntry)
     -> (BytePoolLoc -> mkinfo orig -> mkinfo (extra, BytePoolLoc))
     -> (Int -> BytePoolLoc -> res)
     -> mkinfo orig -> BPEBasedMp mkinfo extra -> BytePoolMp -> (BPEBasedMp mkinfo extra,BytePoolMp,Maybe res)
bpeBasedMpAdd validInfo getBytes updInfo mkRes
              info mp@(szm,m) bpmp
  = if validInfo info
    then case Map.lookup info' m of
           x@(Just inx)
               -> (mp,bpmp,Just (mkRes inx loc))
           _   -> ((szm+1,Map.insert info' szm m),bpmp',Just (mkRes szm loc))
    else (mp,bpmp,Nothing)
  where (bpmp',loc) = maybe (bpmp,emptyBytePoolLoc) (\bytes -> bytePoolMpAdd bytes bpmp) (getBytes info)
        info' = updInfo loc info



bpeExtractMp :: Ord k => (mkinfo (extra, BytePoolLoc) -> k) -> (Int -> res) -> BPEBasedMp mkinfo extra -> Map.Map k res
bpeExtractMp getKey mk (_,m) = Map.fromList [ (getKey info, mk inx) | (info,inx) <- Map.toList m ]



-- type GCStackInfoRepl x = (Int,x)
type GCStackInfoMp = BPEBasedMp GCStackInfo' Int

gcStackInfoMpAdd :: GCStackInfo -> GCStackInfoMp -> BytePoolMp -> (GCStackInfoMp,BytePoolMp,Maybe Int)
gcStackInfoMpAdd
  = bpeBasedMpAdd valid
                  (\i -> Just $ BytePool_Bytes $ gcstinfoPerms i)
                  (\loc i -> i {gcstinfoPerms = (length $ gcstinfoPerms i, loc)})
                  (\inx _ -> inx)
  where valid (GCStackInfo_None) = False
        valid i                  = gcstinfoSz i > 0



-- type StringConstRepl x = x
type StringConstMp = BPEBasedMp StringConst' ()

stringConstMpAdd :: StringConst -> StringConstMp -> BytePoolMp -> (StringConstMp,BytePoolMp,Maybe Int)
stringConstMpAdd = bpeBasedMpAdd (const True) (\(StringConst s) -> Just $ BytePool_String s) (\loc _ -> StringConst ((),loc)) (\_ loc -> bytePoolLocOff loc)





type FunctionInfoMp = BPEBasedMp FunctionInfo' ()

functionInfoMpAdd :: FunctionInfo -> FunctionInfoMp -> BytePoolMp -> (FunctionInfoMp,BytePoolMp,Maybe Int)
functionInfoMpAdd = bpeBasedMpAdd (const True) (\i -> Just $ BytePool_String $ funinfoKey i) (\loc i -> i { funinfoKey = ((),loc) }) const



type CallInfoKey
  = (Int,Maybe (Int,Int))                       -- the key: string constant index (for now), module + functioninfo index (under construction, 20100302 AD)

cikeyStrInx :: CallInfoKey -> Int
cikeyStrInx (x,_) = x

type CallInfoMp
  = ( Int                                       -- counter for index of CallInfo
    , Map.Map (CallInfo' (Maybe Int)            -- optional gc stack info index
                         CallInfoKey
              )
              Int                               -- index of CallInfo
    )

emptyCallInfoMp :: CallInfoMp
emptyCallInfoMp = (0,Map.empty)

ciMpAdd :: CallInfo' (Maybe Int) CallInfoKey -> CallInfoMp -> (CallInfoMp,Int)
ciMpAdd ci mp@(sz,m)
  = case Map.lookup ci m of
      Just i -> (mp,i)
      _      -> ((sz+1,Map.insert ci sz m),sz)



type BCTrL = [(Int,Int,GenC)]



type AnnL = [(AnnKind,GenC)]



enc_InsOp_Deref_Zero   = 0
enc_InsOp_LocE_Imm     = 2
enc_InsOp_ImmSz_Bits32 = 2
enc_InsOp_ImmSz_Bits64 = 3

insOpImmSz2Enc :: Int -> Int
insOpImmSz2Enc 4 = enc_InsOp_ImmSz_Bits32
insOpImmSz2Enc 8 = enc_InsOp_ImmSz_Bits64
insOpImmSz2Enc x = panic ("ToC.insOpImmSz2Enc:" ++ show x)

-- enc of InsOpImmSz -> nr bytes
insOpImmSzEnc2NrBytes :: Int -> Int
insOpImmSzEnc2NrBytes e = 1 `shiftL` e

-- enc of InsOpImmSz -> nr bits
insOpImmSzEnc2NrBits :: Int -> Int
insOpImmSzEnc2NrBits e = 8 * insOpImmSzEnc2NrBytes e



vmCall :: (PP nm) => nm -> [GenC] -> GenC
vmCall nm args = gencCall ("GB_VM_" >|< nm) args

vmLbl :: PP l => l -> GenC
vmLbl l = "L_" >|< l



type Fun2CMp = Map.Map String [GenC]



-- Names of tables
strNmGcStackInfos = "gcStackInfos"



nl :: GenC
nl = text " "

ppHexL :: [GenC] -> GenC
ppHexL
  = ppCommas . cvt
  where cvt = if Cfg.machineIsBigEndian then id else reverse

ppArr :: PP a => [a] -> GenC
ppArr l = indent 2 (gencEndsemic $ gencArray l)

ppArr' :: PP a => [a] -> [GenC]
ppArr' = gencArrayV' ";"

ppNodeFlds :: PP n => n -> GenC
ppNodeFlds n = n >|< ".content.fields"

ppNodeFld :: (PP n,PP o) => n -> o -> GenC
ppNodeFld n o = gencArrayAt (ppNodeFlds n) o

ppCnst :: EHCOpts -> Const -> GenC
ppCnst opts (Const_CFunction n)
  = gencCast "GB_Word" $ gencAddrOf (pref ++ {- (if ehcOptPriv opts then "priv_" else "") ++ -} n)
  where pref = ""  -- if take 4 n == "prim" then "gb_" else ""
ppCnst opts (Const_CCallEncWrapper szs)
  = ppCnst opts (Const_CFunction $ gencBasicSizeGBFunTyNm gencBasicSizeFunPrefix szs)

ppLine :: GenC -> GenC
ppLine p = p >-< text ""

ppFun :: (PP h, PP b) => h -> b -> GenC
ppFun h b = h >#< "{" >-< indent 2 b >-< "}"

mkPre :: String -> String
mkPre = (++ "_")

ppTbl :: String -> GenC -> [GenC] -> GenC
ppTbl tp nm tbl = ppLine $ gencStatic $ tp >#< nm >|< "[] =" >-< ppArr tbl

ppTbl' :: String -> GenC -> [GenC] -> [GenC]
ppTbl' tp nm tbl = [nl, gencStatic $ tp >#< nm >|< "[] ="] ++ ppArr' tbl



ppIntsAsHex' :: [Int] -> [GenC]
ppIntsAsHex' xx = [ hlist $ ppHex' (Just 2) 2 x | x <- xx ]

ppIntsAsHex :: [Int] -> GenC
ppIntsAsHex = ppCurlysCommas . ppIntsAsHex'

ppBytesAsString :: [Int] -> GenC
ppBytesAsString xx = "\"" >|< hlist [ ppHex'' "\\x" (Just 2) 2 x | x <- xx ] >|< "\""

ppStringAsHex' :: String -> [GenC]
ppStringAsHex' s = ppIntsAsHex' [ ord c | c <- s ++ [chr 0] ]

ppStringAsHex :: String -> GenC
ppStringAsHex = ppCurlysCommas . ppStringAsHex'
-- ppStringAsHex s = ppCurlysCommas [ ppHex' (Just 2) 2 (ord c) | c <- s ++ [chr 0] ]

ppWord :: (Show c, Integral c) => c -> GenC
ppWord = ppHexL . ppHex' (Just 2) (Cfg.sizeofWord * 2)

ppLinkChain :: Integer -> GenC
ppLinkChain = ppWord

ppStaticDef :: String -> GenC -> String -> GenC -> GenC
ppStaticDef ty nm modf def -- = "static" >#< ty >#< nm >|< modf >#< "=" >-< indent 2 (def >#< ";")
  = gencStatic $ gencVarDeclInitV ty (nm >|< modf) def



bcodeWord' :: (Integral c) => Int -> c -> [Int]
bcodeWord' sz x
  = (if Cfg.machineIsBigEndian then id else reverse) $ snd $ splitByRadix sz 256 x -- ppHexL . ppHex' (Just 2) (Cfg.sizeofWord * 2)

bcodeWord :: (Integral c) => c -> [Int]
bcodeWord = bcodeWord' Cfg.sizeofWord

bcodeLinkChain :: Integer -> [Int]
bcodeLinkChain = bcodeWord

-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkupGrinByteCodeLamInfo : LookupGrinByteCodeLamInfo
         opts                 : EHCOpts
      synthesized attributes:
         functionInfoExportMp : FunctionInfoExportMp
         pp                   : GenC
         ppMain               : GenC
   alternatives:
      alternative AGItf:
         child module         : Module 
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _module )  =
    (sem_AGItf_AGItf (sem_Module _module ) )
-- semantic domain
type T_AGItf  = LookupGrinByteCodeLamInfo ->
                EHCOpts ->
                ( FunctionInfoExportMp,GenC,GenC)
data Inh_AGItf  = Inh_AGItf {lkupGrinByteCodeLamInfo_Inh_AGItf :: !(LookupGrinByteCodeLamInfo),opts_Inh_AGItf :: !(EHCOpts)}
data Syn_AGItf  = Syn_AGItf {functionInfoExportMp_Syn_AGItf :: !(FunctionInfoExportMp),pp_Syn_AGItf :: !(GenC),ppMain_Syn_AGItf :: !(GenC)}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsIlkupGrinByteCodeLamInfo _lhsIopts )  =
    (let ( _lhsOfunctionInfoExportMp,_lhsOpp,_lhsOppMain) = sem _lhsIlkupGrinByteCodeLamInfo _lhsIopts 
     in  (Syn_AGItf _lhsOfunctionInfoExportMp _lhsOpp _lhsOppMain ))
sem_AGItf_AGItf :: T_Module  ->
                   T_AGItf 
sem_AGItf_AGItf module_  =
    (\ _lhsIlkupGrinByteCodeLamInfo
       _lhsIopts ->
         (let _lhsOpp :: GenC
              _lhsOfunctionInfoExportMp :: FunctionInfoExportMp
              _lhsOppMain :: GenC
              _moduleOlkupGrinByteCodeLamInfo :: LookupGrinByteCodeLamInfo
              _moduleOopts :: EHCOpts
              _moduleIfunctionInfoExportMp :: FunctionInfoExportMp
              _moduleIpp :: GenC
              _moduleIppMain :: GenC
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
              _lhsOpp =
                  _moduleIpp
              -- copy rule (up)
              _lhsOfunctionInfoExportMp =
                  _moduleIfunctionInfoExportMp
              -- copy rule (up)
              _lhsOppMain =
                  _moduleIppMain
              -- copy rule (down)
              _moduleOlkupGrinByteCodeLamInfo =
                  _lhsIlkupGrinByteCodeLamInfo
              -- copy rule (down)
              _moduleOopts =
                  _lhsIopts
              ( _moduleIfunctionInfoExportMp,_moduleIpp,_moduleIppMain) =
                  module_ _moduleOlkupGrinByteCodeLamInfo _moduleOopts 
          in  ( _lhsOfunctionInfoExportMp,_lhsOpp,_lhsOppMain)))
-- Imm ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         immSz                : InsOp_ImmSz 
      chained attribute:
         cLoc                 : CodeAbsLoc
      synthesized attributes:
         bcode                : [Int]
         genc                 : GenC
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative Int:
         child int            : {Integer}
         visit 0:
            local genc        : _
            local _tup1       : _
            local ppL         : _
            local sz          : _
-}
-- cata
sem_Imm :: Imm  ->
           T_Imm 
sem_Imm (Imm_Int _int )  =
    (sem_Imm_Int _int )
-- semantic domain
type T_Imm  = CodeAbsLoc ->
              InsOp_ImmSz  ->
              ( ([Int]),CodeAbsLoc,GenC,GenC,GenC)
sem_Imm_Int :: Integer ->
               T_Imm 
sem_Imm_Int int_  =
    (\ _lhsIcLoc
       _lhsIimmSz ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOmn :: GenC
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgenc :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 118, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + _sz
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 571, column 17)
              _lhsOmn =
                  case _lhsIimmSz of
                    InsOp_ImmSz_Bits08 -> ppHex  2 int_
                    InsOp_ImmSz_Bits16 -> ppHex  4 int_
                    InsOp_ImmSz_Bits32 -> ppHex  8 int_
                    InsOp_ImmSz_Bits64 -> ppHex 16 int_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 823, column 21)
              _genc =
                  ppHex (2 * _sz) int_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1169, column 33)
              __tup1 =
                  case _lhsIimmSz of
                    InsOp_ImmSz_Bits08 -> (ppHex' (Just 2)  2 int_,1)
                    InsOp_ImmSz_Bits16 -> (ppHex' (Just 2)  4 int_,2)
                    InsOp_ImmSz_Bits32 -> (ppHex' (Just 2)  8 int_,4)
                    InsOp_ImmSz_Bits64 -> (ppHex' (Just 2) 16 int_,8)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1169, column 33)
              (_ppL,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1169, column 33)
              (_,_sz) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1174, column 17)
              _lhsOpp =
                  ppHexL _ppL
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1202, column 17)
              _lhsObcode =
                  bcodeWord' _sz int_
              -- copy rule (from local)
              _lhsOgenc =
                  _genc
          in  ( _lhsObcode,_lhsOcLoc,_lhsOgenc,_lhsOmn,_lhsOpp)))
-- InsOp_DataOp ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative FloatWord:
      alternative IntInf:
      alternative IntWord:
-}
-- cata
sem_InsOp_DataOp :: InsOp_DataOp  ->
                    T_InsOp_DataOp 
sem_InsOp_DataOp (InsOp_DataOp_FloatWord )  =
    (sem_InsOp_DataOp_FloatWord )
sem_InsOp_DataOp (InsOp_DataOp_IntInf )  =
    (sem_InsOp_DataOp_IntInf )
sem_InsOp_DataOp (InsOp_DataOp_IntWord )  =
    (sem_InsOp_DataOp_IntWord )
-- semantic domain
type T_InsOp_DataOp  = ( Int,GenC,GenC)
sem_InsOp_DataOp_FloatWord :: T_InsOp_DataOp 
sem_InsOp_DataOp_FloatWord  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 606, column 17)
         _lhsOmn =
             genc "fw"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 734, column 17)
         _lhsOenc =
             2
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_DataOp_IntInf :: T_InsOp_DataOp 
sem_InsOp_DataOp_IntInf  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 605, column 17)
         _lhsOmn =
             genc "ii"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 733, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_DataOp_IntWord :: T_InsOp_DataOp 
sem_InsOp_DataOp_IntWord  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 604, column 17)
         _lhsOmn =
             genc "iw"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 732, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
-- InsOp_Deref -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
         self                 : SELF 
   alternatives:
      alternative Int:
         visit 0:
            local self        : _
      alternative One:
         visit 0:
            local self        : _
      alternative Two:
         visit 0:
            local self        : _
      alternative Zero:
         visit 0:
            local self        : _
-}
-- cata
sem_InsOp_Deref :: InsOp_Deref  ->
                   T_InsOp_Deref 
sem_InsOp_Deref (InsOp_Deref_Int )  =
    (sem_InsOp_Deref_Int )
sem_InsOp_Deref (InsOp_Deref_One )  =
    (sem_InsOp_Deref_One )
sem_InsOp_Deref (InsOp_Deref_Two )  =
    (sem_InsOp_Deref_Two )
sem_InsOp_Deref (InsOp_Deref_Zero )  =
    (sem_InsOp_Deref_Zero )
-- semantic domain
type T_InsOp_Deref  = ( Int,GenC,GenC,InsOp_Deref )
sem_InsOp_Deref_Int :: T_InsOp_Deref 
sem_InsOp_Deref_Int  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_Deref 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 618, column 17)
         _lhsOmn =
             genc "i"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 746, column 17)
         _lhsOenc =
             3
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_Deref_Int
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_Deref_One :: T_InsOp_Deref 
sem_InsOp_Deref_One  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_Deref 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 616, column 17)
         _lhsOmn =
             genc "1"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 744, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_Deref_One
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_Deref_Two :: T_InsOp_Deref 
sem_InsOp_Deref_Two  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_Deref 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 617, column 17)
         _lhsOmn =
             genc "2"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 745, column 17)
         _lhsOenc =
             2
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_Deref_Two
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_Deref_Zero :: T_InsOp_Deref 
sem_InsOp_Deref_Zero  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_Deref 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 615, column 17)
         _lhsOmn =
             genc "0"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 743, column 17)
         _lhsOenc =
             enc_InsOp_Deref_Zero
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_Deref_Zero
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
-- InsOp_DerefB ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative One:
      alternative Two:
-}
-- cata
sem_InsOp_DerefB :: InsOp_DerefB  ->
                    T_InsOp_DerefB 
sem_InsOp_DerefB (InsOp_DerefB_One )  =
    (sem_InsOp_DerefB_One )
sem_InsOp_DerefB (InsOp_DerefB_Two )  =
    (sem_InsOp_DerefB_Two )
-- semantic domain
type T_InsOp_DerefB  = ( Int,GenC,GenC)
sem_InsOp_DerefB_One :: T_InsOp_DerefB 
sem_InsOp_DerefB_One  =
    (let _lhsOenc :: Int
         _lhsOmn :: GenC
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 749, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
         _lhsOmn =
             empty
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_DerefB_Two :: T_InsOp_DerefB 
sem_InsOp_DerefB_Two  =
    (let _lhsOenc :: Int
         _lhsOmn :: GenC
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 750, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
         _lhsOmn =
             empty
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
-- InsOp_ImmSz -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
         self                 : SELF 
   alternatives:
      alternative Bits08:
         visit 0:
            local self        : _
      alternative Bits16:
         visit 0:
            local self        : _
      alternative Bits32:
         visit 0:
            local self        : _
      alternative Bits64:
         visit 0:
            local self        : _
-}
-- cata
sem_InsOp_ImmSz :: InsOp_ImmSz  ->
                   T_InsOp_ImmSz 
sem_InsOp_ImmSz (InsOp_ImmSz_Bits08 )  =
    (sem_InsOp_ImmSz_Bits08 )
sem_InsOp_ImmSz (InsOp_ImmSz_Bits16 )  =
    (sem_InsOp_ImmSz_Bits16 )
sem_InsOp_ImmSz (InsOp_ImmSz_Bits32 )  =
    (sem_InsOp_ImmSz_Bits32 )
sem_InsOp_ImmSz (InsOp_ImmSz_Bits64 )  =
    (sem_InsOp_ImmSz_Bits64 )
-- semantic domain
type T_InsOp_ImmSz  = ( Int,GenC,GenC,InsOp_ImmSz )
sem_InsOp_ImmSz_Bits08 :: T_InsOp_ImmSz 
sem_InsOp_ImmSz_Bits08  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_ImmSz 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 609, column 17)
         _lhsOmn =
             genc "08"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 737, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_ImmSz_Bits08
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_ImmSz_Bits16 :: T_InsOp_ImmSz 
sem_InsOp_ImmSz_Bits16  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_ImmSz 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 610, column 17)
         _lhsOmn =
             genc "16"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 738, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_ImmSz_Bits16
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_ImmSz_Bits32 :: T_InsOp_ImmSz 
sem_InsOp_ImmSz_Bits32  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_ImmSz 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 611, column 17)
         _lhsOmn =
             genc "32"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 739, column 17)
         _lhsOenc =
             enc_InsOp_ImmSz_Bits32
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_ImmSz_Bits32
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_ImmSz_Bits64 :: T_InsOp_ImmSz 
sem_InsOp_ImmSz_Bits64  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_ImmSz 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 612, column 17)
         _lhsOmn =
             genc "64"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 740, column 17)
         _lhsOenc =
             enc_InsOp_ImmSz_Bits64
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_ImmSz_Bits64
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
-- InsOp_LocB --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
         self                 : SELF 
   alternatives:
      alternative Reg:
         visit 0:
            local self        : _
      alternative TOS:
         visit 0:
            local self        : _
-}
-- cata
sem_InsOp_LocB :: InsOp_LocB  ->
                  T_InsOp_LocB 
sem_InsOp_LocB (InsOp_LocB_Reg )  =
    (sem_InsOp_LocB_Reg )
sem_InsOp_LocB (InsOp_LocB_TOS )  =
    (sem_InsOp_LocB_TOS )
-- semantic domain
type T_InsOp_LocB  = ( Int,GenC,GenC,InsOp_LocB )
sem_InsOp_LocB_Reg :: T_InsOp_LocB 
sem_InsOp_LocB_Reg  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_LocB 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 585, column 17)
         _lhsOmn =
             genc "r"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 713, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_LocB_Reg
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_LocB_TOS :: T_InsOp_LocB 
sem_InsOp_LocB_TOS  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_LocB 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 584, column 17)
         _lhsOmn =
             genc "t"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 712, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_LocB_TOS
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
-- InsOp_LocE --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
         self                 : SELF 
   alternatives:
      alternative Imm:
         visit 0:
            local self        : _
      alternative PC:
         visit 0:
            local self        : _
      alternative Reg:
         visit 0:
            local self        : _
      alternative SP:
         visit 0:
            local self        : _
-}
-- cata
sem_InsOp_LocE :: InsOp_LocE  ->
                  T_InsOp_LocE 
sem_InsOp_LocE (InsOp_LocE_Imm )  =
    (sem_InsOp_LocE_Imm )
sem_InsOp_LocE (InsOp_LocE_PC )  =
    (sem_InsOp_LocE_PC )
sem_InsOp_LocE (InsOp_LocE_Reg )  =
    (sem_InsOp_LocE_Reg )
sem_InsOp_LocE (InsOp_LocE_SP )  =
    (sem_InsOp_LocE_SP )
-- semantic domain
type T_InsOp_LocE  = ( Int,GenC,GenC,InsOp_LocE )
sem_InsOp_LocE_Imm :: T_InsOp_LocE 
sem_InsOp_LocE_Imm  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_LocE 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 580, column 17)
         _lhsOmn =
             genc "i"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 708, column 17)
         _lhsOenc =
             enc_InsOp_LocE_Imm
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_LocE_Imm
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_LocE_PC :: T_InsOp_LocE 
sem_InsOp_LocE_PC  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_LocE 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 581, column 17)
         _lhsOmn =
             genc "p"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 709, column 17)
         _lhsOenc =
             3
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_LocE_PC
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_LocE_Reg :: T_InsOp_LocE 
sem_InsOp_LocE_Reg  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_LocE 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 579, column 17)
         _lhsOmn =
             genc "r"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 707, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_LocE_Reg
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
sem_InsOp_LocE_SP :: T_InsOp_LocE 
sem_InsOp_LocE_SP  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         _lhsOself :: InsOp_LocE 
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 578, column 17)
         _lhsOmn =
             genc "s"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 706, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
         -- self rule
         _self =
             InsOp_LocE_SP
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOenc,_lhsOmn,_lhsOpp,_lhsOself))
-- InsOp_LocODst -----------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative Reg:
      alternative TOS:
-}
-- cata
sem_InsOp_LocODst :: InsOp_LocODst  ->
                     T_InsOp_LocODst 
sem_InsOp_LocODst (InsOp_LocODst_Reg )  =
    (sem_InsOp_LocODst_Reg )
sem_InsOp_LocODst (InsOp_LocODst_TOS )  =
    (sem_InsOp_LocODst_TOS )
-- semantic domain
type T_InsOp_LocODst  = ( Int,GenC,GenC)
sem_InsOp_LocODst_Reg :: T_InsOp_LocODst 
sem_InsOp_LocODst_Reg  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 589, column 17)
         _lhsOmn =
             genc "r"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 717, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_LocODst_TOS :: T_InsOp_LocODst 
sem_InsOp_LocODst_TOS  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 588, column 17)
         _lhsOmn =
             genc "t"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 716, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
-- InsOp_LocOSrc -----------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative Imm:
      alternative Reg:
      alternative SP:
      alternative TOS:
-}
-- cata
sem_InsOp_LocOSrc :: InsOp_LocOSrc  ->
                     T_InsOp_LocOSrc 
sem_InsOp_LocOSrc (InsOp_LocOSrc_Imm )  =
    (sem_InsOp_LocOSrc_Imm )
sem_InsOp_LocOSrc (InsOp_LocOSrc_Reg )  =
    (sem_InsOp_LocOSrc_Reg )
sem_InsOp_LocOSrc (InsOp_LocOSrc_SP )  =
    (sem_InsOp_LocOSrc_SP )
sem_InsOp_LocOSrc (InsOp_LocOSrc_TOS )  =
    (sem_InsOp_LocOSrc_TOS )
-- semantic domain
type T_InsOp_LocOSrc  = ( Int,GenC,GenC)
sem_InsOp_LocOSrc_Imm :: T_InsOp_LocOSrc 
sem_InsOp_LocOSrc_Imm  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 594, column 17)
         _lhsOmn =
             genc "i"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 722, column 17)
         _lhsOenc =
             2
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_LocOSrc_Reg :: T_InsOp_LocOSrc 
sem_InsOp_LocOSrc_Reg  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 593, column 17)
         _lhsOmn =
             genc "r"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 721, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_LocOSrc_SP :: T_InsOp_LocOSrc 
sem_InsOp_LocOSrc_SP  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 592, column 17)
         _lhsOmn =
             genc "s"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 720, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_LocOSrc_TOS :: T_InsOp_LocOSrc 
sem_InsOp_LocOSrc_TOS  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 595, column 17)
         _lhsOmn =
             genc "t"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 723, column 17)
         _lhsOenc =
             3
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
-- InsOp_TyOp --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         enc                  : Int
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative Add:
      alternative Mul:
      alternative Quot:
      alternative Sub:
-}
-- cata
sem_InsOp_TyOp :: InsOp_TyOp  ->
                  T_InsOp_TyOp 
sem_InsOp_TyOp (InsOp_TyOp_Add )  =
    (sem_InsOp_TyOp_Add )
sem_InsOp_TyOp (InsOp_TyOp_Mul )  =
    (sem_InsOp_TyOp_Mul )
sem_InsOp_TyOp (InsOp_TyOp_Quot )  =
    (sem_InsOp_TyOp_Quot )
sem_InsOp_TyOp (InsOp_TyOp_Sub )  =
    (sem_InsOp_TyOp_Sub )
-- semantic domain
type T_InsOp_TyOp  = ( Int,GenC,GenC)
sem_InsOp_TyOp_Add :: T_InsOp_TyOp 
sem_InsOp_TyOp_Add  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 598, column 17)
         _lhsOmn =
             genc "a"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 726, column 17)
         _lhsOenc =
             0
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_TyOp_Mul :: T_InsOp_TyOp 
sem_InsOp_TyOp_Mul  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 600, column 17)
         _lhsOmn =
             genc "m"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 728, column 17)
         _lhsOenc =
             2
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_TyOp_Quot :: T_InsOp_TyOp 
sem_InsOp_TyOp_Quot  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 601, column 17)
         _lhsOmn =
             genc "q"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 729, column 17)
         _lhsOenc =
             3
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
sem_InsOp_TyOp_Sub :: T_InsOp_TyOp 
sem_InsOp_TyOp_Sub  =
    (let _lhsOmn :: GenC
         _lhsOenc :: Int
         _lhsOpp :: GenC
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 599, column 17)
         _lhsOmn =
             genc "s"
         -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 727, column 17)
         _lhsOenc =
             1
         -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
         _lhsOpp =
             empty
     in  ( _lhsOenc,_lhsOmn,_lhsOpp))
-- Instr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         annL                 : AnnL
         impNmMp              : Map.Map HsName Int
         labelLocMp           : LabelLocMp
         linkChainResolvedMp  : LinkChainResolvedMp
         lkupLookupFunctionInfoInx : LookupFunctionInfoInx
         opts                 : EHCOpts
         ppNm                 : String -> GenC
      chained attributes:
         bytePoolMp           : BytePoolMp
         cLoc                 : CodeAbsLoc
         callInfoMp           : CallInfoMp
         functionInfoMp       : FunctionInfoMp
         gcStackInfoMp        : GCStackInfoMp
         linkChainResolvedIndInfoSet : LinkChainResolvedIndInfoSet
         stringConstMp        : StringConstMp
      synthesized attributes:
         bcode                : [Int]
         gathAnnL             : AnnL
         gathLabelLocMp       : LabelLocMp
         gathLinkChainKeyMp   : LinkChainKeyMp
         gencL                : [GenC]
         hasCode              : Bool
         isEntry              : Bool
         locRefs              : Seq.FastSeq LocRefLoc
         mbFunStart           : Maybe FunctionInfo
         mn                   : GenC
         pp                   : GenC
   alternatives:
      alternative AllocStore:
         child locSrc         : InsOp_LocB 
         child gcStackInfo    : {GCStackInfo}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup2       : _
            local linkChain   : _
            local _tup3       : _
            local bytePoolMp1 : _
            local gcStackInfoInx : _
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative Ann:
         child kind           : {AnnKind}
         child ann            : {String}
         child instr          : Instr 
         visit 0:
            local gencL       : _
      alternative Apply:
         child locSrc         : InsOp_LocB 
         child callInfo       : {CallInfo}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup4       : _
            local linkChain   : _
            local gcStackInfo : {GCStackInfo}
            local _tup5       : _
            local bytePoolMp1 : _
            local gcStackInfoInx : _
            local _tup6       : _
            local bytePoolMp2 : _
            local stringConstInx : _
            local _tup7       : _
            local ciInx       : _
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative Call:
         child locSrc         : InsOp_LocB 
         child callInfo       : {CallInfoCall}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup8       : _
            local linkChain   : _
            local gcStackInfo : {GCStackInfo}
            local _tup9       : _
            local bytePoolMp1 : _
            local gcStackInfoInx : _
            local _tup10      : _
            local bytePoolMp2 : _
            local stringConstInx : _
            local functionInfoModOff : _
            local functionInfoOff : _
            local _tup11      : _
            local ciInx       : _
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative CallC:
         child nArgSz         : InsOp_ImmSz 
         child nArg           : Imm 
         child linkChainEntryCallEnc : {LinkChainEntry}
         child callInfo       : {CallInfo}
         visit 0:
            local linkCLocCallEnc : {CodeAbsLoc}
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup12      : _
            local linkChainCallEnc : _
            local linkChain   : _
            local gcStackInfo : {GCStackInfo}
            local _tup13      : _
            local bytePoolMp1 : _
            local gcStackInfoInx : _
            local _tup14      : _
            local bytePoolMp2 : _
            local stringConstInx : _
            local _tup15      : _
            local ciInx       : _
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative CaseCall:
         child linkChainEntry : {LinkChainEntry}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local _tup16      : _
            local linkChain   : _
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative Eval:
         child locSrc         : InsOp_LocB 
         child callInfo       : {CallInfo}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup17      : _
            local linkChain   : _
            local gcStackInfo : {GCStackInfo}
            local _tup18      : _
            local bytePoolMp1 : _
            local gcStackInfoInx : _
            local _tup19      : _
            local bytePoolMp2 : _
            local stringConstInx : _
            local _tup20      : _
            local ciInx       : _
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative Fetch:
         child locSrc         : InsOp_LocB 
         visit 0:
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative FetchUpdate:
         visit 0:
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative FunStart:
         child functionInfo   : {FunctionInfo}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup21      : _
            local linkChain   : _
            local _tup22      : _
            local functionInfoInx : _
            local mn          : _
            local gencL       : _
      alternative Halt:
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative Label:
         child locRef         : {LocRef}
         visit 0:
            local labelCLoc   : {CodeAbsLoc}
            local gencL       : _
      alternative LabelRef:
         child locRef         : {LocRef}
         visit 0:
            local labelCLocAft : {CodeAbsLoc}
            local refOff      : _
            local mn          : _
            local gencL       : _
      alternative Ld:
         child ind            : InsOp_Deref 
         child locDst         : InsOp_LocB 
         child locSrc         : InsOp_LocE 
         child immSz          : InsOp_ImmSz 
         child imm            : Imm 
         visit 0:
            local mn          : _
            local indEnc      : _
            local locDstEnc   : _
            local locSrcEnc   : _
            local immSzEnc    : _
            local enc         : _
            local gencL       : _
      alternative LdGlobal:
         child locDst         : InsOp_LocB 
         child linkChainEntry : {LinkChainEntry}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local _tup23      : _
            local linkChain   : _
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative LdNodeTag:
         visit 0:
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative LdString:
         child locDst         : InsOp_LocB 
         child stringConst    : {StringConst}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup24      : _
            local linkChain   : _
            local _tup25      : _
            local stringConstInx : _
            local mn          : _
            local indEnc      : _
            local locDstEnc   : _
            local locSrcEnc   : _
            local immSzEnc    : _
            local enc         : _
            local gencL       : _
      alternative LinkChain:
         child linkChainEntry : {LinkChainEntry}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local _tup26      : _
            local linkChain   : _
            local mn          : _
            local gencL       : _
      alternative Meta:
         child meta           : Meta 
         visit 0:
            local gencL       : _
      alternative Op:
         child op             : InsOp_TyOp 
         child opndTy         : InsOp_DataOp 
         child locDst         : InsOp_LocODst 
         child ind            : InsOp_Deref 
         child locSrc         : InsOp_LocOSrc 
         child immSz          : InsOp_ImmSz 
         child imm            : Imm 
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative RetCall:
         child nArgMineSz     : InsOp_ImmSz 
         child nArgSurrSz     : InsOp_ImmSz 
         child nArgMine       : Imm 
         child nArgSurr       : Imm 
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative RetCase:
         child nResSz         : InsOp_ImmSz 
         child retOffSurrSz   : InsOp_ImmSz 
         child nRes           : Imm 
         child retOffSurr     : Imm 
         child linkChainEntry : {LinkChainEntry}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local _tup27      : _
            local linkChain   : _
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative St:
         child ind            : InsOp_DerefB 
         child locDst         : InsOp_LocE 
         child locSrc         : InsOp_LocB 
         child immSz          : InsOp_ImmSz 
         child imm            : Imm 
         visit 0:
            local mn          : _
            local enc         : _
            local gencL       : _
      alternative TagInt2Word:
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative TagWord2Word:
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative TailApply:
         child locSrc         : InsOp_LocB 
         child nArgMineSz     : InsOp_ImmSz 
         child nArgSurrSz     : InsOp_ImmSz 
         child nArgMine       : Imm 
         child nArgSurr       : Imm 
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local enc3        : _
            local gencL       : _
      alternative TailCall:
         child locSrc         : InsOp_LocB 
         child nArgMineSz     : InsOp_ImmSz 
         child nArgSurrSz     : InsOp_ImmSz 
         child nArgMine       : Imm 
         child nArgSurr       : Imm 
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local enc3        : _
            local gencL       : _
      alternative TailEval:
         child locSrc         : InsOp_LocB 
         child nArgSurrSz     : InsOp_ImmSz 
         child nArgSurr       : Imm 
         child callInfo       : {CallInfo}
         visit 0:
            local linkCLoc    : {CodeAbsLoc}
            local linkChainEntry : {LinkChainEntry}
            local _tup28      : _
            local linkChain   : _
            local gcStackInfo : {GCStackInfo}
            local _tup29      : _
            local bytePoolMp1 : _
            local gcStackInfoInx : _
            local _tup30      : _
            local bytePoolMp2 : _
            local stringConstInx : _
            local _tup31      : _
            local ciInx       : _
            local mn          : _
            local enc         : _
            local enc2        : _
            local enc3        : _
            local gencL       : _
      alternative UntagWord2Int:
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
      alternative UntagWord2Word:
         visit 0:
            local mn          : _
            local enc         : _
            local enc2        : _
            local gencL       : _
-}
-- cata
sem_Instr :: Instr  ->
             T_Instr 
sem_Instr (Instr_AllocStore _locSrc _gcStackInfo )  =
    (sem_Instr_AllocStore (sem_InsOp_LocB _locSrc ) _gcStackInfo )
sem_Instr (Instr_Ann _kind _ann _instr )  =
    (sem_Instr_Ann _kind _ann (sem_Instr _instr ) )
sem_Instr (Instr_Apply _locSrc _callInfo )  =
    (sem_Instr_Apply (sem_InsOp_LocB _locSrc ) _callInfo )
sem_Instr (Instr_Call _locSrc _callInfo )  =
    (sem_Instr_Call (sem_InsOp_LocB _locSrc ) _callInfo )
sem_Instr (Instr_CallC _nArgSz _nArg _linkChainEntryCallEnc _callInfo )  =
    (sem_Instr_CallC (sem_InsOp_ImmSz _nArgSz ) (sem_Imm _nArg ) _linkChainEntryCallEnc _callInfo )
sem_Instr (Instr_CaseCall _linkChainEntry )  =
    (sem_Instr_CaseCall _linkChainEntry )
sem_Instr (Instr_Eval _locSrc _callInfo )  =
    (sem_Instr_Eval (sem_InsOp_LocB _locSrc ) _callInfo )
sem_Instr (Instr_Fetch _locSrc )  =
    (sem_Instr_Fetch (sem_InsOp_LocB _locSrc ) )
sem_Instr (Instr_FetchUpdate )  =
    (sem_Instr_FetchUpdate )
sem_Instr (Instr_FunStart _functionInfo )  =
    (sem_Instr_FunStart _functionInfo )
sem_Instr (Instr_Halt )  =
    (sem_Instr_Halt )
sem_Instr (Instr_Label _locRef )  =
    (sem_Instr_Label _locRef )
sem_Instr (Instr_LabelRef _locRef )  =
    (sem_Instr_LabelRef _locRef )
sem_Instr (Instr_Ld _ind _locDst _locSrc _immSz _imm )  =
    (sem_Instr_Ld (sem_InsOp_Deref _ind ) (sem_InsOp_LocB _locDst ) (sem_InsOp_LocE _locSrc ) (sem_InsOp_ImmSz _immSz ) (sem_Imm _imm ) )
sem_Instr (Instr_LdGlobal _locDst _linkChainEntry )  =
    (sem_Instr_LdGlobal (sem_InsOp_LocB _locDst ) _linkChainEntry )
sem_Instr (Instr_LdNodeTag )  =
    (sem_Instr_LdNodeTag )
sem_Instr (Instr_LdString _locDst _stringConst )  =
    (sem_Instr_LdString (sem_InsOp_LocB _locDst ) _stringConst )
sem_Instr (Instr_LinkChain _linkChainEntry )  =
    (sem_Instr_LinkChain _linkChainEntry )
sem_Instr (Instr_Meta _meta )  =
    (sem_Instr_Meta (sem_Meta _meta ) )
sem_Instr (Instr_Op _op _opndTy _locDst _ind _locSrc _immSz _imm )  =
    (sem_Instr_Op (sem_InsOp_TyOp _op ) (sem_InsOp_DataOp _opndTy ) (sem_InsOp_LocODst _locDst ) (sem_InsOp_Deref _ind ) (sem_InsOp_LocOSrc _locSrc ) (sem_InsOp_ImmSz _immSz ) (sem_Imm _imm ) )
sem_Instr (Instr_RetCall _nArgMineSz _nArgSurrSz _nArgMine _nArgSurr )  =
    (sem_Instr_RetCall (sem_InsOp_ImmSz _nArgMineSz ) (sem_InsOp_ImmSz _nArgSurrSz ) (sem_Imm _nArgMine ) (sem_Imm _nArgSurr ) )
sem_Instr (Instr_RetCase _nResSz _retOffSurrSz _nRes _retOffSurr _linkChainEntry )  =
    (sem_Instr_RetCase (sem_InsOp_ImmSz _nResSz ) (sem_InsOp_ImmSz _retOffSurrSz ) (sem_Imm _nRes ) (sem_Imm _retOffSurr ) _linkChainEntry )
sem_Instr (Instr_St _ind _locDst _locSrc _immSz _imm )  =
    (sem_Instr_St (sem_InsOp_DerefB _ind ) (sem_InsOp_LocE _locDst ) (sem_InsOp_LocB _locSrc ) (sem_InsOp_ImmSz _immSz ) (sem_Imm _imm ) )
sem_Instr (Instr_TagInt2Word )  =
    (sem_Instr_TagInt2Word )
sem_Instr (Instr_TagWord2Word )  =
    (sem_Instr_TagWord2Word )
sem_Instr (Instr_TailApply _locSrc _nArgMineSz _nArgSurrSz _nArgMine _nArgSurr )  =
    (sem_Instr_TailApply (sem_InsOp_LocB _locSrc ) (sem_InsOp_ImmSz _nArgMineSz ) (sem_InsOp_ImmSz _nArgSurrSz ) (sem_Imm _nArgMine ) (sem_Imm _nArgSurr ) )
sem_Instr (Instr_TailCall _locSrc _nArgMineSz _nArgSurrSz _nArgMine _nArgSurr )  =
    (sem_Instr_TailCall (sem_InsOp_LocB _locSrc ) (sem_InsOp_ImmSz _nArgMineSz ) (sem_InsOp_ImmSz _nArgSurrSz ) (sem_Imm _nArgMine ) (sem_Imm _nArgSurr ) )
sem_Instr (Instr_TailEval _locSrc _nArgSurrSz _nArgSurr _callInfo )  =
    (sem_Instr_TailEval (sem_InsOp_LocB _locSrc ) (sem_InsOp_ImmSz _nArgSurrSz ) (sem_Imm _nArgSurr ) _callInfo )
sem_Instr (Instr_UntagWord2Int )  =
    (sem_Instr_UntagWord2Int )
sem_Instr (Instr_UntagWord2Word )  =
    (sem_Instr_UntagWord2Word )
-- semantic domain
type T_Instr  = AnnL ->
                BytePoolMp ->
                CodeAbsLoc ->
                CallInfoMp ->
                FunctionInfoMp ->
                GCStackInfoMp ->
                (Map.Map HsName Int) ->
                LabelLocMp ->
                LinkChainResolvedIndInfoSet ->
                LinkChainResolvedMp ->
                LookupFunctionInfoInx ->
                EHCOpts ->
                (String -> GenC) ->
                StringConstMp ->
                ( ([Int]),BytePoolMp,CodeAbsLoc,CallInfoMp,FunctionInfoMp,AnnL,LabelLocMp,LinkChainKeyMp,GCStackInfoMp,([GenC]),Bool,Bool,LinkChainResolvedIndInfoSet,(Seq.FastSeq LocRefLoc),(Maybe FunctionInfo),GenC,GenC,StringConstMp)
sem_Instr_AllocStore :: T_InsOp_LocB  ->
                        GCStackInfo ->
                        T_Instr 
sem_Instr_AllocStore locSrc_ gcStackInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsObytePoolMp :: BytePoolMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOstringConstMp :: StringConstMp
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 206, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_GCInfo (LinkChainId_Loc _linkCLoc)) (maybe 0 (+1) _gcStackInfoInx)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup2 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 337, column 17)
              _lhsObytePoolMp =
                  _bytePoolMp1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              __tup3 =
                  gcStackInfoMpAdd gcStackInfo_ _lhsIgcStackInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_lhsOgcStackInfoMp,_,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_bytePoolMp1,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_,_gcStackInfoInx) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 541, column 17)
              _mn =
                  "allocstore"    >|< _locSrcImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 672, column 17)
              _enc =
                  0xEC .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 807, column 21)
              _gencL =
                  [vmCall "AllocStoreTos" [gencCast "Word" $ gencAddrArrayAt (_lhsIppNm strNmGcStackInfos) $ lcentryInfo _linkChainEntry]]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1150, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Ann :: AnnKind ->
                 String ->
                 T_Instr  ->
                 T_Instr 
sem_Instr_Ann kind_ ann_ instr_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOgathAnnL :: AnnL
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _instrOannL :: AnnL
              _instrObytePoolMp :: BytePoolMp
              _instrOcLoc :: CodeAbsLoc
              _instrOcallInfoMp :: CallInfoMp
              _instrOfunctionInfoMp :: FunctionInfoMp
              _instrOgcStackInfoMp :: GCStackInfoMp
              _instrOimpNmMp :: (Map.Map HsName Int)
              _instrOlabelLocMp :: LabelLocMp
              _instrOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _instrOlinkChainResolvedMp :: LinkChainResolvedMp
              _instrOlkupLookupFunctionInfoInx :: LookupFunctionInfoInx
              _instrOopts :: EHCOpts
              _instrOppNm :: (String -> GenC)
              _instrOstringConstMp :: StringConstMp
              _instrIbcode :: ([Int])
              _instrIbytePoolMp :: BytePoolMp
              _instrIcLoc :: CodeAbsLoc
              _instrIcallInfoMp :: CallInfoMp
              _instrIfunctionInfoMp :: FunctionInfoMp
              _instrIgathAnnL :: AnnL
              _instrIgathLabelLocMp :: LabelLocMp
              _instrIgathLinkChainKeyMp :: LinkChainKeyMp
              _instrIgcStackInfoMp :: GCStackInfoMp
              _instrIgencL :: ([GenC])
              _instrIhasCode :: Bool
              _instrIisEntry :: Bool
              _instrIlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _instrIlocRefs :: (Seq.FastSeq LocRefLoc)
              _instrImbFunStart :: (Maybe FunctionInfo)
              _instrImn :: GenC
              _instrIpp :: GenC
              _instrIstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 525, column 17)
              _lhsOgathAnnL =
                  (kind_,genc ann_) : _instrIgathAnnL
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 820, column 21)
              _gencL =
                  []
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1161, column 17)
              _lhsOpp =
                  _instrIpp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1219, column 17)
              _lhsObcode =
                  _instrIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  _instrIgathLabelLocMp
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  _instrIgathLinkChainKeyMp
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  _instrIlocRefs
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _instrImn
              -- copy rule (up)
              _lhsObytePoolMp =
                  _instrIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _instrIcLoc
              -- copy rule (up)
              _lhsOcallInfoMp =
                  _instrIcallInfoMp
              -- copy rule (up)
              _lhsOfunctionInfoMp =
                  _instrIfunctionInfoMp
              -- copy rule (up)
              _lhsOgcStackInfoMp =
                  _instrIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (up)
              _lhsOlinkChainResolvedIndInfoSet =
                  _instrIlinkChainResolvedIndInfoSet
              -- copy rule (up)
              _lhsOstringConstMp =
                  _instrIstringConstMp
              -- copy rule (down)
              _instrOannL =
                  _lhsIannL
              -- copy rule (down)
              _instrObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (down)
              _instrOcLoc =
                  _lhsIcLoc
              -- copy rule (down)
              _instrOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (down)
              _instrOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (down)
              _instrOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (down)
              _instrOimpNmMp =
                  _lhsIimpNmMp
              -- copy rule (down)
              _instrOlabelLocMp =
                  _lhsIlabelLocMp
              -- copy rule (down)
              _instrOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (down)
              _instrOlinkChainResolvedMp =
                  _lhsIlinkChainResolvedMp
              -- copy rule (down)
              _instrOlkupLookupFunctionInfoInx =
                  _lhsIlkupLookupFunctionInfoInx
              -- copy rule (down)
              _instrOopts =
                  _lhsIopts
              -- copy rule (down)
              _instrOppNm =
                  _lhsIppNm
              -- copy rule (down)
              _instrOstringConstMp =
                  _lhsIstringConstMp
              ( _instrIbcode,_instrIbytePoolMp,_instrIcLoc,_instrIcallInfoMp,_instrIfunctionInfoMp,_instrIgathAnnL,_instrIgathLabelLocMp,_instrIgathLinkChainKeyMp,_instrIgcStackInfoMp,_instrIgencL,_instrIhasCode,_instrIisEntry,_instrIlinkChainResolvedIndInfoSet,_instrIlocRefs,_instrImbFunStart,_instrImn,_instrIpp,_instrIstringConstMp) =
                  instr_ _instrOannL _instrObytePoolMp _instrOcLoc _instrOcallInfoMp _instrOfunctionInfoMp _instrOgcStackInfoMp _instrOimpNmMp _instrOlabelLocMp _instrOlinkChainResolvedIndInfoSet _instrOlinkChainResolvedMp _instrOlkupLookupFunctionInfoInx _instrOopts _instrOppNm _instrOstringConstMp 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Apply :: T_InsOp_LocB  ->
                   CallInfo ->
                   T_Instr 
sem_Instr_Apply locSrc_ callInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsObytePoolMp :: BytePoolMp
              _gcStackInfo :: GCStackInfo
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOstringConstMp :: StringConstMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgencL :: ([GenC])
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 204, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_CallInfo (LinkChainId_Loc _linkCLoc)) _ciInx
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup4 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 339, column 17)
              _lhsObytePoolMp =
                  _bytePoolMp2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 360, column 17)
              _gcStackInfo =
                  ciGCStackInfo callInfo_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              __tup5 =
                  gcStackInfoMpAdd _gcStackInfo _lhsIgcStackInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_lhsOgcStackInfoMp,_,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_bytePoolMp1,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_,_gcStackInfoInx) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              __tup6 =
                  case ciMbKey callInfo_ of
                    Just s -> stringConstMpAdd s _lhsIstringConstMp _bytePoolMp1
                    _      -> (_lhsIstringConstMp,_bytePoolMp1,Nothing)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_lhsOstringConstMp,_,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_bytePoolMp2,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_,_stringConstInx) =
                  __tup6
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              __tup7 =
                  ciMpAdd (callInfo_ { ciGCStackInfo = _gcStackInfoInx
                                     , ciMbKey       = fmap (\x -> (x,Nothing)) _stringConstInx
                                     }) _lhsIcallInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_lhsOcallInfoMp,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_,_ciInx) =
                  __tup7
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 554, column 17)
              _mn =
                  "apply"         >|< _locSrcImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 660, column 17)
              _enc =
                  0xE2 .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 796, column 21)
              _gencL =
                  let p = case _locSrcIself of
                            InsOp_LocB_TOS -> "ApplyTos"
                            _              -> "Apply_NotImplemented"
                  in  [vmCall p []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1150, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Call :: T_InsOp_LocB  ->
                  CallInfoCall ->
                  T_Instr 
sem_Instr_Call locSrc_ callInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsObytePoolMp :: BytePoolMp
              _gcStackInfo :: GCStackInfo
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOstringConstMp :: StringConstMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgencL :: ([GenC])
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 204, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_CallInfo (LinkChainId_Loc _linkCLoc)) _ciInx
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup8 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup8
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup8
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 339, column 17)
              _lhsObytePoolMp =
                  _bytePoolMp2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 360, column 17)
              _gcStackInfo =
                  ciGCStackInfo callInfo_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              __tup9 =
                  gcStackInfoMpAdd _gcStackInfo _lhsIgcStackInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_lhsOgcStackInfoMp,_,_) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_bytePoolMp1,_) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_,_gcStackInfoInx) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 392, column 17)
              __tup10 =
                  let dflt@(x,y,z,_,_)  = (_lhsIstringConstMp,_bytePoolMp1,Nothing,noInx,noInx)
                      noInx = (-1)::Int
                  in  case ciMbKey callInfo_ of
                        Just (ms,nm) -> ( a, b, c, d, e )
                                     where (a,b,c) = (x,y,z)
                                           d = Map.findWithDefault noInx (panicJust ("ToC.Instr.Call: " ++ show nm) $ hsnQualifier nm) _lhsIimpNmMp
                                           e = maybe noInx id $ _lhsIlkupLookupFunctionInfoInx nm
                        _            -> dflt
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 392, column 17)
              (_lhsOstringConstMp,_,_,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 392, column 17)
              (_,_bytePoolMp2,_,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 392, column 17)
              (_,_,_stringConstInx,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 392, column 17)
              (_,_,_,_functionInfoModOff,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 392, column 17)
              (_,_,_,_,_functionInfoOff) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 480, column 17)
              __tup11 =
                  ciMpAdd (callInfo_ { ciGCStackInfo = _gcStackInfoInx
                                     , ciMbKey       = Just (0,Just (_functionInfoModOff,_functionInfoOff))
                                     }) _lhsIcallInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 480, column 17)
              (_lhsOcallInfoMp,_) =
                  __tup11
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 480, column 17)
              (_,_ciInx) =
                  __tup11
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 543, column 17)
              _mn =
                  "call"          >|< _locSrcImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 663, column 17)
              _enc =
                  0xF0 .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1150, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_CallC :: T_InsOp_ImmSz  ->
                   T_Imm  ->
                   LinkChainEntry ->
                   CallInfo ->
                   T_Instr 
sem_Instr_CallC nArgSz_ nArg_ linkChainEntryCallEnc_ callInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLocCallEnc :: CodeAbsLoc
              _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _nArgOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOgathLabelLocMp :: LabelLocMp
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsObytePoolMp :: BytePoolMp
              _gcStackInfo :: GCStackInfo
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOstringConstMp :: StringConstMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOhasCode :: Bool
              _nArgOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgencL :: ([GenC])
              _nArgSzIenc :: Int
              _nArgSzImn :: GenC
              _nArgSzIpp :: GenC
              _nArgSzIself :: InsOp_ImmSz 
              _nArgIbcode :: ([Int])
              _nArgIcLoc :: CodeAbsLoc
              _nArgIgenc :: GenC
              _nArgImn :: GenC
              _nArgIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 136, column 17)
              _linkCLocCallEnc =
                  _nArgIcLoc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 139, column 17)
              _linkCLoc =
                  _linkCLocCallEnc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 155, column 17)
              _nArgOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 190, column 17)
              _lhsOgathLabelLocMp =
                  mkLabelLocMp _linkCLocCallEnc linkChainEntryCallEnc_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 204, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_CallInfo (LinkChainId_Loc _linkCLoc)) _ciInx
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 227, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry `Map.union` mkLinkChainKeyMp linkChainEntryCallEnc_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 252, column 17)
              __tup12 =
                  let (e1,s1) = updLinkChainResolvedIndInfoSet linkChainEntryCallEnc_ _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
                      (e2,s2) = updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp s1
                  in  (e1,e2,s2)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 252, column 17)
              (_linkChainCallEnc,_,_) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 252, column 17)
              (_,_linkChain,_) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 252, column 17)
              (_,_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 340, column 17)
              _lhsObytePoolMp =
                  _bytePoolMp2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 360, column 17)
              _gcStackInfo =
                  ciGCStackInfo callInfo_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              __tup13 =
                  gcStackInfoMpAdd _gcStackInfo _lhsIgcStackInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_lhsOgcStackInfoMp,_,_) =
                  __tup13
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_bytePoolMp1,_) =
                  __tup13
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_,_gcStackInfoInx) =
                  __tup13
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              __tup14 =
                  case ciMbKey callInfo_ of
                    Just s -> stringConstMpAdd s _lhsIstringConstMp _bytePoolMp1
                    _      -> (_lhsIstringConstMp,_bytePoolMp1,Nothing)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_lhsOstringConstMp,_,_) =
                  __tup14
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_bytePoolMp2,_) =
                  __tup14
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_,_stringConstInx) =
                  __tup14
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              __tup15 =
                  ciMpAdd (callInfo_ { ciGCStackInfo = _gcStackInfoInx
                                     , ciMbKey       = fmap (\x -> (x,Nothing)) _stringConstInx
                                     }) _lhsIcallInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_lhsOcallInfoMp,_) =
                  __tup15
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_,_ciInx) =
                  __tup15
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 550, column 17)
              _mn =
                  "callc"         >|< _nArgSzImn >#< _nArgImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 667, column 17)
              _enc =
                  0xF7
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 686, column 17)
              _enc2 =
                  (_nArgSzIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 761, column 17)
              _nArgOimmSz =
                  _nArgSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1158, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,_nArgIpp, ppLinkChain _linkChainCallEnc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1216, column 17)
              _lhsObcode =
                  [_enc, _enc2] ++ _nArgIbcode ++ bcodeLinkChain _linkChainCallEnc ++ bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              ( _nArgSzIenc,_nArgSzImn,_nArgSzIpp,_nArgSzIself) =
                  nArgSz_ 
              ( _nArgIbcode,_nArgIcLoc,_nArgIgenc,_nArgImn,_nArgIpp) =
                  nArg_ _nArgOcLoc _nArgOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_CaseCall :: LinkChainEntry ->
                      T_Instr 
sem_Instr_CaseCall linkChainEntry_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 188, column 17)
              _lhsOgathLabelLocMp =
                  mkLabelLocMp _linkCLoc linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup16 =
                  updLinkChainResolvedIndInfoSet linkChainEntry_ _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 547, column 17)
              _mn =
                  genc "casecall"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 666, column 17)
              _enc =
                  0xF6
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 808, column 21)
              _gencL =
                  [vmCall "CaseBegin" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1150, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Eval :: T_InsOp_LocB  ->
                  CallInfo ->
                  T_Instr 
sem_Instr_Eval locSrc_ callInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsObytePoolMp :: BytePoolMp
              _gcStackInfo :: GCStackInfo
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOstringConstMp :: StringConstMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgencL :: ([GenC])
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 204, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_CallInfo (LinkChainId_Loc _linkCLoc)) _ciInx
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup17 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup17
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup17
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 339, column 17)
              _lhsObytePoolMp =
                  _bytePoolMp2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 360, column 17)
              _gcStackInfo =
                  ciGCStackInfo callInfo_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              __tup18 =
                  gcStackInfoMpAdd _gcStackInfo _lhsIgcStackInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_lhsOgcStackInfoMp,_,_) =
                  __tup18
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_bytePoolMp1,_) =
                  __tup18
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_,_gcStackInfoInx) =
                  __tup18
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              __tup19 =
                  case ciMbKey callInfo_ of
                    Just s -> stringConstMpAdd s _lhsIstringConstMp _bytePoolMp1
                    _      -> (_lhsIstringConstMp,_bytePoolMp1,Nothing)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_lhsOstringConstMp,_,_) =
                  __tup19
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_bytePoolMp2,_) =
                  __tup19
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_,_stringConstInx) =
                  __tup19
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              __tup20 =
                  ciMpAdd (callInfo_ { ciGCStackInfo = _gcStackInfoInx
                                     , ciMbKey       = fmap (\x -> (x,Nothing)) _stringConstInx
                                     }) _lhsIcallInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_lhsOcallInfoMp,_) =
                  __tup20
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_,_ciInx) =
                  __tup20
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 553, column 17)
              _mn =
                  "eval"          >|< _locSrcImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 659, column 17)
              _enc =
                  0xE0 .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 792, column 21)
              _gencL =
                  let p = case _locSrcIself of
                            InsOp_LocB_TOS -> "EvalTos"
                            _              -> "Eval_NotImplemented"
                  in  [vmCall p []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1150, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Fetch :: T_InsOp_LocB  ->
                   T_Instr 
sem_Instr_Fetch locSrc_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 160, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 542, column 17)
              _mn =
                  "fetch"         >|< _locSrcImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 673, column 17)
              _enc =
                  0xEE .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 801, column 21)
              _gencL =
                  [vmCall "FetchTos" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1146, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1206, column 17)
              _lhsObcode =
                  [_enc]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_FetchUpdate :: T_Instr 
sem_Instr_FetchUpdate  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 154, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 548, column 17)
              _mn =
                  genc "fetchupd"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 675, column 17)
              _enc =
                  0xF9
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 802, column 21)
              _gencL =
                  [vmCall "FetchUpdateTos" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1157, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1215, column 17)
              _lhsObcode =
                  [_enc]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_FunStart :: FunctionInfo ->
                      T_Instr 
sem_Instr_FunStart functionInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsObytePoolMp :: BytePoolMp
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 127, column 17)
              _linkCLoc =
                  _lhsIcLoc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 210, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_FunctionInfo (LinkChainId_Loc _linkCLoc)) (maybe 0 id _functionInfoInx)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup21 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup21
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup21
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 436, column 17)
              __tup22 =
                  functionInfoMpAdd functionInfo_ _lhsIfunctionInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 436, column 17)
              (_lhsOfunctionInfoMp,_,_) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 436, column 17)
              (_,_lhsObytePoolMp,_) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 436, column 17)
              (_,_,_functionInfoInx) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 568, column 17)
              _mn =
                  genc ".fun"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 829, column 17)
              _lhsOmbFunStart =
                  Just functionInfo_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1166, column 17)
              _lhsOpp =
                  ppCommas [ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1224, column 17)
              _lhsObcode =
                  bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Halt :: T_Instr 
sem_Instr_Halt  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 159, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 555, column 17)
              _mn =
                  genc "halt"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 678, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 689, column 17)
              _enc2 =
                  0xFF
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1163, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1221, column 17)
              _lhsObcode =
                  [_enc, _enc2]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Label :: LocRef ->
                   T_Instr 
sem_Instr_Label locRef_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _labelCLoc :: CodeAbsLoc
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOisEntry :: Bool
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOhasCode :: Bool
              _lhsOgathAnnL :: AnnL
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsObcode :: ([Int])
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOmn :: GenC
              _lhsOpp :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 122, column 17)
              _labelCLoc =
                  _lhsIcLoc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 169, column 17)
              _lhsOlocRefs =
                  Seq.singleton (locRef_,_labelCLoc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 174, column 17)
              _lhsOisEntry =
                  case locRef_ of {LocRef_CodeEntry _ -> True; _ -> False}
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 185, column 17)
              _lhsOgathLabelLocMp =
                  if lrefIsLabel locRef_ then Map.singleton (lrefId locRef_) _labelCLoc else Map.empty
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 507, column 17)
              _lhsOhasCode =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 523, column 17)
              _lhsOgathAnnL =
                  [(AnnLabel,genc locRef_)]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 809, column 21)
              _gencL =
                  case locRef_ of
                      LocRef_EndSwitch _ -> [vmCall "CaseEnd" []]
                      LocRef_CaseArm _ i -> [vmCall "CaseArm" [genc i]]
                      r                  -> [gencLabel r]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1199, column 35)
              _lhsObcode =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcLoc =
                  _lhsIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_LabelRef :: LocRef ->
                      T_Instr 
sem_Instr_LabelRef locRef_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _labelCLocAft :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOgathAnnL :: AnnL
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 124, column 17)
              _labelCLocAft =
                  _lhsIcLoc + Cfg.gbLabelOffsetSize
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 148, column 17)
              _lhsOcLoc =
                  _labelCLocAft
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 199, column 17)
              _refOff =
                  (panicJust "Instr.LabelRef.refOff" $ Map.lookup (lrefId locRef_) _lhsIlabelLocMp)
                      - _labelCLocAft
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 524, column 17)
              _lhsOgathAnnL =
                  [(AnnLabelRef,genc locRef_)]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 566, column 17)
              _mn =
                  ".ref"          >#< lrefId locRef_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1164, column 17)
              _lhsOpp =
                  ppCommas [ppWord _refOff]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1222, column 17)
              _lhsObcode =
                  bcodeWord _refOff
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Ld :: T_InsOp_Deref  ->
                T_InsOp_LocB  ->
                T_InsOp_LocE  ->
                T_InsOp_ImmSz  ->
                T_Imm  ->
                T_Instr 
sem_Instr_Ld ind_ locDst_ locSrc_ immSz_ imm_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _immOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _immOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _indIenc :: Int
              _indImn :: GenC
              _indIpp :: GenC
              _indIself :: InsOp_Deref 
              _locDstIenc :: Int
              _locDstImn :: GenC
              _locDstIpp :: GenC
              _locDstIself :: InsOp_LocB 
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocE 
              _immSzIenc :: Int
              _immSzImn :: GenC
              _immSzIpp :: GenC
              _immSzIself :: InsOp_ImmSz 
              _immIbcode :: ([Int])
              _immIcLoc :: CodeAbsLoc
              _immIgenc :: GenC
              _immImn :: GenC
              _immIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 144, column 17)
              _immOcLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 539, column 17)
              _mn =
                  "l"             >|< _indImn >|< _locDstImn >|< _locSrcImn >|< _immSzImn >#< _immImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 645, column 17)
              _indEnc =
                  _indIenc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 645, column 17)
              _locDstEnc =
                  _locDstIenc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 645, column 17)
              _locSrcEnc =
                  _locSrcIenc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 645, column 17)
              _immSzEnc =
                  _immSzIenc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 653, column 17)
              _enc =
                  0x00 .|. (_indEnc `shiftL` 5) .|. (_locDstEnc `shiftL` 4) .|. (_locSrcEnc `shiftL` 2) .|. (_immSzEnc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 755, column 17)
              _immOimmSz =
                  _immSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 783, column 21)
              _gencL =
                  let p = case (_indIself, _locDstIself, _locSrcIself) of
                            (InsOp_Deref_Zero, InsOp_LocB_TOS, InsOp_LocE_Imm) -> "Push"
                            (InsOp_Deref_Int , InsOp_LocB_TOS, InsOp_LocE_Imm) -> "PushInt"
                            (InsOp_Deref_One , InsOp_LocB_TOS, InsOp_LocE_SP ) -> "PushTos"
                            (InsOp_Deref_Two , InsOp_LocB_TOS, InsOp_LocE_SP ) -> "PushTosX"
                            (InsOp_Deref_One , InsOp_LocB_TOS, InsOp_LocE_Reg) -> "PushRrX"
                            (InsOp_Deref_One , InsOp_LocB_Reg, InsOp_LocE_SP ) -> "LdRrTos"
                            _                                                  -> "Push_NotImplemented"
                  in  [vmCall (p >|< insOpImmSzEnc2NrBits _immSzIenc) [_immIgenc]]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1145, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,_immIpp]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1205, column 17)
              _lhsObcode =
                  _enc : _immIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _immIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              ( _indIenc,_indImn,_indIpp,_indIself) =
                  ind_ 
              ( _locDstIenc,_locDstImn,_locDstIpp,_locDstIself) =
                  locDst_ 
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
              ( _immSzIenc,_immSzImn,_immSzIpp,_immSzIself) =
                  immSz_ 
              ( _immIbcode,_immIcLoc,_immIgenc,_immImn,_immIpp) =
                  imm_ _immOcLoc _immOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_LdGlobal :: T_InsOp_LocB  ->
                      LinkChainEntry ->
                      T_Instr 
sem_Instr_LdGlobal locDst_ linkChainEntry_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOstringConstMp :: StringConstMp
              _locDstIenc :: Int
              _locDstImn :: GenC
              _locDstIpp :: GenC
              _locDstIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 188, column 17)
              _lhsOgathLabelLocMp =
                  mkLabelLocMp _linkCLoc linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup23 =
                  updLinkChainResolvedIndInfoSet linkChainEntry_ _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup23
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup23
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 563, column 17)
              _mn =
                  "ldg"           >|< _locDstImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 661, column 17)
              _enc =
                  0xE6 .|. (_locDstIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1150, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              ( _locDstIenc,_locDstImn,_locDstIpp,_locDstIself) =
                  locDst_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_LdNodeTag :: T_Instr 
sem_Instr_LdNodeTag  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 154, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 549, column 17)
              _mn =
                  genc "lnt"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 676, column 17)
              _enc =
                  0xFC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 800, column 21)
              _gencL =
                  [vmCall "PushNdTg" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1157, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1215, column 17)
              _lhsObcode =
                  [_enc]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_LdString :: T_InsOp_LocB  ->
                      StringConst ->
                      T_Instr 
sem_Instr_LdString locDst_ stringConst_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _lhsObytePoolMp :: BytePoolMp
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _locDstIenc :: Int
              _locDstImn :: GenC
              _locDstIpp :: GenC
              _locDstIself :: InsOp_LocB 
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 130, column 17)
              _linkCLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 208, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_StringConst (LinkChainId_Loc _linkCLoc)) (maybe 0 id _stringConstInx)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup24 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup24
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup24
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 384, column 17)
              __tup25 =
                  stringConstMpAdd stringConst_ _lhsIstringConstMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 384, column 17)
              (_lhsOstringConstMp,_,_) =
                  __tup25
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 384, column 17)
              (_,_lhsObytePoolMp,_) =
                  __tup25
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 384, column 17)
              (_,_,_stringConstInx) =
                  __tup25
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 564, column 17)
              _mn =
                  "ldstr"         >|< _locDstImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 649, column 17)
              _indEnc =
                  enc_InsOp_Deref_Zero
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 649, column 17)
              _locDstEnc =
                  _locDstIenc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 649, column 17)
              _locSrcEnc =
                  enc_InsOp_LocE_Imm
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 649, column 17)
              _immSzEnc =
                  insOpImmSz2Enc Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 653, column 17)
              _enc =
                  0x00 .|. (_indEnc `shiftL` 5) .|. (_locDstEnc `shiftL` 4) .|. (_locSrcEnc `shiftL` 2) .|. (_immSzEnc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1148, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1208, column 17)
              _lhsObcode =
                  _enc : bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              ( _locDstIenc,_locDstImn,_locDstIpp,_locDstIself) =
                  locDst_ 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_LinkChain :: LinkChainEntry ->
                       T_Instr 
sem_Instr_LinkChain linkChainEntry_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 127, column 17)
              _linkCLoc =
                  _lhsIcLoc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 188, column 17)
              _lhsOgathLabelLocMp =
                  mkLabelLocMp _linkCLoc linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup26 =
                  updLinkChainResolvedIndInfoSet linkChainEntry_ _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup26
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup26
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 567, column 17)
              _mn =
                  genc ".lnk"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1166, column 17)
              _lhsOpp =
                  ppCommas [ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1224, column 17)
              _lhsObcode =
                  bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Meta :: T_Meta  ->
                  T_Instr 
sem_Instr_Meta meta_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOpp :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _metaOannL :: AnnL
              _metaObytePoolMp :: BytePoolMp
              _metaOcallInfoMp :: CallInfoMp
              _metaOfunctionInfoMp :: FunctionInfoMp
              _metaOgcStackInfoMp :: GCStackInfoMp
              _metaOimpNmMp :: (Map.Map HsName Int)
              _metaOlabelLocMp :: LabelLocMp
              _metaOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _metaOlinkChainResolvedMp :: LinkChainResolvedMp
              _metaOlkupLookupFunctionInfoInx :: LookupFunctionInfoInx
              _metaOopts :: EHCOpts
              _metaOppNm :: (String -> GenC)
              _metaOstringConstMp :: StringConstMp
              _metaIbytePoolMp :: BytePoolMp
              _metaIcallInfoMp :: CallInfoMp
              _metaIenc :: Int
              _metaIfunctionInfoMp :: FunctionInfoMp
              _metaIgathAnnL :: AnnL
              _metaIgathLabelLocMp :: LabelLocMp
              _metaIgathLinkChainKeyMp :: LinkChainKeyMp
              _metaIgcStackInfoMp :: GCStackInfoMp
              _metaIlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _metaIlocRefs :: (Seq.FastSeq LocRefLoc)
              _metaIpp :: GenC
              _metaIstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 507, column 17)
              _lhsOhasCode =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 820, column 21)
              _gencL =
                  []
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1199, column 35)
              _lhsObcode =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  _metaIgathAnnL
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  _metaIgathLabelLocMp
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  _metaIgathLinkChainKeyMp
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  _metaIlocRefs
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
              _lhsOpp =
                  _metaIpp
              -- copy rule (up)
              _lhsObytePoolMp =
                  _metaIbytePoolMp
              -- copy rule (chain)
              _lhsOcLoc =
                  _lhsIcLoc
              -- copy rule (up)
              _lhsOcallInfoMp =
                  _metaIcallInfoMp
              -- copy rule (up)
              _lhsOfunctionInfoMp =
                  _metaIfunctionInfoMp
              -- copy rule (up)
              _lhsOgcStackInfoMp =
                  _metaIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (up)
              _lhsOlinkChainResolvedIndInfoSet =
                  _metaIlinkChainResolvedIndInfoSet
              -- copy rule (up)
              _lhsOstringConstMp =
                  _metaIstringConstMp
              -- copy rule (down)
              _metaOannL =
                  _lhsIannL
              -- copy rule (down)
              _metaObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (down)
              _metaOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (down)
              _metaOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (down)
              _metaOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (down)
              _metaOimpNmMp =
                  _lhsIimpNmMp
              -- copy rule (down)
              _metaOlabelLocMp =
                  _lhsIlabelLocMp
              -- copy rule (down)
              _metaOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (down)
              _metaOlinkChainResolvedMp =
                  _lhsIlinkChainResolvedMp
              -- copy rule (down)
              _metaOlkupLookupFunctionInfoInx =
                  _lhsIlkupLookupFunctionInfoInx
              -- copy rule (down)
              _metaOopts =
                  _lhsIopts
              -- copy rule (down)
              _metaOppNm =
                  _lhsIppNm
              -- copy rule (down)
              _metaOstringConstMp =
                  _lhsIstringConstMp
              ( _metaIbytePoolMp,_metaIcallInfoMp,_metaIenc,_metaIfunctionInfoMp,_metaIgathAnnL,_metaIgathLabelLocMp,_metaIgathLinkChainKeyMp,_metaIgcStackInfoMp,_metaIlinkChainResolvedIndInfoSet,_metaIlocRefs,_metaIpp,_metaIstringConstMp) =
                  meta_ _metaOannL _metaObytePoolMp _metaOcallInfoMp _metaOfunctionInfoMp _metaOgcStackInfoMp _metaOimpNmMp _metaOlabelLocMp _metaOlinkChainResolvedIndInfoSet _metaOlinkChainResolvedMp _metaOlkupLookupFunctionInfoInx _metaOopts _metaOppNm _metaOstringConstMp 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_Op :: T_InsOp_TyOp  ->
                T_InsOp_DataOp  ->
                T_InsOp_LocODst  ->
                T_InsOp_Deref  ->
                T_InsOp_LocOSrc  ->
                T_InsOp_ImmSz  ->
                T_Imm  ->
                T_Instr 
sem_Instr_Op op_ opndTy_ locDst_ ind_ locSrc_ immSz_ imm_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _immOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _immOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _opIenc :: Int
              _opImn :: GenC
              _opIpp :: GenC
              _opndTyIenc :: Int
              _opndTyImn :: GenC
              _opndTyIpp :: GenC
              _locDstIenc :: Int
              _locDstImn :: GenC
              _locDstIpp :: GenC
              _indIenc :: Int
              _indImn :: GenC
              _indIpp :: GenC
              _indIself :: InsOp_Deref 
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _immSzIenc :: Int
              _immSzImn :: GenC
              _immSzIpp :: GenC
              _immSzIself :: InsOp_ImmSz 
              _immIbcode :: ([Int])
              _immIcLoc :: CodeAbsLoc
              _immIgenc :: GenC
              _immImn :: GenC
              _immIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 145, column 17)
              _immOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 565, column 17)
              _mn =
                  "o"             >|< _opImn >|< _locDstImn >|< _opndTyImn >|< _indImn >|< _locSrcImn >|< _immSzImn >#< _immImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 657, column 17)
              _enc =
                  0xC0 .|. (_opIenc `shiftL` 3) .|. (_opndTyIenc `shiftL` 1) .|. (_locDstIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 688, column 17)
              _enc2 =
                  (_indIenc `shiftL` 4) .|. (_locSrcIenc `shiftL` 2) .|. (_immSzIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 756, column 17)
              _immOimmSz =
                  _immSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1160, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,_immIpp]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1218, column 17)
              _lhsObcode =
                  [_enc, _enc2] ++ _immIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _immIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              ( _opIenc,_opImn,_opIpp) =
                  op_ 
              ( _opndTyIenc,_opndTyImn,_opndTyIpp) =
                  opndTy_ 
              ( _locDstIenc,_locDstImn,_locDstIpp) =
                  locDst_ 
              ( _indIenc,_indImn,_indIpp,_indIself) =
                  ind_ 
              ( _locSrcIenc,_locSrcImn,_locSrcIpp) =
                  locSrc_ 
              ( _immSzIenc,_immSzImn,_immSzIpp,_immSzIself) =
                  immSz_ 
              ( _immIbcode,_immIcLoc,_immIgenc,_immImn,_immIpp) =
                  imm_ _immOcLoc _immOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_RetCall :: T_InsOp_ImmSz  ->
                     T_InsOp_ImmSz  ->
                     T_Imm  ->
                     T_Imm  ->
                     T_Instr 
sem_Instr_RetCall nArgMineSz_ nArgSurrSz_ nArgMine_ nArgSurr_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _nArgMineOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _nArgMineOimmSz :: InsOp_ImmSz 
              _nArgSurrOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _nArgSurrOcLoc :: CodeAbsLoc
              _nArgMineSzIenc :: Int
              _nArgMineSzImn :: GenC
              _nArgMineSzIpp :: GenC
              _nArgMineSzIself :: InsOp_ImmSz 
              _nArgSurrSzIenc :: Int
              _nArgSurrSzImn :: GenC
              _nArgSurrSzIpp :: GenC
              _nArgSurrSzIself :: InsOp_ImmSz 
              _nArgMineIbcode :: ([Int])
              _nArgMineIcLoc :: CodeAbsLoc
              _nArgMineIgenc :: GenC
              _nArgMineImn :: GenC
              _nArgMineIpp :: GenC
              _nArgSurrIbcode :: ([Int])
              _nArgSurrIcLoc :: CodeAbsLoc
              _nArgSurrIgenc :: GenC
              _nArgSurrImn :: GenC
              _nArgSurrIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 150, column 17)
              _nArgMineOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 551, column 17)
              _mn =
                  "retcall"       >|< _nArgMineSzImn >|< _nArgSurrSzImn >#< _nArgMineImn >#< _nArgSurrImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 664, column 17)
              _enc =
                  0xF4
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 685, column 17)
              _enc2 =
                  (_nArgMineSzIenc `shiftL` 2) .|. (_nArgSurrSzIenc `shiftL` 0)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 758, column 17)
              _nArgMineOimmSz =
                  _nArgMineSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 759, column 17)
              _nArgSurrOimmSz =
                  _nArgSurrSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1152, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,_nArgMineIpp,_nArgSurrIpp]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1210, column 17)
              _lhsObcode =
                  [_enc, _enc2] ++ _nArgMineIbcode ++ _nArgSurrIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _nArgSurrIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              -- copy rule (chain)
              _nArgSurrOcLoc =
                  _nArgMineIcLoc
              ( _nArgMineSzIenc,_nArgMineSzImn,_nArgMineSzIpp,_nArgMineSzIself) =
                  nArgMineSz_ 
              ( _nArgSurrSzIenc,_nArgSurrSzImn,_nArgSurrSzIpp,_nArgSurrSzIself) =
                  nArgSurrSz_ 
              ( _nArgMineIbcode,_nArgMineIcLoc,_nArgMineIgenc,_nArgMineImn,_nArgMineIpp) =
                  nArgMine_ _nArgMineOcLoc _nArgMineOimmSz 
              ( _nArgSurrIbcode,_nArgSurrIcLoc,_nArgSurrIgenc,_nArgSurrImn,_nArgSurrIpp) =
                  nArgSurr_ _nArgSurrOcLoc _nArgSurrOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_RetCase :: T_InsOp_ImmSz  ->
                     T_InsOp_ImmSz  ->
                     T_Imm  ->
                     T_Imm  ->
                     LinkChainEntry ->
                     T_Instr 
sem_Instr_RetCase nResSz_ retOffSurrSz_ nRes_ retOffSurr_ linkChainEntry_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _nResOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOhasCode :: Bool
              _nResOimmSz :: InsOp_ImmSz 
              _retOffSurrOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOstringConstMp :: StringConstMp
              _retOffSurrOcLoc :: CodeAbsLoc
              _nResSzIenc :: Int
              _nResSzImn :: GenC
              _nResSzIpp :: GenC
              _nResSzIself :: InsOp_ImmSz 
              _retOffSurrSzIenc :: Int
              _retOffSurrSzImn :: GenC
              _retOffSurrSzIpp :: GenC
              _retOffSurrSzIself :: InsOp_ImmSz 
              _nResIbcode :: ([Int])
              _nResIcLoc :: CodeAbsLoc
              _nResIgenc :: GenC
              _nResImn :: GenC
              _nResIpp :: GenC
              _retOffSurrIbcode :: ([Int])
              _retOffSurrIcLoc :: CodeAbsLoc
              _retOffSurrIgenc :: GenC
              _retOffSurrImn :: GenC
              _retOffSurrIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 141, column 17)
              _linkCLoc =
                  _retOffSurrIcLoc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 157, column 17)
              _nResOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 188, column 17)
              _lhsOgathLabelLocMp =
                  mkLabelLocMp _linkCLoc linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp linkChainEntry_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup27 =
                  updLinkChainResolvedIndInfoSet linkChainEntry_ _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup27
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup27
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 552, column 17)
              _mn =
                  "retcase"       >|< _nResSzImn >|< _retOffSurrSzImn >#< _nResImn >#< _retOffSurrImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 665, column 17)
              _enc =
                  0xF5
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 687, column 17)
              _enc2 =
                  (_nResSzIenc `shiftL` 2) .|. (_retOffSurrSzIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 762, column 17)
              _nResOimmSz =
                  _nResSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 763, column 17)
              _retOffSurrOimmSz =
                  _retOffSurrSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1159, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,_nResIpp,_retOffSurrIpp, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1217, column 17)
              _lhsObcode =
                  [_enc, _enc2] ++ _nResIbcode ++ _retOffSurrIbcode ++ bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              -- copy rule (chain)
              _retOffSurrOcLoc =
                  _nResIcLoc
              ( _nResSzIenc,_nResSzImn,_nResSzIpp,_nResSzIself) =
                  nResSz_ 
              ( _retOffSurrSzIenc,_retOffSurrSzImn,_retOffSurrSzIpp,_retOffSurrSzIself) =
                  retOffSurrSz_ 
              ( _nResIbcode,_nResIcLoc,_nResIgenc,_nResImn,_nResIpp) =
                  nRes_ _nResOcLoc _nResOimmSz 
              ( _retOffSurrIbcode,_retOffSurrIcLoc,_retOffSurrIgenc,_retOffSurrImn,_retOffSurrIpp) =
                  retOffSurr_ _retOffSurrOcLoc _retOffSurrOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_St :: T_InsOp_DerefB  ->
                T_InsOp_LocE  ->
                T_InsOp_LocB  ->
                T_InsOp_ImmSz  ->
                T_Imm  ->
                T_Instr 
sem_Instr_St ind_ locDst_ locSrc_ immSz_ imm_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _immOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _immOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _indIenc :: Int
              _indImn :: GenC
              _indIpp :: GenC
              _locDstIenc :: Int
              _locDstImn :: GenC
              _locDstIpp :: GenC
              _locDstIself :: InsOp_LocE 
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              _immSzIenc :: Int
              _immSzImn :: GenC
              _immSzIpp :: GenC
              _immSzIself :: InsOp_ImmSz 
              _immIbcode :: ([Int])
              _immIcLoc :: CodeAbsLoc
              _immIgenc :: GenC
              _immImn :: GenC
              _immIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 144, column 17)
              _immOcLoc =
                  _lhsIcLoc + 1
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 540, column 17)
              _mn =
                  "s"             >|< _indImn >|< _locDstImn >|< _locSrcImn >|< _immSzImn >#< _immImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 655, column 17)
              _enc =
                  0x80 .|. (_indIenc `shiftL` 5) .|. (_locDstIenc `shiftL` 3) .|. (_locSrcIenc `shiftL` 2) .|. (_immSzIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 755, column 17)
              _immOimmSz =
                  _immSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1145, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,_immIpp]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1205, column 17)
              _lhsObcode =
                  _enc : _immIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _immIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              ( _indIenc,_indImn,_indIpp) =
                  ind_ 
              ( _locDstIenc,_locDstImn,_locDstIpp,_locDstIself) =
                  locDst_ 
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
              ( _immSzIenc,_immSzImn,_immSzIpp,_immSzIself) =
                  immSz_ 
              ( _immIbcode,_immIcLoc,_immIgenc,_immImn,_immIpp) =
                  imm_ _immOcLoc _immOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_TagInt2Word :: T_Instr 
sem_Instr_TagInt2Word  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 159, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 556, column 17)
              _mn =
                  genc "tagi2w"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 678, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 690, column 17)
              _enc2 =
                  0xFC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 803, column 21)
              _gencL =
                  [vmCall "TagInt2WordTos" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1163, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1221, column 17)
              _lhsObcode =
                  [_enc, _enc2]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_TagWord2Word :: T_Instr 
sem_Instr_TagWord2Word  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 159, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 560, column 17)
              _mn =
                  genc "tagw2w"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 678, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 694, column 17)
              _enc2 =
                  0xFA
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 805, column 21)
              _gencL =
                  [vmCall "TagWord2WordTos" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1163, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1221, column 17)
              _lhsObcode =
                  [_enc, _enc2]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_TailApply :: T_InsOp_LocB  ->
                       T_InsOp_ImmSz  ->
                       T_InsOp_ImmSz  ->
                       T_Imm  ->
                       T_Imm  ->
                       T_Instr 
sem_Instr_TailApply locSrc_ nArgMineSz_ nArgSurrSz_ nArgMine_ nArgSurr_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _nArgMineOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _nArgMineOimmSz :: InsOp_ImmSz 
              _nArgSurrOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _nArgSurrOcLoc :: CodeAbsLoc
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              _nArgMineSzIenc :: Int
              _nArgMineSzImn :: GenC
              _nArgMineSzIpp :: GenC
              _nArgMineSzIself :: InsOp_ImmSz 
              _nArgSurrSzIenc :: Int
              _nArgSurrSzImn :: GenC
              _nArgSurrSzIpp :: GenC
              _nArgSurrSzIself :: InsOp_ImmSz 
              _nArgMineIbcode :: ([Int])
              _nArgMineIcLoc :: CodeAbsLoc
              _nArgMineIgenc :: GenC
              _nArgMineImn :: GenC
              _nArgMineIpp :: GenC
              _nArgSurrIbcode :: ([Int])
              _nArgSurrIcLoc :: CodeAbsLoc
              _nArgSurrIgenc :: GenC
              _nArgSurrImn :: GenC
              _nArgSurrIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 152, column 17)
              _nArgMineOcLoc =
                  _lhsIcLoc + 3
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 545, column 17)
              _mn =
                  "tailapply"     >|< _locSrcImn >|< _nArgMineSzImn >|< _nArgSurrSzImn >#< _nArgMineImn >#< _nArgSurrImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 670, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 683, column 17)
              _enc2 =
                  (0x02 `shiftL` 1) .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 699, column 17)
              _enc3 =
                  (_nArgMineSzIenc `shiftL` 2) .|. (_nArgSurrSzIenc `shiftL` 0)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 758, column 17)
              _nArgMineOimmSz =
                  _nArgMineSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 759, column 17)
              _nArgSurrOimmSz =
                  _nArgSurrSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1154, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,ppHex 2 _enc3,_nArgMineIpp,_nArgSurrIpp]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1212, column 17)
              _lhsObcode =
                  [_enc, _enc2, _enc3] ++ _nArgMineIbcode ++ _nArgSurrIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _nArgSurrIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              -- copy rule (chain)
              _nArgSurrOcLoc =
                  _nArgMineIcLoc
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
              ( _nArgMineSzIenc,_nArgMineSzImn,_nArgMineSzIpp,_nArgMineSzIself) =
                  nArgMineSz_ 
              ( _nArgSurrSzIenc,_nArgSurrSzImn,_nArgSurrSzIpp,_nArgSurrSzIself) =
                  nArgSurrSz_ 
              ( _nArgMineIbcode,_nArgMineIcLoc,_nArgMineIgenc,_nArgMineImn,_nArgMineIpp) =
                  nArgMine_ _nArgMineOcLoc _nArgMineOimmSz 
              ( _nArgSurrIbcode,_nArgSurrIcLoc,_nArgSurrIgenc,_nArgSurrImn,_nArgSurrIpp) =
                  nArgSurr_ _nArgSurrOcLoc _nArgSurrOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_TailCall :: T_InsOp_LocB  ->
                      T_InsOp_ImmSz  ->
                      T_InsOp_ImmSz  ->
                      T_Imm  ->
                      T_Imm  ->
                      T_Instr 
sem_Instr_TailCall locSrc_ nArgMineSz_ nArgSurrSz_ nArgMine_ nArgSurr_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _nArgMineOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _nArgMineOimmSz :: InsOp_ImmSz 
              _nArgSurrOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _nArgSurrOcLoc :: CodeAbsLoc
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              _nArgMineSzIenc :: Int
              _nArgMineSzImn :: GenC
              _nArgMineSzIpp :: GenC
              _nArgMineSzIself :: InsOp_ImmSz 
              _nArgSurrSzIenc :: Int
              _nArgSurrSzImn :: GenC
              _nArgSurrSzIpp :: GenC
              _nArgSurrSzIself :: InsOp_ImmSz 
              _nArgMineIbcode :: ([Int])
              _nArgMineIcLoc :: CodeAbsLoc
              _nArgMineIgenc :: GenC
              _nArgMineImn :: GenC
              _nArgMineIpp :: GenC
              _nArgSurrIbcode :: ([Int])
              _nArgSurrIcLoc :: CodeAbsLoc
              _nArgSurrIgenc :: GenC
              _nArgSurrImn :: GenC
              _nArgSurrIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 152, column 17)
              _nArgMineOcLoc =
                  _lhsIcLoc + 3
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 544, column 17)
              _mn =
                  "tailcall"      >|< _locSrcImn >|< _nArgMineSzImn >|< _nArgSurrSzImn >#< _nArgMineImn >#< _nArgSurrImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 670, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 681, column 17)
              _enc2 =
                  (0x00 `shiftL` 1) .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 699, column 17)
              _enc3 =
                  (_nArgMineSzIenc `shiftL` 2) .|. (_nArgSurrSzIenc `shiftL` 0)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 758, column 17)
              _nArgMineOimmSz =
                  _nArgMineSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 759, column 17)
              _nArgSurrOimmSz =
                  _nArgSurrSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1154, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,ppHex 2 _enc3,_nArgMineIpp,_nArgSurrIpp]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1212, column 17)
              _lhsObcode =
                  [_enc, _enc2, _enc3] ++ _nArgMineIbcode ++ _nArgSurrIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _nArgSurrIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
              -- copy rule (chain)
              _nArgSurrOcLoc =
                  _nArgMineIcLoc
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
              ( _nArgMineSzIenc,_nArgMineSzImn,_nArgMineSzIpp,_nArgMineSzIself) =
                  nArgMineSz_ 
              ( _nArgSurrSzIenc,_nArgSurrSzImn,_nArgSurrSzIpp,_nArgSurrSzIself) =
                  nArgSurrSz_ 
              ( _nArgMineIbcode,_nArgMineIcLoc,_nArgMineIgenc,_nArgMineImn,_nArgMineIpp) =
                  nArgMine_ _nArgMineOcLoc _nArgMineOimmSz 
              ( _nArgSurrIbcode,_nArgSurrIcLoc,_nArgSurrIgenc,_nArgSurrImn,_nArgSurrIpp) =
                  nArgSurr_ _nArgSurrOcLoc _nArgSurrOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_TailEval :: T_InsOp_LocB  ->
                      T_InsOp_ImmSz  ->
                      T_Imm  ->
                      CallInfo ->
                      T_Instr 
sem_Instr_TailEval locSrc_ nArgSurrSz_ nArgSurr_ callInfo_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _linkCLoc :: CodeAbsLoc
              _lhsOcLoc :: CodeAbsLoc
              _nArgSurrOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _linkChainEntry :: LinkChainEntry
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsObytePoolMp :: BytePoolMp
              _gcStackInfo :: GCStackInfo
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOstringConstMp :: StringConstMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOhasCode :: Bool
              _nArgSurrOimmSz :: InsOp_ImmSz 
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgencL :: ([GenC])
              _locSrcIenc :: Int
              _locSrcImn :: GenC
              _locSrcIpp :: GenC
              _locSrcIself :: InsOp_LocB 
              _nArgSurrSzIenc :: Int
              _nArgSurrSzImn :: GenC
              _nArgSurrSzIpp :: GenC
              _nArgSurrSzIself :: InsOp_ImmSz 
              _nArgSurrIbcode :: ([Int])
              _nArgSurrIcLoc :: CodeAbsLoc
              _nArgSurrIgenc :: GenC
              _nArgSurrImn :: GenC
              _nArgSurrIpp :: GenC
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 133, column 17)
              _linkCLoc =
                  _nArgSurrIcLoc
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 147, column 17)
              _lhsOcLoc =
                  _linkCLoc + Cfg.sizeofWord
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 156, column 17)
              _nArgSurrOcLoc =
                  _lhsIcLoc + 3
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 204, column 17)
              _linkChainEntry =
                  LinkChainEntry (LinkChainKey LinkChainKind_CallInfo (LinkChainId_Loc _linkCLoc)) _ciInx
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 225, column 17)
              _lhsOgathLinkChainKeyMp =
                  mkLinkChainKeyMp _linkChainEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              __tup28 =
                  updLinkChainResolvedIndInfoSet _linkChainEntry _lhsIlinkChainResolvedMp _lhsIlinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_linkChain,_) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 250, column 17)
              (_,_lhsOlinkChainResolvedIndInfoSet) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 339, column 17)
              _lhsObytePoolMp =
                  _bytePoolMp2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 360, column 17)
              _gcStackInfo =
                  ciGCStackInfo callInfo_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              __tup29 =
                  gcStackInfoMpAdd _gcStackInfo _lhsIgcStackInfoMp _lhsIbytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_lhsOgcStackInfoMp,_,_) =
                  __tup29
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_bytePoolMp1,_) =
                  __tup29
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 365, column 17)
              (_,_,_gcStackInfoInx) =
                  __tup29
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              __tup30 =
                  case ciMbKey callInfo_ of
                    Just s -> stringConstMpAdd s _lhsIstringConstMp _bytePoolMp1
                    _      -> (_lhsIstringConstMp,_bytePoolMp1,Nothing)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_lhsOstringConstMp,_,_) =
                  __tup30
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_bytePoolMp2,_) =
                  __tup30
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 388, column 17)
              (_,_,_stringConstInx) =
                  __tup30
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              __tup31 =
                  ciMpAdd (callInfo_ { ciGCStackInfo = _gcStackInfoInx
                                     , ciMbKey       = fmap (\x -> (x,Nothing)) _stringConstInx
                                     }) _lhsIcallInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_lhsOcallInfoMp,_) =
                  __tup31
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 477, column 17)
              (_,_ciInx) =
                  __tup31
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 546, column 17)
              _mn =
                  "taileval"      >|< _locSrcImn >|< _nArgSurrSzImn >#< _nArgSurrImn
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 670, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 682, column 17)
              _enc2 =
                  (0x01 `shiftL` 1) .|. (_locSrcIenc)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 700, column 17)
              _enc3 =
                  (_nArgSurrSzIenc `shiftL` 0)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 760, column 17)
              _nArgSurrOimmSz =
                  _nArgSurrSzIself
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 818, column 21)
              _gencL =
                  [gencCmt _mn]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1155, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2,ppHex 2 _enc3,_nArgSurrIpp, ppLinkChain _linkChain]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1213, column 17)
              _lhsObcode =
                  [_enc, _enc2, _enc3] ++ _nArgSurrIbcode ++ bcodeLinkChain _linkChain
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              ( _locSrcIenc,_locSrcImn,_locSrcIpp,_locSrcIself) =
                  locSrc_ 
              ( _nArgSurrSzIenc,_nArgSurrSzImn,_nArgSurrSzIpp,_nArgSurrSzIself) =
                  nArgSurrSz_ 
              ( _nArgSurrIbcode,_nArgSurrIcLoc,_nArgSurrIgenc,_nArgSurrImn,_nArgSurrIpp) =
                  nArgSurr_ _nArgSurrOcLoc _nArgSurrOimmSz 
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_UntagWord2Int :: T_Instr 
sem_Instr_UntagWord2Int  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 159, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 558, column 17)
              _mn =
                  genc "untagw2i"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 678, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 692, column 17)
              _enc2 =
                  0xFD
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 804, column 21)
              _gencL =
                  [vmCall "UntagWord2IntTos" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1163, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1221, column 17)
              _lhsObcode =
                  [_enc, _enc2]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
sem_Instr_UntagWord2Word :: T_Instr 
sem_Instr_UntagWord2Word  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOcLoc :: CodeAbsLoc
              _lhsOisEntry :: Bool
              _lhsOhasCode :: Bool
              _lhsOmbFunStart :: (Maybe FunctionInfo)
              _lhsOpp :: GenC
              _lhsObcode :: ([Int])
              _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOmn :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 159, column 17)
              _lhsOcLoc =
                  _lhsIcLoc + 2
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 175, column 17)
              _lhsOisEntry =
                  False
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 509, column 17)
              _lhsOhasCode =
                  True
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 562, column 17)
              _mn =
                  genc "untagw2w"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 678, column 17)
              _enc =
                  0xFE
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 696, column 17)
              _enc2 =
                  0xFB
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 806, column 21)
              _gencL =
                  [vmCall "UntagWord2WordTos" []]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 831, column 17)
              _lhsOmbFunStart =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1163, column 17)
              _lhsOpp =
                  ppCommas [ppHex 2 _enc,ppHex 2 _enc2]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1221, column 17)
              _lhsObcode =
                  [_enc, _enc2]
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 516, column 32)
              _lhsOgathAnnL =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 536, column 34)
              _lhsOmn =
                  _mn
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOhasCode,_lhsOisEntry,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOmbFunStart,_lhsOmn,_lhsOpp,_lhsOstringConstMp)))
-- Instrs ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         annL                 : AnnL
         impNmMp              : Map.Map HsName Int
         labelLocMp           : LabelLocMp
         linkChainResolvedMp  : LinkChainResolvedMp
         lkupLookupFunctionInfoInx : LookupFunctionInfoInx
         opts                 : EHCOpts
         ppNm                 : String -> GenC
      chained attributes:
         bytePoolMp           : BytePoolMp
         cLoc                 : CodeAbsLoc
         callInfoMp           : CallInfoMp
         functionInfoMp       : FunctionInfoMp
         gcStackInfoMp        : GCStackInfoMp
         linkChainResolvedIndInfoSet : LinkChainResolvedIndInfoSet
         stringConstMp        : StringConstMp
      synthesized attributes:
         bcTrL                : [BCTrL]
         bcode                : [Int]
         fun2CMp              : Fun2CMp
         gathLabelLocMp       : LabelLocMp
         gathLinkChainKeyMp   : LinkChainKeyMp
         gencL                : [GenC]
         locRefs              : Seq.FastSeq LocRefLoc
         pp                   : GenC
         ppL                  : [GenC]
   alternatives:
      alternative Cons:
         child hd             : Instr 
         child tl             : Instrs 
         visit 0:
            local _tup32      : _
            local annL        : _
            local gencL       : _
            local _tup33      : {(Fun2CMp,[GenC])}
      alternative Nil:
         visit 0:
            local gencL       : _
-}
-- cata
sem_Instrs :: Instrs  ->
              T_Instrs 
sem_Instrs list  =
    (Prelude.foldr sem_Instrs_Cons sem_Instrs_Nil (Prelude.map sem_Instr list) )
-- semantic domain
type T_Instrs  = AnnL ->
                 BytePoolMp ->
                 CodeAbsLoc ->
                 CallInfoMp ->
                 FunctionInfoMp ->
                 GCStackInfoMp ->
                 (Map.Map HsName Int) ->
                 LabelLocMp ->
                 LinkChainResolvedIndInfoSet ->
                 LinkChainResolvedMp ->
                 LookupFunctionInfoInx ->
                 EHCOpts ->
                 (String -> GenC) ->
                 StringConstMp ->
                 ( ([BCTrL]),([Int]),BytePoolMp,CodeAbsLoc,CallInfoMp,Fun2CMp,FunctionInfoMp,LabelLocMp,LinkChainKeyMp,GCStackInfoMp,([GenC]),LinkChainResolvedIndInfoSet,(Seq.FastSeq LocRefLoc),GenC,([GenC]),StringConstMp)
sem_Instrs_Cons :: T_Instr  ->
                   T_Instrs  ->
                   T_Instrs 
sem_Instrs_Cons hd_ tl_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsObcTrL :: ([BCTrL])
              _tlOannL :: AnnL
              __tup33 :: ((Fun2CMp,[GenC]))
              _lhsOfun2CMp :: Fun2CMp
              _lhsOgencL :: ([GenC])
              _lhsOppL :: ([GenC])
              _lhsObcode :: ([Int])
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOpp :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              _hdOannL :: AnnL
              _hdObytePoolMp :: BytePoolMp
              _hdOcLoc :: CodeAbsLoc
              _hdOcallInfoMp :: CallInfoMp
              _hdOfunctionInfoMp :: FunctionInfoMp
              _hdOgcStackInfoMp :: GCStackInfoMp
              _hdOimpNmMp :: (Map.Map HsName Int)
              _hdOlabelLocMp :: LabelLocMp
              _hdOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _hdOlinkChainResolvedMp :: LinkChainResolvedMp
              _hdOlkupLookupFunctionInfoInx :: LookupFunctionInfoInx
              _hdOopts :: EHCOpts
              _hdOppNm :: (String -> GenC)
              _hdOstringConstMp :: StringConstMp
              _tlObytePoolMp :: BytePoolMp
              _tlOcLoc :: CodeAbsLoc
              _tlOcallInfoMp :: CallInfoMp
              _tlOfunctionInfoMp :: FunctionInfoMp
              _tlOgcStackInfoMp :: GCStackInfoMp
              _tlOimpNmMp :: (Map.Map HsName Int)
              _tlOlabelLocMp :: LabelLocMp
              _tlOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _tlOlinkChainResolvedMp :: LinkChainResolvedMp
              _tlOlkupLookupFunctionInfoInx :: LookupFunctionInfoInx
              _tlOopts :: EHCOpts
              _tlOppNm :: (String -> GenC)
              _tlOstringConstMp :: StringConstMp
              _hdIbcode :: ([Int])
              _hdIbytePoolMp :: BytePoolMp
              _hdIcLoc :: CodeAbsLoc
              _hdIcallInfoMp :: CallInfoMp
              _hdIfunctionInfoMp :: FunctionInfoMp
              _hdIgathAnnL :: AnnL
              _hdIgathLabelLocMp :: LabelLocMp
              _hdIgathLinkChainKeyMp :: LinkChainKeyMp
              _hdIgcStackInfoMp :: GCStackInfoMp
              _hdIgencL :: ([GenC])
              _hdIhasCode :: Bool
              _hdIisEntry :: Bool
              _hdIlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _hdIlocRefs :: (Seq.FastSeq LocRefLoc)
              _hdImbFunStart :: (Maybe FunctionInfo)
              _hdImn :: GenC
              _hdIpp :: GenC
              _hdIstringConstMp :: StringConstMp
              _tlIbcTrL :: ([BCTrL])
              _tlIbcode :: ([Int])
              _tlIbytePoolMp :: BytePoolMp
              _tlIcLoc :: CodeAbsLoc
              _tlIcallInfoMp :: CallInfoMp
              _tlIfun2CMp :: Fun2CMp
              _tlIfunctionInfoMp :: FunctionInfoMp
              _tlIgathLabelLocMp :: LabelLocMp
              _tlIgathLinkChainKeyMp :: LinkChainKeyMp
              _tlIgcStackInfoMp :: GCStackInfoMp
              _tlIgencL :: ([GenC])
              _tlIlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _tlIlocRefs :: (Seq.FastSeq LocRefLoc)
              _tlIpp :: GenC
              _tlIppL :: ([GenC])
              _tlIstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 496, column 17)
              _lhsObcTrL =
                  if _hdIhasCode
                  then let i = (_lhsIcLoc,_hdIcLoc - _lhsIcLoc,ppCommas' ([_hdImn] ++ [ k >|< "=" >|< m | (k,m) <- _annL, allow k]))
                           allow k = not (k `elem` [AnnComment,AnnStackDepth])
                       in  case _tlIbcTrL of
                             (l:ls) -> (i:l) : ls
                  else if _hdIisEntry then [] : _tlIbcTrL else _tlIbcTrL
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 528, column 17)
              __tup32 =
                  let m = _lhsIannL ++ _hdIgathAnnL
                  in  if _hdIhasCode
                      then (m,[])
                      else ([],m)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 528, column 17)
              (_annL,_) =
                  __tup32
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 528, column 17)
              (_,_tlOannL) =
                  __tup32
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 780, column 21)
              _gencL =
                  map gencStat _hdIgencL ++ _tlIgencL
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 841, column 33)
              __tup33 =
                  case _hdImbFunStart of
                      Just i -> (Map.insert (hsnShowAlphanumeric $ funinfoNm i) _tlIgencL _tlIfun2CMp, [])
                      _      -> (_tlIfun2CMp, _gencL)
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 841, column 33)
              (_lhsOfun2CMp,_) =
                  __tup33
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 841, column 33)
              (_,_lhsOgencL) =
                  __tup33
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1180, column 17)
              _lhsOppL =
                  if _hdIhasCode
                  then let instr = indent 3 _hdIpp
                           locmnemo = ppCmtC _lhsIopts (_lhsIcLoc >|< ":" >#< _hdImn)
                           metapp = indent 40 (vlist $ map (\(k,m) -> ppCmtC _lhsIopts (k >#< "[" >#< m >#< "]")) $ _annL)
                       in  (locmnemo >|< metapp >-< instr) : _tlIppL
                  else _tlIppL
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1228, column 17)
              _lhsObcode =
                  if _hdIhasCode
                  then _hdIbcode ++ _tlIbcode
                  else _tlIbcode
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  _hdIgathLabelLocMp `Map.union` _tlIgathLabelLocMp
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  _hdIgathLinkChainKeyMp `Map.union` _tlIgathLinkChainKeyMp
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  _hdIlocRefs Seq.:++: _tlIlocRefs
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
              _lhsOpp =
                  _hdIpp >-< _tlIpp
              -- copy rule (up)
              _lhsObytePoolMp =
                  _tlIbytePoolMp
              -- copy rule (up)
              _lhsOcLoc =
                  _tlIcLoc
              -- copy rule (up)
              _lhsOcallInfoMp =
                  _tlIcallInfoMp
              -- copy rule (up)
              _lhsOfunctionInfoMp =
                  _tlIfunctionInfoMp
              -- copy rule (up)
              _lhsOgcStackInfoMp =
                  _tlIgcStackInfoMp
              -- copy rule (up)
              _lhsOlinkChainResolvedIndInfoSet =
                  _tlIlinkChainResolvedIndInfoSet
              -- copy rule (up)
              _lhsOstringConstMp =
                  _tlIstringConstMp
              -- copy rule (from local)
              _hdOannL =
                  _annL
              -- copy rule (down)
              _hdObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (down)
              _hdOcLoc =
                  _lhsIcLoc
              -- copy rule (down)
              _hdOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (down)
              _hdOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (down)
              _hdOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (down)
              _hdOimpNmMp =
                  _lhsIimpNmMp
              -- copy rule (down)
              _hdOlabelLocMp =
                  _lhsIlabelLocMp
              -- copy rule (down)
              _hdOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (down)
              _hdOlinkChainResolvedMp =
                  _lhsIlinkChainResolvedMp
              -- copy rule (down)
              _hdOlkupLookupFunctionInfoInx =
                  _lhsIlkupLookupFunctionInfoInx
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOppNm =
                  _lhsIppNm
              -- copy rule (down)
              _hdOstringConstMp =
                  _lhsIstringConstMp
              -- copy rule (chain)
              _tlObytePoolMp =
                  _hdIbytePoolMp
              -- copy rule (chain)
              _tlOcLoc =
                  _hdIcLoc
              -- copy rule (chain)
              _tlOcallInfoMp =
                  _hdIcallInfoMp
              -- copy rule (chain)
              _tlOfunctionInfoMp =
                  _hdIfunctionInfoMp
              -- copy rule (chain)
              _tlOgcStackInfoMp =
                  _hdIgcStackInfoMp
              -- copy rule (down)
              _tlOimpNmMp =
                  _lhsIimpNmMp
              -- copy rule (down)
              _tlOlabelLocMp =
                  _lhsIlabelLocMp
              -- copy rule (chain)
              _tlOlinkChainResolvedIndInfoSet =
                  _hdIlinkChainResolvedIndInfoSet
              -- copy rule (down)
              _tlOlinkChainResolvedMp =
                  _lhsIlinkChainResolvedMp
              -- copy rule (down)
              _tlOlkupLookupFunctionInfoInx =
                  _lhsIlkupLookupFunctionInfoInx
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOppNm =
                  _lhsIppNm
              -- copy rule (chain)
              _tlOstringConstMp =
                  _hdIstringConstMp
              ( _hdIbcode,_hdIbytePoolMp,_hdIcLoc,_hdIcallInfoMp,_hdIfunctionInfoMp,_hdIgathAnnL,_hdIgathLabelLocMp,_hdIgathLinkChainKeyMp,_hdIgcStackInfoMp,_hdIgencL,_hdIhasCode,_hdIisEntry,_hdIlinkChainResolvedIndInfoSet,_hdIlocRefs,_hdImbFunStart,_hdImn,_hdIpp,_hdIstringConstMp) =
                  hd_ _hdOannL _hdObytePoolMp _hdOcLoc _hdOcallInfoMp _hdOfunctionInfoMp _hdOgcStackInfoMp _hdOimpNmMp _hdOlabelLocMp _hdOlinkChainResolvedIndInfoSet _hdOlinkChainResolvedMp _hdOlkupLookupFunctionInfoInx _hdOopts _hdOppNm _hdOstringConstMp 
              ( _tlIbcTrL,_tlIbcode,_tlIbytePoolMp,_tlIcLoc,_tlIcallInfoMp,_tlIfun2CMp,_tlIfunctionInfoMp,_tlIgathLabelLocMp,_tlIgathLinkChainKeyMp,_tlIgcStackInfoMp,_tlIgencL,_tlIlinkChainResolvedIndInfoSet,_tlIlocRefs,_tlIpp,_tlIppL,_tlIstringConstMp) =
                  tl_ _tlOannL _tlObytePoolMp _tlOcLoc _tlOcallInfoMp _tlOfunctionInfoMp _tlOgcStackInfoMp _tlOimpNmMp _tlOlabelLocMp _tlOlinkChainResolvedIndInfoSet _tlOlinkChainResolvedMp _tlOlkupLookupFunctionInfoInx _tlOopts _tlOppNm _tlOstringConstMp 
          in  ( _lhsObcTrL,_lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfun2CMp,_lhsOfunctionInfoMp,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOpp,_lhsOppL,_lhsOstringConstMp)))
sem_Instrs_Nil :: T_Instrs 
sem_Instrs_Nil  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcLoc
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsObcTrL :: ([BCTrL])
              _lhsOfun2CMp :: Fun2CMp
              _lhsOppL :: ([GenC])
              _lhsObcode :: ([Int])
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOpp :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcLoc :: CodeAbsLoc
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOgencL :: ([GenC])
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 495, column 17)
              _lhsObcTrL =
                  [[]]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 779, column 21)
              _gencL =
                  []
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 840, column 17)
              _lhsOfun2CMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1179, column 17)
              _lhsOppL =
                  []
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 1227, column 17)
              _lhsObcode =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcLoc =
                  _lhsIcLoc
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (from local)
              _lhsOgencL =
                  _gencL
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObcTrL,_lhsObcode,_lhsObytePoolMp,_lhsOcLoc,_lhsOcallInfoMp,_lhsOfun2CMp,_lhsOfunctionInfoMp,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOgencL,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOpp,_lhsOppL,_lhsOstringConstMp)))
-- Meta --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         annL                 : AnnL
         impNmMp              : Map.Map HsName Int
         labelLocMp           : LabelLocMp
         linkChainResolvedMp  : LinkChainResolvedMp
         lkupLookupFunctionInfoInx : LookupFunctionInfoInx
         opts                 : EHCOpts
         ppNm                 : String -> GenC
      chained attributes:
         bytePoolMp           : BytePoolMp
         callInfoMp           : CallInfoMp
         functionInfoMp       : FunctionInfoMp
         gcStackInfoMp        : GCStackInfoMp
         linkChainResolvedIndInfoSet : LinkChainResolvedIndInfoSet
         stringConstMp        : StringConstMp
      synthesized attributes:
         enc                  : Int
         gathAnnL             : AnnL
         gathLabelLocMp       : LabelLocMp
         gathLinkChainKeyMp   : LinkChainKeyMp
         locRefs              : Seq.FastSeq LocRefLoc
         pp                   : GenC
   alternatives:
      alternative CmtHeader:
         child kind           : {AnnKind}
         child ann            : {PP_Doc}
         visit 0:
            local enc         : _
-}
-- cata
sem_Meta :: Meta  ->
            T_Meta 
sem_Meta (Meta_CmtHeader _kind _ann )  =
    (sem_Meta_CmtHeader _kind _ann )
-- semantic domain
type T_Meta  = AnnL ->
               BytePoolMp ->
               CallInfoMp ->
               FunctionInfoMp ->
               GCStackInfoMp ->
               (Map.Map HsName Int) ->
               LabelLocMp ->
               LinkChainResolvedIndInfoSet ->
               LinkChainResolvedMp ->
               LookupFunctionInfoInx ->
               EHCOpts ->
               (String -> GenC) ->
               StringConstMp ->
               ( BytePoolMp,CallInfoMp,Int,FunctionInfoMp,AnnL,LabelLocMp,LinkChainKeyMp,GCStackInfoMp,LinkChainResolvedIndInfoSet,(Seq.FastSeq LocRefLoc),GenC,StringConstMp)
sem_Meta_CmtHeader :: AnnKind ->
                      PP_Doc ->
                      T_Meta 
sem_Meta_CmtHeader kind_ ann_  =
    (\ _lhsIannL
       _lhsIbytePoolMp
       _lhsIcallInfoMp
       _lhsIfunctionInfoMp
       _lhsIgcStackInfoMp
       _lhsIimpNmMp
       _lhsIlabelLocMp
       _lhsIlinkChainResolvedIndInfoSet
       _lhsIlinkChainResolvedMp
       _lhsIlkupLookupFunctionInfoInx
       _lhsIopts
       _lhsIppNm
       _lhsIstringConstMp ->
         (let _lhsOgathAnnL :: AnnL
              _lhsOgathLabelLocMp :: LabelLocMp
              _lhsOgathLinkChainKeyMp :: LinkChainKeyMp
              _lhsOlocRefs :: (Seq.FastSeq LocRefLoc)
              _lhsOpp :: GenC
              _lhsObytePoolMp :: BytePoolMp
              _lhsOcallInfoMp :: CallInfoMp
              _lhsOenc :: Int
              _lhsOfunctionInfoMp :: FunctionInfoMp
              _lhsOgcStackInfoMp :: GCStackInfoMp
              _lhsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _lhsOstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 520, column 17)
              _lhsOgathAnnL =
                  [(kind_,genc ann_)]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 703, column 17)
              _enc =
                  0xFF
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 182, column 36)
              _lhsOgathLabelLocMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 221, column 40)
              _lhsOgathLinkChainKeyMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 166, column 29)
              _lhsOlocRefs =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 895, column 27)
              _lhsOpp =
                  empty
              -- copy rule (chain)
              _lhsObytePoolMp =
                  _lhsIbytePoolMp
              -- copy rule (chain)
              _lhsOcallInfoMp =
                  _lhsIcallInfoMp
              -- copy rule (from local)
              _lhsOenc =
                  _enc
              -- copy rule (chain)
              _lhsOfunctionInfoMp =
                  _lhsIfunctionInfoMp
              -- copy rule (chain)
              _lhsOgcStackInfoMp =
                  _lhsIgcStackInfoMp
              -- copy rule (chain)
              _lhsOlinkChainResolvedIndInfoSet =
                  _lhsIlinkChainResolvedIndInfoSet
              -- copy rule (chain)
              _lhsOstringConstMp =
                  _lhsIstringConstMp
          in  ( _lhsObytePoolMp,_lhsOcallInfoMp,_lhsOenc,_lhsOfunctionInfoMp,_lhsOgathAnnL,_lhsOgathLabelLocMp,_lhsOgathLinkChainKeyMp,_lhsOgcStackInfoMp,_lhsOlinkChainResolvedIndInfoSet,_lhsOlocRefs,_lhsOpp,_lhsOstringConstMp)))
-- Module ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lkupGrinByteCodeLamInfo : LookupGrinByteCodeLamInfo
         opts                 : EHCOpts
      synthesized attributes:
         functionInfoExportMp : FunctionInfoExportMp
         pp                   : GenC
         ppMain               : GenC
   alternatives:
      alternative Mod:
         child moduleNm       : {String}
         child allImpModNmL   : {AssocL HsName String}
         child impModNmL      : {AssocL HsName Int}
         child expEntryL      : {[EntryInfo]}
         child allEntryL      : {[EntryInfo]}
         child instrs         : Instrs 
         child constL         : {[Const]}
         child cafEntryL      : {[Int]}
         child mainCafEntry   : {Int}
         child includeL       : {[String]}
         visit 0:
            local lkupLookupFunctionInfoInx : _
            local impNmMp     : _
            local labelLocMp  : _
            local _tup34      : _
            local linkChainResolvedMp : _
            local mbLinkChainCodeLoc : _
            local _tup35      : _
            local bytePoolMp  : _
            local allEntryBPedL : _
            local ccallencWrappers : _
            local functionInfoExportMp : _
            local moduleNmPre : _
            local ppNm        : _
            local strNmByteCodeModule : _
            local strNmByteCodeEntry : _
            local ppNmACAF    : _
            local ppNmByteCodeTbl : _
            local ppNmByteCodeTraceTbl : _
            local ppNmByteCodeModule : _
            local ppNmByteCodeEntry : _
            local ppNmConstTbl : _
            local ppNmGlobEntriesTbl : _
            local ppNmCafEntriesTbl : _
            local ppNmCafGlEntryIndicesTbl : _
            local ppNmCafTbl  : _
            local strNmInitF  : _
            local ppNmInitF   : _
            local ppNmMainCAFEntry : _
            local ppNmModEntriesTbl : _
            local ppNmGCStackInfosTbl : _
            local ppNmLinkChainIndTbl : _
            local ppNmCallInfoTbl : _
            local ppNmBytePool : _
            local ppNmFunctionInfoTbl : _
            local strNmImpTbl : _
            local strNmExpNode : _
            local strNmExpNodeSz : _
            local ppImpModTbl : _
            local ppNmExpNode : _
            local ppNmExpNodeOffs : _
            local ppNmExpNodeSz : _
            local _tup36      : {(GenC,GenC)}
-}
-- cata
sem_Module :: Module  ->
              T_Module 
sem_Module (Module_Mod _moduleNm _allImpModNmL _impModNmL _expEntryL _allEntryL _instrs _constL _cafEntryL _mainCafEntry _includeL )  =
    (sem_Module_Mod _moduleNm _allImpModNmL _impModNmL _expEntryL _allEntryL (sem_Instrs _instrs ) _constL _cafEntryL _mainCafEntry _includeL )
-- semantic domain
type T_Module  = LookupGrinByteCodeLamInfo ->
                 EHCOpts ->
                 ( FunctionInfoExportMp,GenC,GenC)
sem_Module_Mod :: String ->
                  (AssocL HsName String) ->
                  (AssocL HsName Int) ->
                  ([EntryInfo]) ->
                  ([EntryInfo]) ->
                  T_Instrs  ->
                  ([Const]) ->
                  ([Int]) ->
                  Int ->
                  ([String]) ->
                  T_Module 
sem_Module_Mod moduleNm_ allImpModNmL_ impModNmL_ expEntryL_ allEntryL_ instrs_ constL_ cafEntryL_ mainCafEntry_ includeL_  =
    (\ _lhsIlkupGrinByteCodeLamInfo
       _lhsIopts ->
         (let _instrsOcLoc :: CodeAbsLoc
              _instrsOlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _instrsObytePoolMp :: BytePoolMp
              _instrsOgcStackInfoMp :: GCStackInfoMp
              _instrsOstringConstMp :: StringConstMp
              _instrsOfunctionInfoMp :: FunctionInfoMp
              _instrsOcallInfoMp :: CallInfoMp
              _instrsOannL :: AnnL
              __tup36 :: ((GenC,GenC))
              _lhsOpp :: GenC
              _lhsOppMain :: GenC
              _lhsOfunctionInfoExportMp :: FunctionInfoExportMp
              _instrsOimpNmMp :: (Map.Map HsName Int)
              _instrsOlabelLocMp :: LabelLocMp
              _instrsOlinkChainResolvedMp :: LinkChainResolvedMp
              _instrsOlkupLookupFunctionInfoInx :: LookupFunctionInfoInx
              _instrsOopts :: EHCOpts
              _instrsOppNm :: (String -> GenC)
              _instrsIbcTrL :: ([BCTrL])
              _instrsIbcode :: ([Int])
              _instrsIbytePoolMp :: BytePoolMp
              _instrsIcLoc :: CodeAbsLoc
              _instrsIcallInfoMp :: CallInfoMp
              _instrsIfun2CMp :: Fun2CMp
              _instrsIfunctionInfoMp :: FunctionInfoMp
              _instrsIgathLabelLocMp :: LabelLocMp
              _instrsIgathLinkChainKeyMp :: LinkChainKeyMp
              _instrsIgcStackInfoMp :: GCStackInfoMp
              _instrsIgencL :: ([GenC])
              _instrsIlinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
              _instrsIlocRefs :: (Seq.FastSeq LocRefLoc)
              _instrsIpp :: GenC
              _instrsIppL :: ([GenC])
              _instrsIstringConstMp :: StringConstMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 71, column 17)
              _lkupLookupFunctionInfoInx =
                  \n -> fmap gblaminfoFuninfoKey
                        $ maybe (_lhsIlkupGrinByteCodeLamInfo n) Just
                        $ Map.lookup n _functionInfoExportMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 79, column 17)
              _impNmMp =
                  Map.fromList impModNmL_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 115, column 17)
              _instrsOcLoc =
                  0
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 196, column 17)
              _labelLocMp =
                  _instrsIgathLabelLocMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 233, column 33)
              __tup34 =
                  linkChainMpResolve _instrsIgathLabelLocMp _instrsIgathLinkChainKeyMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 233, column 33)
              (_linkChainResolvedMp,_) =
                  __tup34
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 233, column 33)
              (_,_mbLinkChainCodeLoc) =
                  __tup34
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 239, column 17)
              _instrsOlinkChainResolvedIndInfoSet =
                  emptyLinkChainResolvedIndInfoSet
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 334, column 17)
              _instrsObytePoolMp =
                  emptyBytePoolMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 369, column 17)
              _instrsOgcStackInfoMp =
                  emptyBPEBasedMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 403, column 17)
              _instrsOstringConstMp =
                  emptyBPEBasedMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 405, column 33)
              __tup35 =
                  foldr (\(e) (sm,bm,es)
                          -> let (sm',bm',inx) = maybe (sm,bm,Nothing) (\s -> stringConstMpAdd s sm bm) (eiMbNmStr e)
                             in  (sm',bm',(e {eiMbNmStr = inx}) : es)
                        )
                        (_instrsIstringConstMp,_instrsIbytePoolMp,[]) allEntryL_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 405, column 33)
              (_,_bytePoolMp,_) =
                  __tup35
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 405, column 33)
              (_,_,_allEntryBPedL) =
                  __tup35
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 415, column 17)
              _ccallencWrappers =
                  let tyPre = "GB_Ty_CFun_"
                  in  [ gencBasicSizeFunTyDef tyPre bs
                        >-< gencFunDef "static void" (gencBasicSizeGBFunTyNm gencBasicSizeFunPrefix bs) []
                              (  gencWrapperCFunDef tyPre Nothing
                                                    (genc nArgs) [res] (Left bs)
                              )
                      | Const_CCallEncWrapper bs@(res:args) <- constL_
                      , let nArgs = length args
                      ]
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 439, column 17)
              _instrsOfunctionInfoMp =
                  emptyBPEBasedMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 445, column 17)
              _functionInfoExportMp =
                  bpeExtractMp (\i -> funinfoNm i) GrinByteCodeLamInfo _instrsIfunctionInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 486, column 17)
              _instrsOcallInfoMp =
                  emptyCallInfoMp
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 534, column 17)
              _instrsOannL =
                  []
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _moduleNmPre =
                  mkPre moduleNm_
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNm =
                  \n -> _moduleNmPre >|< n
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _strNmByteCodeModule =
                  "bytecodeModule"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _strNmByteCodeEntry =
                  "bytecodeEntries"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmACAF =
                  _ppNm "caf"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmByteCodeTbl =
                  _ppNm "bytecode"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmByteCodeTraceTbl =
                  _ppNm "bytecodeInstrEntries"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmByteCodeModule =
                  _ppNm _strNmByteCodeModule
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmByteCodeEntry =
                  _ppNm _strNmByteCodeEntry
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmConstTbl =
                  _ppNm "constants"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmGlobEntriesTbl =
                  _ppNm "globalEntries"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmCafEntriesTbl =
                  _ppNm "cafEntries"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmCafGlEntryIndicesTbl =
                  _ppNm "cafGlEntryIndices"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmCafTbl =
                  _ppNm "cafs"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _strNmInitF =
                  "initModule"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmInitF =
                  _ppNm _strNmInitF
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmMainCAFEntry =
                  _ppNm "mainEntryPtr"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmModEntriesTbl =
                  _ppNm "moduleEntries"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmGCStackInfosTbl =
                  _ppNm strNmGcStackInfos
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmLinkChainIndTbl =
                  _ppNm "linkChainIndirections"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmCallInfoTbl =
                  _ppNm "callinfos"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmBytePool =
                  _ppNm "bytePool"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmFunctionInfoTbl =
                  _ppNm "functionInfos"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _strNmImpTbl =
                  "impMods"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _strNmExpNode =
                  "expNode"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _strNmExpNodeSz =
                  _strNmExpNode ++ "_size"
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppImpModTbl =
                  _ppNm _strNmImpTbl
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmExpNode =
                  _ppNm _strNmExpNode
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmExpNodeOffs =
                  _ppNm (_strNmExpNode ++ "_offs")
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 901, column 17)
              _ppNmExpNodeSz =
                  _ppNm _strNmExpNodeSz
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 934, column 33)
              __tup36 =
                  let top     = ppLine $ ppCmtC _lhsIopts ("bytecode in C encoding for module" >#< moduleNm_)
                                         >-< (vlist $ map gencInclude $
                                                (  ["rts.h", "bc/interpreter.h"]
                                                ++ includeL_
                                             )  )
                      cfuns | ehcOptPriv _lhsIopts  = [ gencFunDef "void" n [] b | (n,b) <- Map.toList $ Map.delete (show hsnMain) $ _instrsIfun2CMp ]
                            | otherwise             = []
                      callinfos
                              = ppTbl' "CallInfo" _ppNmCallInfoTbl
                                  [ "MkCallInfoWith"
                                    >|< ppParensCommas
                                          [ genc $ fromEnum $ ciKind ci
                                          , ppCommas $ mkci $ ciMbKey ci
                                          , gcref
                                          , genc (ciExtra ci)
                                          ]
                                    >#< ppCmtC _lhsIopts (show ci)
                                  | (gcref,ci,i) <- cis
                                  ]
                              where cis = [ (maybe gencNULL (gencAddrArrayAt _ppNmGCStackInfosTbl) $ ciGCStackInfo ci, ci, i)
                                          | (ci,i) <- sortOn snd $ Map.toList $ snd _instrsIcallInfoMp
                                          ]
                                    mkci Nothing               = [gencNULL                       , none, none]
                                    mkci (Just (s,Nothing   )) = [gencAddrArrayAt _ppNmBytePool s, none, none]
                                    mkci (Just (s,Just (m,o))) = [gencAddrArrayAt _ppNmBytePool s, genc m, genc o]
                                    none = genc "FunctionInfo_Inx_None"
                      consts  = ppTbl' "GB_Word" _ppNmConstTbl (map (ppCnst _lhsIopts) constL_ )
                      bcTrL   = tail _instrsIbcTrL
                      bcode   = ppTbl' "GB_Byte" _ppNmByteCodeTbl _instrsIppL
                      bytePool= ppTbl' "Word8" _ppNmBytePool
                                       [ ppCmtC _lhsIopts (loc >#< bytePoolEntryComment e) >-< vlist enc
                                       | (e,loc) <- sortOn (fst.snd) $ Map.toList $ snd _bytePoolMp
                                       , let enc = bytePoolEntryEncoding e
                                       , not (null enc)
                                       ]
                      bcodetrace
                              = ppTbl' "GB_ByteCodeInstrEntry" _ppNmByteCodeTraceTbl
                                  (map (\(l,sz,c) -> ppCurlysCommas [gencAddrArrayAt _ppNmByteCodeTbl l,genc sz,ppPacked "\"" "\"" c])
                                   $ concat bcTrL
                                  )
                      bcodeentries
                              = ppTbl' "GB_ByteCodeEntryPoint" _ppNmByteCodeEntry
                                  (zipWith3 (\i l e
                                              -> let (bc,bcs) = if ehcOptGenTrace _lhsIopts then (gencAddrArrayAt _ppNmByteCodeTraceTbl i, l) else (gencNULL,0)
                                                 in  ppCurlysCommas
                                                       [ (maybe gencNULL (gencAddrArrayAt _ppNmBytePool) (eiMbNmStr e)) >#< ppCmtC _lhsIopts (eiNm e)
                                                       , bc, genc bcs
                                                       ]
                                            ) (scanl (+) 0 bcTrLengthL) bcTrLengthL _allEntryBPedL
                                  )
                              where bcTrLengthL = map length bcTrL
                      bcodemodtrace
                              = gencVarDeclInitV "GB_ByteCodeModule" _ppNmByteCodeModule
                                       (ppCurlysCommasBlock
                                          (  [ gencStr moduleNm_ ]
                                          ++ (if ehcOptGenTrace _lhsIopts
                                              then [ _ppNmByteCodeEntry, genc $ length allEntryL_ ]
                                              else [ gencNULL, genc "0" ]
                                             )
                                          ++ [ _ppNmByteCodeTbl
                                             , genc _instrsIcLoc
                                             ]
                                       )  )
                      linkChainInds
                              = ppTbl' "GB_LinkChainResolvedInfo" _ppNmLinkChainIndTbl
                                  (map (\(LinkChainResolvedInfo kind info off) -> ppCurlysCommasBlock $ map genc [info, off, fromEnum kind]) $ reverse inds
                                  )
                              where (_,inds) = _instrsIlinkChainResolvedIndInfoSet
                      glEntryMp= Map.fromList [ (i,c) | (LocRef_CodeEntry i,c) <- Seq.toList _instrsIlocRefs ]
                      cafGlEntryIndices = ppTbl' "HalfWord" _ppNmCafGlEntryIndicesTbl [ genc i | i <- cafEntryL_ ]
                      nrCafs = length cafEntryL_
                      glEntries = ppTbl' "GB_BytePtr" _ppNmGlobEntriesTbl [ gencAddrArrayAt _ppNmByteCodeTbl c | c <- Map.elems glEntryMp ]
                      gcStackInfoL = sortOn snd $ Map.toList $ snd _instrsIgcStackInfoMp
                      gcStackInfosTbl = ppTbl' "GCStackInfo" _ppNmGCStackInfosTbl [ mk i | (i,_) <- gcStackInfoL ]
                                where mk (GCStackInfo sz (len,loc)) = ppCurlysCommasBlock [genc sz, genc len, gencAddrArrayAt _ppNmBytePool (bytePoolLocOff loc)]
                      functionInfosTbl = ppTbl' "FunctionInfo" _ppNmFunctionInfoTbl $ map mk $ sortOn snd $ Map.toList $ snd _instrsIfunctionInfoMp
                                where mk (FunctionInfo (_,loc) _ sz flgs,_)
                                        = ppCurlysCommas [ genc (Cfg.sizeofWord * sz)
                                                         , ppListSep "" "" " || " $ maybeNull [FunctionInfoFlag_None] id flgs
                                                         , gencAddrArrayAt _ppNmBytePool (bytePoolLocOff loc)
                                                         ]
                      initf   = ppLine $ ppCmtC _lhsIopts "Initialization"
                                         >-< ppFun ("void" >#< _ppNmInitF >|<
                                                    "(GB_ModEntry* modTbl, Word modTblInx)"
                                                   )
                                                   ("gb_InitTables" >|< ppParensCommasBlock args >#< ";")
                              where args = [ _ppNmByteCodeTbl, pp _instrsIcLoc
                                           , _ppNmCafGlEntryIndicesTbl, genc nrCafs
                                           , _ppNmGlobEntriesTbl, genc (Map.size glEntryMp)
                                           , _ppNmConstTbl
                                           , _ppNmGCStackInfosTbl
                                           , _ppNmLinkChainIndTbl
                                           , _ppNmCallInfoTbl, genc (Map.size $ snd _instrsIcallInfoMp)
                                           , _ppNmFunctionInfoTbl, genc (Map.size $ snd _instrsIfunctionInfoMp)
                                           , _ppNmBytePool
                                           , genc $ maybe 0 id _mbLinkChainCodeLoc
                                           , _ppImpModTbl, genc (length impModNmL_)
                                           , gencAddrOf _ppNmExpNode
                                           , _ppNmExpNodeSz, _ppNmExpNodeOffs
                                           , genc "modTbl", genc "modTblInx"
                                           ]
                      maincaf = ppLine $ gencStatic $ gencVarDeclInit "GB_BytePtr*" _ppNmMainCAFEntry (gencAddrArrayAt _ppNmGlobEntriesTbl mainCafEntry_)
                      mainf   = ppLine $ ppCmtC _lhsIopts "Main entry point"
                                         >-< ppFun "int main(int argc, char** argv)"
                                                   (vlist
                                                    $  [ gencVarDeclInit "int" "nRtsOpts" (0::Int)
                                                       , genc "main_GB_Init1( argc, argv, &nRtsOpts ) ;"
                                                       , gencAssign "gb_Opt_TraceSteps" (ehcOptGenTrace _lhsIopts)
                                                       , gencAssign "gb_Opt_Info" (ehcOptGenRTSInfo _lhsIopts)
                                                       , gencUpdAssign "-" "argc" "nRtsOpts"
                                                       , gencUpdAssign "+" "argv" "nRtsOpts"
                                                       ]
                                                    ++ (if ehcOptGenTrace _lhsIopts then ["IF_GB_TR_ON(3,{gb_prModEntries" >|< ppParens _ppNmModEntriesTbl >#< ";});"] else [])
                                                    ++ [ "GB_MkExpNodeIn(" >#< mkPre m >|< _strNmExpNode >|< ", " >|< mkPre m >|< _strNmExpNodeSz >|< " ) ;"
                                                       | (_,m) <- allImpModNmL_
                                                       ]
                                                    ++ [ mkPre m >|< _strNmInitF >|< ppParensCommas [_ppNmModEntriesTbl,genc minx] >#< ";"
                                                       | (minx,(_,m)) <- zip [0::Int ..] allImpModNmL_
                                                       ]
                                                    ++ [ "gb_SetModTable" >|< ppParensCommas [_ppNmModEntriesTbl,genc $ length allImpModNmL_] >#< ";" ]
                                                    ++ [ "main_GB_Run( argc, argv, gb_code_Eval, Cast(GB_Word,*" >|< _ppNmMainCAFEntry >|< ") ) ;"
                                                       , genc "return main_GB_Exit( argc, argv) ;"
                                                       ]
                                                   )
                      expnd   = ppLine $ gencVarDecl "GB_NodePtr" _ppNmExpNode
                                         >-< (gencStatic $ "int" >#< _ppNmExpNodeOffs >|< "[] =")
                                         >-< ppArr [ eiEntryNr e >#< ppCmtC _lhsIopts (eiNm e) | e <- expEntryL_ ]
                                         >-< gencVarDeclInit "int" _ppNmExpNodeSz (length expEntryL_)
                      externs = nl : map (\(_,m) -> (gencExtern $ gencEndsemic $ "void" >#< mkPre m >|< _strNmInitF >|< "(GB_ModEntry*,Word)")
                                                >-< (gencExtern $ gencVarDecl "GB_NodePtr" (mkPre m >|< _strNmExpNode))
                                                >-< (gencExtern $ gencVarDecl "int" (mkPre m >|< _strNmExpNodeSz))
                                                >-< (gencExtern $ gencVarDecl "GB_ByteCodeModule" (mkPre m >|< _strNmByteCodeModule))
                                         ) allImpModNmL_
                      impmods = ppTbl "GB_ImpModEntry" _ppImpModTbl
                                  ( [ ppCurlysCommasBlock [ gencStr (show n), genc "0" >#< ppCmtC _lhsIopts "filled during linking" ]
                                    | (n,_) <- impModNmL_
                                    ]
                                  )
                      mods    = ppTbl "GB_ModEntry" _ppNmModEntriesTbl
                                  ( [ ppCurlysCommasBlock
                                        [ gencStr (show n)
                                        , gencAddrOf $ mkPre m >|< _strNmExpNode
                                        , gencAddrOf $ mkPre m >|< _strNmByteCodeModule
                                        , _ppNmFunctionInfoTbl
                                        ]
                                    | (n,m) <- allImpModNmL_
                                    ]
                                    ++ [ppCurlysCommasBlock (replicate 4 gencNULL)]
                                  )
                  in  ( vlist
                        $  [ top ]
                        ++ _ccallencWrappers
                        ++ bytePool
                        ++ linkChainInds
                        ++ gcStackInfosTbl
                        ++ functionInfosTbl
                        ++ callinfos
                        ++ bcode
                        ++ (if ehcOptGenTrace _lhsIopts then bcodetrace ++ bcodeentries else [])
                        ++ [bcodemodtrace]
                        ++ consts
                        ++ cfuns
                        ++ cafGlEntryIndices
                        ++ glEntries
                        ++ [ expnd
                           , impmods
                           ]
                        ++ [ initf
                           ]
                      , vlist
                        $  [ maincaf ]
                        ++ externs
                        ++ [ mods
                           , mainf
                           ]
                      )
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 934, column 33)
              (_lhsOpp,_) =
                  __tup36
              -- "build/101/lib-ehc/EH101/GrinByteCode/ToC.ag"(line 934, column 33)
              (_,_lhsOppMain) =
                  __tup36
              -- copy rule (from local)
              _lhsOfunctionInfoExportMp =
                  _functionInfoExportMp
              -- copy rule (from local)
              _instrsOimpNmMp =
                  _impNmMp
              -- copy rule (from local)
              _instrsOlabelLocMp =
                  _labelLocMp
              -- copy rule (from local)
              _instrsOlinkChainResolvedMp =
                  _linkChainResolvedMp
              -- copy rule (from local)
              _instrsOlkupLookupFunctionInfoInx =
                  _lkupLookupFunctionInfoInx
              -- copy rule (down)
              _instrsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _instrsOppNm =
                  _ppNm
              ( _instrsIbcTrL,_instrsIbcode,_instrsIbytePoolMp,_instrsIcLoc,_instrsIcallInfoMp,_instrsIfun2CMp,_instrsIfunctionInfoMp,_instrsIgathLabelLocMp,_instrsIgathLinkChainKeyMp,_instrsIgcStackInfoMp,_instrsIgencL,_instrsIlinkChainResolvedIndInfoSet,_instrsIlocRefs,_instrsIpp,_instrsIppL,_instrsIstringConstMp) =
                  instrs_ _instrsOannL _instrsObytePoolMp _instrsOcLoc _instrsOcallInfoMp _instrsOfunctionInfoMp _instrsOgcStackInfoMp _instrsOimpNmMp _instrsOlabelLocMp _instrsOlinkChainResolvedIndInfoSet _instrsOlinkChainResolvedMp _instrsOlkupLookupFunctionInfoInx _instrsOopts _instrsOppNm _instrsOstringConstMp 
          in  ( _lhsOfunctionInfoExportMp,_lhsOpp,_lhsOppMain)))