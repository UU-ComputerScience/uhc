%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Common} import(UU.Scanner.Position,EH.Util.Utils,{%{EH}Base.HsName},{%{EH}Base.Builtin}) export(module {%{EH}Base.HsName})
%%]

%%[1 export(Assoc,AssocL, ppAssocL)
%%]

%%[1111.exp.hdAndTl export(hdAndTl, hdAndTl')
%%]

%%[1 import(EH.Util.Pretty, Data.List) export(ppListSepFill, ppSpaced, ppAppTop, ppCon, ppCmt)
%%]

%%[1 export(SemApp(..),mkRngProdOpt)
%%]

%%[1 export(assocLElts,assocLKeys)
%%]

%%[1 export(ParNeed(..), ParNeedL, parNeedApp)
%%]

%%[1 export(Fixity(..))
%%]

%%[1 export(UID(..), mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, mkNewLevUID6, mkNewLevUID7, mkNewLevUID8, uidNext, mkNewUID, mkNewUIDL, uidStart, uidNull, uidChild, mkInfNewUIDL)
%%]

%%[1 export(Range(..),emptyRange,builtinRange,mkRange1,mkRange2)
%%]

%%[1 export(assocLMapElt,assocLMapKey)
%%]

%%[1 export(NmLev,nmLevAbsent, nmLevBuiltin, nmLevOutside, nmLevModule)
%%]

%%[1 import(EH.Util.ScanUtils) export(tokMkQName,tokMkQNames,tokMkInt,tokMkStr)
%%]

%%[1.Token hs import(UU.Scanner.Token)
%%]

%%[2 import(qualified Data.Set as Set)
%%]

%%[5 -1.Token hs import({%{EH}Scanner.Token})
%%]

%%[2 export(unions)
%%]

%%[4 export(listCombineUniq)
%%]

%%[4 export(CoContraVariance(..), cocoOpp)
%%]

%%[4 export(FIMode(..),fimOpp,fimSwapCoCo)
%%]

%%[7 export(Seq,mkSeq,unitSeq,concatSeq,"(<+>)",seqToList,emptySeq,concatSeqs,filterSeq)
%%]

%%[7 export(mkNewLevUIDL,mkInfNewLevUIDL)
%%]

%%[7_2 import(qualified Data.Map as Map, Data.Map(Map), Data.Set(Set))
%%]

%%[7_2 export(threadMap,Belowness(..), groupAllBy, mergeListMap)
%%]

%%[7 export(ppFld,mkExtAppPP,mkPPAppFun)
%%]

%%[7 export(uidHNm)
%%]

%%[8888 -(1.exp.hdAndTl 1.Misc.hdAndTl) import (EH.Util.Utils hiding (tr,trp)) export(module EH.Util.Utils)
%%]

%%[8 import (EH.Util.FPath,IO,Char,Data.Maybe,Numeric)
%%]

%%[8 export(putCompileMsg, openFPath,writeToFile, writePP)
%%]

%%[8 export(ppHsnNonAlpha)
%%]

%%[88 export(sortByOn,sortOn,groupOn,groupSortOn)
%%]

%%[8 import (qualified Data.Map as Map) export(showPP,ppPair,ppFM)
%%]

%%[8 export(CTag(..),ctagTag,ctagChar,ctagInt,emptyCTag)
%%]

%%[8 hs export(ctag,ppCTag,ppCTagInt) 
%%]

%%[8 export(CTagsMp)
%%]

%%[8 export(ppUID')
%%]

%%[90 export(groupSortByOn)
%%]

%%[9 export(ppListV,ppAssocLV)
%%]

%%[9 export(PredOccId(..),mkPrId,poiHNm)
%%]

%%[9 export(PrfCtxtId)
%%]

%%[9 export(snd3,thd)
%%]

%%[9 export(basePrfCtxtId)
%%]

%%[20 export(ppCurlysAssocL)
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Printing of names with non-alpha numeric constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
hsnEscapeeChars :: ScanOpts -> Set.Set Char
hsnEscapeeChars scanOpts
  = Set.fromList ('$' : scoSpecChars scanOpts ++ scoOpChars scanOpts)

ppHsnNonAlpha :: ScanOpts -> HsName -> PP_Doc
ppHsnNonAlpha scanOpts
  = p
  where escapeeChars = hsnEscapeeChars scanOpts
        p n = let name = show n
              in  {- if name `elem`  scoKeywordsTxt scanOpts
                   then pp ('$' : '_' : name)
                   else -} 
                        let s = foldr (\c r -> if c `Set.member` escapeeChars then '$':c:r else c:r) [] name
                         in  pp ('$':s)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unique id's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.UID.Base
newtype UID = UID [Int] deriving (Eq,Ord)
%%]

%%[1
instance HSNM UID where
  mkHNm = mkHNm . show
%%]

%%[1.UID.UIDL
type UIDL = [UID]
%%]

%%[2
type UIDS = Set.Set UID
%%]

%%[1.UID.Show
instance Show UID where
  show (UID ls) = concat . intersperse "_" . map show . reverse $ ls
%%]

%%[1.UID.mkNewLevUID
uidNext :: UID -> UID
uidNext (UID (n:ns)) = UID (n+1:ns)

uidChild :: UID -> UID
uidChild (UID ns) = UID (0:ns)

mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u = (uidNext u, uidChild u)
%%]

%%[1
uidStart :: UID
uidStart = UID [0]
%%]

%%[1.UID.Utils
mkNewLevUID2 u = let { (u',u1)          = mkNewLevUID   u; (u'',u2)         = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3)         = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4)      = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4,u5)   = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)
mkNewLevUID6 u = let { (u',u1,u2,u3)    = mkNewLevUID3  u; (u'',u4,u5,u6)   = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6)
mkNewLevUID7 u = let { (u',u1,u2,u3,u4) = mkNewLevUID4  u; (u'',u5,u6,u7)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6,u7)
mkNewLevUID8 u = let { (u',u1,u2,u3,u4) = mkNewLevUID4  u; (u'',u5,u6,u7,u8) = mkNewLevUID4  u'} in (u'',u1,u2,u3,u4,u5,u6,u7,u8)

uidNull :: UID
uidNull  = UID []

mkNewUID :: UID -> (UID,UID)
mkNewUID   uid = (uidNext uid,uid)

mkInfNewUIDL' :: (UID -> (UID,UID)) -> UID -> [UID]
mkInfNewUIDL' mk uid
  =  let  l = iterate (\(nxt,uid) -> mk nxt) . mkNewUID $ uid
     in   map snd l

mkNewUIDL' :: (UID -> (UID,UID)) -> Int -> UID -> [UID] -- assume sz > 0
mkNewUIDL' mk sz uid
  =  take sz (mkInfNewUIDL' mk uid)

mkNewUIDL :: Int -> UID -> [UID] -- assume sz > 0
mkNewUIDL = mkNewUIDL' mkNewUID

mkInfNewUIDL :: UID -> [UID]
mkInfNewUIDL = mkInfNewUIDL' mkNewUID

instance PP UID where
  pp = text . show
%%]

%%[8
ppUID' :: UID -> PP_Doc
ppUID' (UID ls) = ppCurlysCommas ls
%%]

%%[7
uidHNm :: UID -> HsName
uidHNm = hsnFromString . show
%%]

%%[7
mkInfNewLevUIDL :: UID -> [UID]
mkInfNewLevUIDL = mkInfNewUIDL' mkNewLevUID

mkNewLevUIDL :: Int -> UID -> [UID]
mkNewLevUIDL = mkNewUIDL' mkNewLevUID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Proof context id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 hs
type PrfCtxtId = UID
%%]

%%[9
basePrfCtxtId :: PrfCtxtId
basePrfCtxtId = uidStart
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred occurrence id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 hs
data PredOccId
  = PredOccId {poiCxId :: !PrfCtxtId, poiId :: !UID}
  deriving (Show,Eq,Ord)
%%]

%%[9 hs
mkPrId :: PrfCtxtId -> UID -> PredOccId
mkPrId ci u = PredOccId ci u

poiHNm :: PredOccId -> HsName
poiHNm = uidHNm . poiId
%%]

%%[9 export(mkPrIdCHR)
mkPrIdCHR :: UID -> PredOccId
mkPrIdCHR = mkPrId basePrfCtxtId
%%]

%%[9 export(emptyPredOccId)
emptyPredOccId :: PredOccId
emptyPredOccId = mkPrId basePrfCtxtId uidStart
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ordered sequence, 'delayed concat' list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7
newtype Seq a = Seq ([a] -> [a])

emptySeq :: Seq a
emptySeq = Seq id

mkSeq :: [a] -> Seq a
mkSeq l = Seq (l++)

unitSeq :: a -> Seq a
unitSeq e = Seq (e:)

concatSeq :: Seq a -> Seq a -> Seq a
concatSeq (Seq s1) (Seq s2) = Seq (s1.s2)

concatSeqs :: [Seq a] -> Seq a
concatSeqs = foldr (<+>) emptySeq

infixr 5 <+>

(<+>) :: Seq a -> Seq a -> Seq a
(<+>) = concatSeq

seqToList :: Seq a -> [a]
seqToList (Seq s) = s []

instance Functor Seq where
  fmap f = mkSeq . map f . seqToList

filterSeq :: (a -> Bool) -> Seq a -> Seq a
filterSeq p = mkSeq . filter p . seqToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Semantics classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.SemApp
class SemApp a where
  semApp            ::  a -> a -> a
  semAppTop         ::  a -> a
  semCon            ::  (Position n,HSNM n) => n -> a
  semParens         ::  a -> a
  mk1App            ::  a -> a -> a
  mkApp             ::  [a] -> a
  mk1ConApp         ::  (Position n,HSNM n) => n -> a -> a
  mkConApp          ::  (Position n,HSNM n) => n -> [a] -> a
  mkProdApp         ::  [a] -> a
  mk1Arrow          ::  a -> a -> a
  mkArrow           ::  [a] -> a -> a
%%]
%%[1
  mkRngApp          ::  Range -> [a] -> a
  mkRngVar          ::  HSNM n => Range -> n -> a
  mkRngCon          ::  HSNM n => Range -> n -> a
  mkRngProd         ::  Range -> [a] -> a
%%]
%%[1.SemApp.default
  mkApp as          =   case as of  [a]  ->  a
                                    _    ->  semAppTop (foldl1 semApp as)
  mk1App     a r    =   mkApp [a,r]
  mkConApp   c as   =   mkApp (semCon c : as)
  mk1ConApp  c a    =   mkConApp c [a]
  mkProdApp  as     =   mkConApp (hsnProd (length as)) as
  mk1Arrow   a r    =   mkApp [semCon hsnArrow,a,r]
  mkArrow           =   flip (foldr mk1Arrow)
%%]
%%[1
  mkRngApp _        =   mkApp
  mkRngProd _       =   mkProdApp
%%]

%%[1
mkRngProdOpt :: SemApp e => Range -> [e] -> e
mkRngProdOpt r [e] = e
mkRngProdOpt r es  = mkRngProd r es
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.PP.ppAppTop
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) args dflt
  =  if       hsnIsArrow conNm
%%[[9
              || hsnIsPrArrow conNm
%%]]
                                then  ppListSep "" "" (" " >|< con >|< " ") args
     else if  hsnIsProd  conNm  then  ppParensCommas args
%%]
%%[5
     else if  hsnIsList  conNm  then  ppBracketsCommas args
%%]
%%[7
     else if  hsnIsRec   conNm  then  ppListSep (hsnORec >|< con) hsnCRec "," args
     else if  hsnIsSum   conNm  then  ppListSep (hsnOSum >|< con) hsnCSum "," args
     else if  hsnIsRow   conNm  then  ppListSep (hsnORow >|< con) hsnCRow "," args
%%]
%%[1
                                else  dflt
%%]

%%[1.PP.NeededByExpr
ppCon :: HsName -> PP_Doc
ppCon nm =  if    hsnIsProd nm
            then  ppParens (text (replicate (hsnProdArity nm - 1) ','))
            else  pp nm

ppCmt :: PP_Doc -> PP_Doc
ppCmt p = "{-" >#< p >#< "-}"
%%]

-- ppCommaList now in EH.Util lib
ppCommaList :: PP a => [a] -> PP_Doc
ppCommaList = ppListSep "[" "]" ","
%%[1.PP.Rest

ppSpaced :: PP a => [a] -> PP_Doc
ppSpaced = ppListSep "" "" " "

ppListSepFill :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepFill o c s pps
  = l pps
  where l []      = o >|< c
        l [p]     = o >|< pp p >|< c
        l (p:ps)  = fill ((o >|< pp p) : map (s >|<) ps) >|< c
%%]

%%[7
ppFld :: String -> Maybe HsName -> HsName -> PP_Doc -> PP_Doc -> PP_Doc
ppFld sep positionalNm nm nmPP f
  = case positionalNm of
      Just pn | pn == nm -> f
      _                  -> nmPP >#< sep >#< f

mkPPAppFun :: HsName -> PP_Doc -> PP_Doc
mkPPAppFun c p = if c == hsnRowEmpty then empty else p >|< "|"

mkExtAppPP :: (HsName,PP_Doc,[PP_Doc]) -> (HsName,PP_Doc,[PP_Doc],PP_Doc) -> (PP_Doc,[PP_Doc])
mkExtAppPP (funNm,funNmPP,funPPL) (argNm,argNmPP,argPPL,argPP)
  =  if hsnIsRec funNm || hsnIsSum funNm
     then (mkPPAppFun argNm argNmPP,argPPL)
     else (funNmPP,funPPL ++ [argPP])
%%]

%%[9
instance (PP a, PP b) => PP (a,b) where
  pp (a,b) = ppParensCommas' [pp a,pp b]
%%]

%%[8
ppPair :: (PP a, PP b) => (a,b) -> PP_Doc
ppPair (x,y) = ppParens (pp x >|< "," >|< pp y)
%%]

%%[8
showPP :: PP a => a -> String
showPP x = disp (pp x) 100 ""
%%]

%%[8
ppFM :: (PP k,PP v) => Map.Map k v -> PP_Doc
ppFM = ppAssocL . Map.toList
%%]

%%[9
ppListV :: PP a => [a] -> PP_Doc
ppListV = vlist . map pp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Putting stuff on output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
putCompileMsg :: Verbosity -> Verbosity -> String -> Maybe String -> HsName -> FPath -> IO ()
putCompileMsg v optsVerbosity msg mbMsg2 modNm fNm
  = if optsVerbosity >= v
    then do { hPutStrLn stdout (strBlankPad 30 msg ++ " " ++ strBlankPad 22 (show modNm) ++ " (" ++ fpathToStr fNm ++ maybe "" (\m -> ", " ++ m) mbMsg2 ++ ")")
            ; hFlush stdout
            }
    else return ()
%%]

%%[8
openFPath :: FPath -> IOMode -> IO (String, Handle)
openFPath fp mode | fpathIsEmpty fp = case mode of
                                        ReadMode      -> return ("<stdin>" ,stdin )
                                        WriteMode     -> return ("<stdout>",stdout)
                                        AppendMode    -> return ("<stdout>",stdout)
                                        ReadWriteMode -> error "cannot use stdin/stdout with random access"
                  | otherwise       = do
                                        let fNm = fpathToStr fp
                                        h <- openFile fNm mode
                                        return (fNm,h)

writePP ::  (a -> PP_Doc) -> a -> FPath -> IO ()
writePP f text fp = writeToFile (show.f $ text) fp

writeToFile str fp
  = do { (fn, fh) <- openFPath fp WriteMode
       ; hPutStrLn fh str
       ; hClose fh
       }

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prio computation for need of parenthesis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ParNeed
data ParNeed =  ParNotNeeded | ParNeededLow | ParNeeded | ParNeededHigh | ParOverrideNeeded
                deriving (Eq,Ord)

type ParNeedL = [ParNeed]

parNeedApp :: HsName -> (ParNeed,ParNeedL)
parNeedApp conNm
  =  let  pr  | hsnIsArrow  conNm   =  (ParNeededLow,[ParNotNeeded,ParNeeded])
              | hsnIsProd   conNm   =  (ParOverrideNeeded,repeat ParNotNeeded)
%%[[5
              | hsnIsList   conNm   =  (ParOverrideNeeded,[ParNotNeeded])
%%]]
%%[[7
              | hsnIsRec    conNm   =  (ParOverrideNeeded,[ParNotNeeded])
              | hsnIsSum    conNm   =  (ParOverrideNeeded,[ParNotNeeded])
              | hsnIsRow    conNm   =  (ParOverrideNeeded,repeat ParNotNeeded)
%%]]
              | otherwise           =  (ParNeeded,repeat ParNeededHigh)
     in   pr
%%]

%%[1 export(ppParNeed)
ppParNeed :: PP p => ParNeed -> ParNeed -> p -> PP_Doc
ppParNeed locNeed globNeed p
  = par (pp p)
  where par = if globNeed > locNeed then ppParens else id
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Co/Contra variance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.CoContraVariance
data CoContraVariance =  CoVariant | ContraVariant | CoContraVariant deriving (Show,Eq)
%%]

%%[4
instance PP CoContraVariance where
  pp CoVariant        = pp "CC+"
  pp ContraVariant    = pp "CC-"
  pp CoContraVariant  = pp "CCo"
%%]

%%[4.cocoOpp
cocoOpp :: CoContraVariance -> CoContraVariance
cocoOpp  CoVariant      =   ContraVariant
cocoOpp  ContraVariant  =   CoVariant
cocoOpp  _              =   CoContraVariant
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Belowness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7_2
data Belowness = Below | NotBelow | UnknownBelow deriving (Show,Eq,Ord)
%%]

%%[7_2
instance PP Belowness where
  pp Below        = pp "B+"
  pp NotBelow     = pp "B-"
  pp UnknownBelow = pp "B?"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tags (of data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs
data CTag
  = CTagRec
  | CTag {ctagTyNm :: !HsName, ctagNm :: !HsName, ctagTag' :: !Int, ctagArity :: !Int, ctagMaxArity :: !Int}
  deriving (Show,Eq,Ord)

ctagTag :: CTag -> Int
ctagTag CTagRec = 0
ctagTag t       = ctagTag' t

ctagInt  =  CTag hsnInt  hsnInt  0 1 1
ctagChar =  CTag hsnChar hsnChar 0 1 1

emptyCTag = CTag hsnUnknown hsnUnknown 0 0 0
%%]

%%[9 export(mkClassCTag)
-- only used when `not ehcCfgClassViaRec'
mkClassCTag :: HsName -> Int -> CTag
mkClassCTag n sz = CTag n n 0 sz sz
%%]

%%[8 hs
ctag :: a -> (HsName -> HsName -> Int -> Int -> Int -> a) -> CTag -> a
ctag n t tg = case tg of {CTag tn cn i a ma -> t tn cn i a ma; _ -> n}

ppCTag :: CTag -> PP_Doc
ppCTag = ctag (pp "Rec") (\tn cn t a ma -> pp t >|< "/" >|< pp cn >|< "/" >|< pp a >|< "/" >|< pp ma)

ppCTagInt :: CTag -> PP_Doc
ppCTagInt = ctag (pp "-1") (\_ _ t _ _ -> pp t)

instance PP CTag where
  pp = ppCTag
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unboxed values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(Unbox(..))
data Unbox
  = Unbox_FirstField
  | Unbox_Tag         !Int
  | Unbox_None
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc info passed to backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs
type CTagsMp = AssocL HsName (AssocL HsName CTag)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AssocL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AssocL
type Assoc k v = (k,v)
type AssocL k v = [Assoc k v]
%%]

%%[1.ppAssocL
ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL al = ppListSepFill "[ " " ]" ", " (map (\(k,v) -> pp k >|< ":" >|< pp v) al)
%%]

%%[9.ppAssocL -1.ppAssocL
ppAssocL' :: (PP k, PP v) => ([PP_Doc] -> PP_Doc) -> AssocL k v -> PP_Doc
ppAssocL' ppL al = ppL (map (\(k,v) -> pp k >|< ":" >|< pp v) al)

ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL = ppAssocL' (ppBlock "[" "]" ",")

ppAssocLV :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocLV = ppAssocL' vlist
%%]

%%[20
-- intended for parsing
ppCurlysAssocL :: (k -> PP_Doc) -> (v -> PP_Doc) -> AssocL k v -> PP_Doc
ppCurlysAssocL pk pv = ppCurlysCommasBlock . map (\(k,v) -> pk k >#< "=" >#< pv v)
%%]

%%[1
assocLMap :: (k -> v -> (k',v')) -> AssocL k v -> AssocL k' v'
assocLMap f = map (uncurry f)

assocLMapElt :: (v -> v') -> AssocL k v -> AssocL k v'
assocLMapElt f = assocLMap (\k v -> (k,f v))

assocLMapKey :: (k -> k') -> AssocL k v -> AssocL k' v
assocLMapKey f = assocLMap (\k v -> (f k,v))
%%]

%%[4 export(assocLMapUnzip)
assocLMapUnzip :: AssocL k (v1,v2) -> (AssocL k v1,AssocL k v2)
assocLMapUnzip l = unzip [ ((k,v1),(k,v2)) | (k,(v1,v2)) <- l ]
%%]

%%[1
assocLKeys :: AssocL k v -> [k]
assocLKeys = map fst

assocLElts :: AssocL k v -> [v]
assocLElts = map snd
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting mode (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIMode
data FIMode  =  FitSubLR
             |  FitSubRL
             |  FitUnify
%%]
%%[4_2
             |  FitMeet
             |  FitJoin
%%]
%%[4
             deriving (Eq,Ord)
%%]

%%[4
fimOpp :: FIMode -> FIMode
fimOpp m
  =  case m of
       FitSubLR  -> FitSubRL
       FitSubRL  -> FitSubLR
%%]
%%[4_2
       FitMeet   -> FitJoin
       FitJoin   -> FitMeet
%%]
%%[4
       _         -> m
%%]

%%[4
fimSwapCoCo :: CoContraVariance -> FIMode -> FIMode
fimSwapCoCo coco m = case coco of {ContraVariant -> fimOpp m; _ -> m}
%%]

%%[4
instance Show FIMode where
  show FitSubLR  = "<="
  show FitSubRL  = ">="
  show FitUnify  = "=="
%%]
%%[4_2
  show FitMeet   = "=^="
  show FitJoin   = "=v="
%%]

%%[4
instance PP FIMode where
  pp m = pp (show m)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List related, should move in time to general library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.unions
unions :: Eq a => [[a]] -> [a]
unions = foldr union []
%%]

%%[4.listCombineUniq
listCombineUniq :: Eq a => [[a]] -> [a]
listCombineUniq = nub . concat
%%]

%%[7_2
threadMap :: (a -> c -> (b, c)) -> c -> [a] -> ([b], c)
threadMap f c = foldr (\a (bs, c) -> let (b, c') = f a c in (b:bs, c')) ([], c)
%%]

%%[7_2
groupAllBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupAllBy f = Map.elems . foldr (\v -> Map.insertWith (++) (f v) [v]) Map.empty 
%%]

%%[7_2
mergeListMap :: Ord k => Map k [a] -> Map k [a] -> Map k [a]
mergeListMap = Map.unionWith (++)
%%]

%%[90
groupByOn :: (b -> b -> Bool) -> (a -> b) -> [a] -> [[a]]
groupByOn eq sel = groupBy (\e1 e2 -> sel e1 `eq` sel e2)

groupSortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [[a]]
groupSortByOn cmp sel = groupByOn (\e1 e2 -> cmp e1 e2 == EQ) sel . sortByOn cmp sel
%%]

%%[8 export(replicateBy)
replicateBy :: [a] -> b -> [b]
replicateBy l e = replicate (length l) e
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(strBlankPad)
strBlankPad :: Int -> String -> String
strBlankPad n s = s ++ replicate (n - length s) ' '
%%]

%%[9
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd :: (a,b,c) -> c
thd (a,b,c) = c
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Verbosity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Verbosity(..))
data Verbosity
  = VerboseQuiet | VerboseNormal | VerboseALot | VerboseDebug
  deriving (Eq,Ord)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR scoped translation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRScoped(..))
data CHRScoped
  = CHRScopedInstOnly | CHRScopedMutualSuper | CHRScopedAll
  deriving (Eq,Ord)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimisation level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Optimise(..))
data Optimise
  = OptimiseNone | OptimiseNormal | OptimiseALot
  deriving (Eq,Ord,Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Point in sequence of EH compilation phases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(CompilePoint(..))
data CompilePoint
  = CompilePoint_Imports
  | CompilePoint_Parse
  | CompilePoint_AnalHS
  | CompilePoint_AnalEH
%%[[8
  | CompilePoint_Core
%%]]
  | CompilePoint_All
  deriving (Eq,Ord,Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
data Fixity
  = Fixity_Infix | Fixity_Infixr | Fixity_Infixl
  deriving (Eq,Ord,Show)

instance PP Fixity where
  pp Fixity_Infix  = pp "infix"
  pp Fixity_Infixl = pp "infixl"
  pp Fixity_Infixr = pp "infixr"
%%]

%%[1 export(fixityMaxPrio)
fixityMaxPrio :: Int
fixityMaxPrio = 9
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Eq,Ord for Pos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
instance Eq Pos where
  p1 == p2 = line p1 == line p2 && column p2 == column p2

instance Ord Pos where
  compare p1 p2
    = case compare (line p1) (line p2) of
        EQ -> compare (column p1) (column p2)
        c  -> c
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Range
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
data Range
  = Range_Range    !Pos !Pos
  | Range_Unknown
  | Range_Builtin

emptyRange :: Range
emptyRange = Range_Unknown

builtinRange :: Range
builtinRange = Range_Builtin

mkPos :: Position p => p -> Pos
mkPos p = Pos (line p) (column p) (file p)

mkRange1 :: Position p => p -> Range
mkRange1 p = Range_Range (mkPos p) noPos

mkRange2 :: Position p => p -> p -> Range
mkRange2 p1 p2 = Range_Range (mkPos p1) (mkPos p2)
%%]

%%[1
instance Show Range where
  show (Range_Range p _) = show p
  show Range_Unknown     = "??"
  show Range_Builtin     = "builtin"

instance PP Range where
  pp (Range_Range p _) = pp p
  pp r                 = pp $ show r
%%]

%%[1
rngAdd :: Range -> Range -> Range
rngAdd r1 r2
  = case (r1,r2) of
      (Range_Range l1 h1,Range_Range l2 h2)
        -> Range_Range (l1 `min` l2) (h1 `max` h2)
      (Range_Range _ _,_)
        -> r1
      (_,Range_Range _ _)
        -> r2
      _ -> Range_Unknown
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting of Range
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.rngLift export(rngLift,RngLift)
type RngLift x = x

rngLift :: Range -> v -> v
rngLift r v = v
%%]

%%[99 -1.rngLift export(rngLift,RngLift)
type RngLift x = Range -> x

rngLift :: Range -> (Range -> v) -> v
rngLift r mkv
  = x `seq` x
  where x = mkv r
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instance variant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(InstVariant(..))
data InstVariant
  = InstNormal | InstDefault
%%[[95
  | InstDeriving
%%]]
  deriving (Eq,Ord,Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Levels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs
type NmLev = Int

nmLevAbsent, nmLevBuiltin, nmLevOutside, nmLevModule :: NmLev
nmLevAbsent  = -3
nmLevBuiltin = -2
nmLevOutside = -1
nmLevModule  =  0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Token related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs
-- Assumption: tokTpIsInt (genTokTp t) == True
tokMkInt :: Token -> Int
tokMkInt t
  = case genTokTp t of
      Just TkInteger10 -> read v
      _                -> 0
  where v = genTokVal t

tokMkStr :: Token -> String
tokMkStr = genTokVal
%%]

%%[1.tokMkQName hs
tokMkQName :: Token -> HsName
tokMkQName = hsnFromString . genTokVal
%%]

%%[7 -1.tokMkQName hs
tokMkQName :: Token -> HsName
tokMkQName t
  = case genTokTp t of
      Just tp | tokTpIsInt tp -> HNPos $ tokMkInt t
      _                       -> mkHNm $ genTokVal t
%%]

%%[1 hs
tokMkQNames :: [Token] -> [HsName]
tokMkQNames = map tokMkQName

instance HSNM Token where
  mkHNm = tokMkQName
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name supply, with/without uniqueness required
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(hsnUniqSupply,hsnLclSupply,hsnLclSupplyWith)
hsnUniqSupply :: UID -> [HsName]
hsnUniqSupply = map uidHNm . iterate uidNext

hsnLclSupplyWith :: HsName -> [HsName]
hsnLclSupplyWith n = map (\i -> hsnSuffix n $ "_" ++ show i) [1..]

hsnLclSupply :: [HsName]
hsnLclSupply = hsnLclSupplyWith (hsnFromString "")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hex printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(strHex)
strHex :: Integral a => Int -> a -> String
strHex prec x
  = replicate (prec - length h) '0' ++ h
  where h = showHex x []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Backend(..))
data Backend
  = BackendGrinByteCode
  | BackendSilly
  deriving (Eq, Ord)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance ForceEval Fixity
%%[[101
  where
    fevCount x | x `seq` True = cm1 "Fixity"
%%]]

instance ForceEval HsName where
  forceEval x@(HNm     s) | forceEval s `seq` True = x
  forceEval x@(HNmNr _ n) | forceEval n `seq` True = x
  forceEval x@(HNmQ    l) | forceEval l `seq` True = x
  forceEval x                                      = x
%%[[101
  fevCount (HNm     s) = cm1 "HNm"   `cmUnion` fevCount s
  fevCount (HNmNr i n) = cm1 "HNmNr" `cmUnion` fevCount n
  fevCount (HNmQ    l) = cm1 "HNmQ"  `cmUnion` fevCount l
  fevCount (HNPos   p) = cm1 "HNPos" `cmUnion` fevCount p
%%]]

instance ForceEval CTag where
  forceEval x@(CTag tn n t a ma) | forceEval tn `seq` forceEval n `seq` True = x
  forceEval x = x
%%[[101
  fevCount (CTag tn n t a ma) = cmUnions [cm1 "CTag",fevCount tn,fevCount n,fevCount t,fevCount a,fevCount ma]
  fevCount CTagRec            = cm1 "CTagRec"
%%]]

instance ForceEval Range where
  forceEval x@(Range_Range b e) | forceEval b `seq` forceEval e `seq` True = x
  forceEval x = x
%%[[101
  fevCount (Range_Range b e) = cm1 "Range_Range" `cmUnion` fevCount b `cmUnion` fevCount e
  fevCount Range_Unknown     = cm1 "Range_Unknown"
  fevCount Range_Builtin     = cm1 "Range_Builtin"
%%]]

instance ForceEval Pos where
  forceEval x@(Pos l c f) | forceEval l `seq` forceEval c `seq` forceEval f `seq` True = x
%%[[101
  fevCount (Pos l c f) = cm1 "Pos" `cmUnion` fevCount l `cmUnion` fevCount c `cmUnion` fevCount f
%%]]

instance ForceEval IdOcc
%%[[101
  where
    fevCount (IdOcc x y) = cm1 "IdOcc" `cmUnion` fevCount x `cmUnion` fevCount y
%%]]

%%[[101
instance ForceEval IdOccKind where
  fevCount x | x `seq` True = cm1 "IdOccKind_*"
%%]]

instance ForceEval UID where
  forceEval x@(UID l) | forceEval l `seq` True = x
%%[[101
  fevCount (UID l) = cm1 "UID" `cmUnion` fevCount l
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fake AG dependency: first param is not used, only introduces an AG dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(agFakeDependOn)
agFakeDependOn :: a -> b -> b
agFakeDependOn _ x = x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable name (used by CHR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(VarUIDHsName(..),vunmNm)
data VarUIDHsName
  = VarUIDHs_Name		{ vunmId :: !UID, vunmNm' :: !HsName }
  | VarUIDHs_UID		{ vunmId :: !UID }
  | VarUIDHs_Var		!UID
  deriving (Eq, Ord)

vunmNm :: VarUIDHsName -> HsName
vunmNm (VarUIDHs_Name _ n) = n
vunmNm (VarUIDHs_UID  i  ) = mkHNm i
vunmNm _                   = panic "Common.assnmNm"
%%]

%%[9 export(vunmMbVar)
vunmMbVar :: VarUIDHsName -> Maybe UID
vunmMbVar (VarUIDHs_Var v) = Just v
vunmMbVar _                = Nothing
%%]

%%[9
instance Show VarUIDHsName where
  show (VarUIDHs_Name _ n) = show n
  show (VarUIDHs_UID  i  ) = show i
  show (VarUIDHs_Var  i  ) = show i

instance PP VarUIDHsName where
  pp a = pp $ show a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic lookup wrapper checking for cycles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2 hs
withLkupLiftCyc2 :: (t -> Maybe UID) -> (UID -> Maybe t) -> x -> (UIDS -> t -> x) -> (t -> x) -> UIDS -> UID -> x
withLkupLiftCyc2 get lookup dflt yes no vsVisited v
  = case lookup v of
      Just t | not (v `Set.member` vsVisited)
        -> yes (Set.insert v vsVisited) t
      _ -> dflt
%%]

%%[2 hs export(withLkupLiftCyc1,withLkupLift)
withLkupLiftCyc1 :: (t -> Maybe UID) -> (UID -> Maybe t) -> (UIDS -> t -> x) -> (t -> x) -> UIDS -> t -> x
withLkupLiftCyc1 get lookup yes no vsVisited t
  = maybe dflt (withLkupLiftCyc2 get lookup dflt yes no vsVisited) $ get t
  where dflt = no t

withLkupLift :: (t -> Maybe UID) -> (UID -> Maybe t) -> (t -> x) -> (t -> x) -> t -> x
withLkupLift get lookup yes no t
  = withLkupLiftCyc1 get lookup (\_ t -> yes t) no Set.empty t
%%]

%%[2 hs
lookupLiftCyc1 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x' -> (x->x') -> x -> x'
lookupLiftCyc1 get lookup dflt found x
  = lk Set.empty dflt found x
  where lk s dflt found x = withLkupLiftCyc1 get lookup (\s t -> lk s (found t) found t) (const dflt) s x

lookupLiftCyc2 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x' -> (x->x') -> UID -> x'
lookupLiftCyc2 get lookup dflt found x
  = maybe dflt (\x -> lookupLiftCyc1 get lookup (found x) found x) $ lookup x
%%]

%%[2 export(lookupLiftCycMb1,lookupLiftCycMb2)
lookupLiftCycMb1 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x -> Maybe x
lookupLiftCycMb1 get lookup x = lookupLiftCyc1 get lookup Nothing Just x

lookupLiftCycMb2 :: (x -> Maybe UID) -> (UID -> Maybe x) -> UID -> Maybe x
lookupLiftCycMb2 get lookup x = lookupLiftCyc2 get lookup Nothing Just x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Presence of something (just a boolean with meaning)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(Presence(..))
data Presence = Present | Absent deriving (Eq,Ord,Show)
%%]


