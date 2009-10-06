%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Common} import(UU.Scanner.Position,EH.Util.Utils,{%{EH}Base.HsName},{%{EH}Base.Builtin}) export(module {%{EH}Base.HsName})
%%]

%%[1 export(Assoc,AssocL)
%%]

%%[1111.exp.hdAndTl export(hdAndTl, hdAndTl')
%%]

%%[1 import(EH.Util.Pretty, Data.List) export(ppListSepFill, ppSpaced, ppCon, ppCmt)
%%]

%%[1 export(assocLElts,assocLKeys)
%%]

%%[1 export(ParNeed(..), ParNeedL, parNeedApp)
%%]

%%[1 export(Fixity(..))
%%]

%%[1 export(mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, mkNewLevUID6, mkNewLevUID7, mkNewLevUID8, uidNext, mkNewUID, mkNewUIDL, uidStart, uidNull, uidChild, mkInfNewUIDL)
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

%%[7777 export(Seq,mkSeq,unitSeq,concatSeq,"(<+>)",seqToList,emptySeq,concatSeqs,filterSeq)
%%]

%%[7 export(mkNewLevUIDL,mkInfNewLevUIDL)
%%]

%%[7_2 import(qualified Data.Map as Map, Data.Map(Map), Data.Set(Set))
%%]

%%[7_2 export(threadMap,Belowness(..), groupAllBy, mergeListMap)
%%]

%%[7 export(uidHNm, uidQualHNm)
%%]

%%[8888 -(1.exp.hdAndTl 1.Misc.hdAndTl) import (EH.Util.Utils hiding (tr,trp)) export(module EH.Util.Utils)
%%]

%%[8 import (EH.Util.FPath,IO,Char,Data.Maybe,Numeric)
%%]

%%[8 export(putCompileMsg)
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

%%[9 export(ppListV)
%%]

%%[9 export(snd3,thd)
%%]

%%[20 export(ppCurlysAssocL)
%%]

%%[99 import({%{EH}Base.Hashable})
%%]
%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Printing of names with non-alpha numeric constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance Show (Ptr a) where
   showsPrec _ (Ptr a) rs = pad_out (showHex (addrToInteger a) "")
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ls = 
          '0':'x':(replicate (2*SIZEOF_HSPTR - length ls) '0') ++ ls ++ rs


%%[8 export(ppHsnNonAlpha,ppHsnEscaped,hsnEscapeeChars)
ppHsnEscaped :: Either Char (Set.Set Char) -> Char -> Set.Set Char -> HsName -> PP_Doc
ppHsnEscaped first escChar escapeeChars
  = \n -> let (nh:nt) = show n
          in  pp $ hd ++ chkhd nh ++ (concatMap esc nt)
  where (hd,chkhd) = either (\c -> ([c],(:""))) (\chs -> ("",\h -> if Set.member h chs then [escChar,h] else esc h)) first
        escapeeChars' = Set.unions [escapeeChars, Set.fromList [escChar]]
        hexChars      = Set.fromList $ ['\NUL'..' '] ++ "\t\r\n"
        esc c | Set.member c escapeeChars' = [escChar,c]
              | Set.member c hexChars      = [escChar,'x'] ++ pad_out (showHex (ord c) "")
              | otherwise                  = [c]
        pad_out ls = (replicate (2 - length ls) '0') ++ ls

hsnEscapeeChars :: Char -> ScanOpts -> Set.Set Char
hsnEscapeeChars escChar scanOpts
  = Set.fromList [escChar] `Set.union` scoSpecChars scanOpts `Set.union` scoOpChars scanOpts

ppHsnNonAlpha :: ScanOpts -> HsName -> PP_Doc
ppHsnNonAlpha scanOpts
  = p
  where escapeeChars = hsnEscapeeChars '$' scanOpts
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

%%[1.UID.Base export(UID)
%%[[1
newtype UID = UID { uidInts :: [Int] }
%%][99
data UID = UID { uidHash :: !Hash, uidInts :: [Int] }
%%]]
  deriving (Eq,Ord)
%%]

%%[1 export(mkUID)
mkUID :: [Int] -> UID
%%[[1
mkUID is = UID is
%%][99
mkUID is = UID (hashList is) is
%%]]
%%]

%%[1
instance HSNM UID where
  mkHNm = mkHNm . show
%%]

%%[99
instance Hashable UID where
  hash = uidHash
%%]

%%[1.UID.UIDL
type UIDL = [UID]
%%]

%%[2 export(UIDS)
type UIDS = Set.Set UID
%%]

%%[1.UID.Show
instance Show UID where
  show uid = concat . intersperse "_" . map show . reverse $ uidInts uid
%%]

%%[1.UID.mkNewLevUID
uidNext :: UID -> UID
%%[[1
uidNext (UID   (n:ns)) = mkUID (n+1:ns)
%%][99
uidNext (UID _ (n:ns)) = mkUID (n+1:ns)
%%]]

uidChild :: UID -> UID
%%[[1
uidChild (UID   ns) = mkUID (0:ns)
%%][99
uidChild (UID _ ns) = mkUID (0:ns)
%%]]
%%]

%%[1.UID.mkNewLevUID
mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u = (uidNext u, uidChild u)
%%]

%%[1 export(uidFromInt)
uidFromInt :: Int -> UID
uidFromInt i = mkUID [i]
%%]

%%[1
uidStart :: UID
uidStart = uidFromInt 0
%%]

%%[1.UID.Utils
mkNewLevUID2 u = let { (u',u1)          = mkNewLevUID   u; (u'',u2)          = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3)          = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4)       = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4,u5)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)
mkNewLevUID6 u = let { (u',u1,u2,u3)    = mkNewLevUID3  u; (u'',u4,u5,u6)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6)
mkNewLevUID7 u = let { (u',u1,u2,u3,u4) = mkNewLevUID4  u; (u'',u5,u6,u7)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6,u7)
mkNewLevUID8 u = let { (u',u1,u2,u3,u4) = mkNewLevUID4  u; (u'',u5,u6,u7,u8) = mkNewLevUID4  u'} in (u'',u1,u2,u3,u4,u5,u6,u7,u8)

uidNull :: UID
uidNull  = mkUID []

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
ppUID' uid = ppCurlysCommas $ uidInts uid
%%]

%%[7
uidHNm :: UID -> HsName
uidHNm = hsnFromString . show
%%]

%%[7
uidQualHNm :: HsName -> UID -> HsName
uidQualHNm modnm uid = 
%%[[20                  
                        hsnPrefixQual modnm $
%%]]
                        uidHNm uid
%%]



%%[7
mkInfNewLevUIDL :: UID -> [UID]
mkInfNewLevUIDL = mkInfNewUIDL' mkNewLevUID

mkNewLevUIDL :: Int -> UID -> [UID]
mkNewLevUIDL = mkNewUIDL' mkNewLevUID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to unique mechanism of AG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(nextUnique)
nextUnique = mkNewLevUID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred occurrence id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 hs export(PredOccId(..))
newtype PredOccId
  = PredOccId
      { poiId    	:: UID
      }
  deriving (Show,Eq,Ord)
%%]

%%[9 hs export(mkPrId,poiHNm)
mkPrId :: UID -> PredOccId
mkPrId u = PredOccId u

poiHNm :: PredOccId -> HsName
poiHNm = uidHNm . poiId
%%]

%%[9 export(mkPrIdCHR)
mkPrIdCHR :: UID -> PredOccId
mkPrIdCHR = mkPrId
%%]

%%[9 export(emptyPredOccId)
emptyPredOccId :: PredOccId
emptyPredOccId = mkPrId uidStart
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ordered sequence, 'delayed concat' list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7
%%]
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Semantics classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.SemApp export(SemApp(..))
%%[[SemAppCore1
class SemApp a where
  -- basic semantics
  semApp            ::  a -> a -> a
  semAppTop         ::  a -> a
  semVar            ::  (Position n,HSNM n) => n -> a
  semCon            ::  (Position n,HSNM n) => n -> a
  semParens         ::  a -> a
%%]]
  -- basic semantics with Range
  semRngApp         ::  Range -> a -> a -> a
  semRngAppTop      ::  Range -> a -> a
  semRngVar         ::  (Position n,HSNM n) => Range -> n -> a
  semRngCon         ::  (Position n,HSNM n) => Range -> n -> a
  semRngParens      ::  Range -> a -> a
  -- constructing
%%[[SemAppCore2
  mkApp             ::  [a] -> a
  mkConApp          ::  (Position n,HSNM n) => n -> [a] -> a
  mkProdApp         ::  [a] -> a
  mk1Arrow          ::  a -> a -> a
  mkArrow           ::  [a] -> a -> a
%%]]
  mk1App            ::  a -> a -> a
  mk1ConApp         ::  (Position n,HSNM n) => n -> a -> a
  -- constructin with Range
  mk1RngApp         ::  Range -> a -> a -> a
  mkRngApp          ::  Range -> [a] -> a
  mkRngProd         ::  Range -> [a] -> a

  -- defaults semantics
  semApp            =   semRngApp    emptyRange
  semAppTop         =   semRngAppTop emptyRange
  semVar            =   semRngVar    emptyRange
  semCon            =   semRngCon    emptyRange
  semParens         =   semRngParens emptyRange
  semRngApp    _    =   semApp   
  semRngAppTop _    =   semAppTop
  semRngVar    _    =   semVar   
  semRngCon    _    =   semCon   
  semRngParens _    =   semParens
  -- defaults
  mkApp             =   mkRngApp emptyRange
  mk1App     a r    =   mkApp [a,r]
  mkConApp   c as   =   mkApp (semCon c : as)
  mk1ConApp  c a    =   mkConApp c [a]
  mkProdApp  as     =   mkConApp (hsnProd (length as)) as
  mk1Arrow   a r    =   mkApp [semCon hsnArrow,a,r]
  mkArrow           =   flip (foldr mk1Arrow)
  -- defaults with Range
  mkRngProd rng     =   mkProdApp				-- to be done
  mk1RngApp rng a r =   mkRngApp rng [a,r]
  mkRngApp  rng as  =   case as of
                          [a] -> a
                          _   -> semRngAppTop rng (foldl1 (semRngApp rng) as)
%%]

%%[1 export(mkRngProdOpt)
mkRngProdOpt :: SemApp e => Range -> [e] -> e
mkRngProdOpt r [e] = e
mkRngProdOpt r es  = mkRngProd r es
%%]

%%[1 export(mkRngParApp)
mkRngParApp :: SemApp e => Range -> [e] -> e
mkRngParApp r [a] = a
mkRngParApp r as  = semRngParens r (mkRngApp r as)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.PP.ppAppTop export(ppAppTop)
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) argL dflt
  =  if       hsnIsArrow conNm
%%[[9
              || hsnIsPrArrow conNm
%%]]
                                then  ppListSep "" "" (" " >|< con >|< " ") argL
     else if  hsnIsProd  conNm  then  ppParensCommas argL
%%[[5
     else if  hsnIsList  conNm  then  ppBracketsCommas argL
%%]]
%%[[7
     else if  hsnIsRec   conNm  then  ppListSep (hsnORec >|< con) hsnCRec "," argL
     else if  hsnIsSum   conNm  then  ppListSep (hsnOSum >|< con) hsnCSum "," argL
     else if  hsnIsRow   conNm  then  ppListSep (hsnORow >|< con) hsnCRow "," argL
%%]]
                                else  dflt
%%]

%%[99 export(ppAppTop')
ppAppTop' :: PP arg => (HsName,arg) -> [arg] -> [Bool] -> PP_Doc -> PP_Doc
ppAppTop' cc@(conNm,_) [_,a] [True,_] _ | hsnIsArrow conNm || hsnIsPrArrow conNm    = pp a
ppAppTop' cc argL _ dflt                                                            = ppAppTop cc argL dflt
%%]

%%[1.PP.NeededByExpr
ppCon :: HsName -> PP_Doc
ppCon nm =  if    hsnIsProd nm
            then  ppParens (text (replicate (hsnProdArity nm - 1) ','))
            else  pp nm

ppCmt :: PP_Doc -> PP_Doc
ppCmt p = "{-" >#< p >#< "-}"
%%]

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

%%[7 export(ppFld,mkPPAppFun,mkPPAppFun')
ppFld :: String -> Maybe HsName -> HsName -> PP_Doc -> PP_Doc -> PP_Doc
ppFld sep positionalNm nm nmPP f
  = case positionalNm of
      Just pn | pn == nm -> f
      _                  -> nmPP >#< sep >#< f

mkPPAppFun' :: String -> HsName -> PP_Doc -> PP_Doc
mkPPAppFun' sep c p = if c == hsnRowEmpty then empty else p >|< sep

mkPPAppFun :: HsName -> PP_Doc -> PP_Doc
mkPPAppFun = mkPPAppFun' "|"
%%]

%%[7 export(mkExtAppPP,mkExtAppPP')
mkExtAppPP' :: String -> (HsName,PP_Doc,[PP_Doc]) -> (HsName,PP_Doc,[PP_Doc],PP_Doc) -> (PP_Doc,[PP_Doc])
mkExtAppPP' sep (funNm,funNmPP,funPPL) (argNm,argNmPP,argPPL,argPP)
  =  if hsnIsRec funNm || hsnIsSum funNm
     then (mkPPAppFun' sep argNm argNmPP,argPPL)
     else (funNmPP,funPPL ++ [argPP])

mkExtAppPP :: (HsName,PP_Doc,[PP_Doc]) -> (HsName,PP_Doc,[PP_Doc],PP_Doc) -> (PP_Doc,[PP_Doc])
mkExtAppPP = mkExtAppPP' "|"
%%]

%%[8
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
    then do { hPutStrLn stdout (strBlankPad 40 msg ++ " " ++ strBlankPad 22 (show modNm) ++ " (" ++ fpathToStr fNm ++ maybe "" (\m -> ", " ++ m) mbMsg2 ++ ")")
            ; hFlush stdout
            }
    else return ()
%%]

%%[8 export(writePP, writeToFile)
writePP ::  (a -> PP_Doc) -> a -> FPath -> IO ()
writePP f text fp = writeToFile (show.f $ text) fp

writeToFile' :: Bool -> String -> FPath -> IO ()
writeToFile' binary str fp
  = do { (fn, fh) <- openFPath fp WriteMode binary
       ; (if binary then hPutStr else hPutStrLn) fh str
       ; hClose fh
       }

writeToFile :: String -> FPath -> IO ()
writeToFile = writeToFile' False

%%]

%%[(8 java) export(writeBinaryToFile)
writeBinaryToFile :: String -> FPath -> IO ()
writeBinaryToFile = writeToFile' True
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
  | CTag
      { ctagTyNm 		:: !HsName
      , ctagNm 			:: !HsName
      , ctagTag' 		:: !Int
      , ctagArity 		:: !Int
      , ctagMaxArity 	:: !Int
      }
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

%%[1.ppAssocL export(ppAssocL)
ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL al = ppListSepFill "[ " " ]" ", " (map (\(k,v) -> pp k >|< ":" >|< pp v) al)
%%]

%%[8.ppAssocL -1.ppAssocL export(ppAssocL,ppAssocL',ppAssocLV)
ppAssocL' :: (PP k, PP v, PP s) => ([PP_Doc] -> PP_Doc) -> s -> AssocL k v -> PP_Doc
ppAssocL' ppL sep al = ppL (map (\(k,v) -> pp k >|< sep >#< pp v) al)

ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL = ppAssocL' (ppBlock "[" "]" ",") ":"

ppAssocLV :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocLV = ppAssocL' vlist ":"
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

%%[1 export(assocLGroupSort)
assocLGroupSort :: Ord k => AssocL k v -> AssocL k [v]
assocLGroupSort = map (foldr (\(k,v) (_,vs) -> (k,v:vs)) (undefined,[])) . groupSortOn fst
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
  = VerboseQuiet | VerboseMinimal | VerboseNormal | VerboseALot | VerboseDebug
  deriving (Eq,Ord,Enum)
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
%%[[(8 codegen)
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

%%[95 export(fixityAppPrio)
fixityAppPrio :: Int
fixityAppPrio = fixityMaxPrio + 1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Eq,Ord for Pos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
instance Eq Pos where
  p1 == p2 = line p1 == line p2 && column p1 == column p2

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
show2Pos :: Pos -> Pos -> String
show2Pos p1 p2
  | p1 /= p2 && p2 /= noPos  = if line p1 == line p2 
                               then mk (show (line p1))                          (Just $ show (column p1) ++ "-" ++ show (column p2))
                               else mk (show (line p1) ++ "-" ++ show (line p2)) Nothing
  | otherwise                =      mk (show (line p1))                          (Just $ show (column p1))
  where mk l c = file p1 ++ ":" ++ l ++ maybe "" (":" ++) c
%%]

%%[1
instance Show Range where
  show (Range_Range p q) = show2Pos p q
  show Range_Unknown     = "??"
  show Range_Builtin     = "builtin"

instance PP Range where
  pp = pp . show
%%]

%%[1 export(isEmptyRange)
isEmptyRange :: Range -> Bool
isEmptyRange  Range_Unknown    = True
isEmptyRange (Range_Range p _) = p == noPos
isEmptyRange  _                = False
%%]

%%[99
instance Eq Range where
  _ == _ = True				-- a Range is ballast, not a criterium to decide equality for

instance Ord Range where
  _ `compare` _ = EQ		-- a Range is ballast, not a criterium to decide equality for
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

%%[99 export(rangeUnion,rangeUnions)
posMax, posMin :: Pos -> Pos -> Pos
posMax (Pos l1 c1 f1) (Pos l2 c2 _) = Pos (l1 `max` l2) (c1 `max` c2) f1
posMin (Pos l1 c1 f1) (Pos l2 c2 _) = Pos (l1 `min` l2) (c1 `min` c2) f1

rangeUnion :: Range -> Range -> Range
rangeUnion (Range_Range b1 e1) (Range_Range b2 e2) = Range_Range (b1 `posMin` b2) (e1' `posMax` e2')
                                                  where e1' = if e1 == noPos then b1 else e1
                                                        e2' = if e2 == noPos then b2 else e2
rangeUnion Range_Unknown       r2                  = r2
rangeUnion r1                  _                   = r1

rangeUnions :: [Range] -> Range
rangeUnions = foldr1 rangeUnion
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting of Range
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.rngLift export(RngLiftArg,rngLift,rngAntilift)
type RngLiftArg  x = x
type RngLift     x = Range -> RngLiftArg x -> x

rngLift :: RngLift v
rngLift r v = v

rngAntilift :: v -> RngLiftArg v
rngAntilift = id
%%]

%%[99 -1.rngLift export(RngLiftArg,rngLift,rngAntilift)
type RngLiftArg  x = Range -> x
type RngLift     x = Range -> RngLiftArg x -> x

rngLift :: RngLift v
rngLift r mkv
  = x `seq` x
  where x = mkv r

rngAntilift :: v -> RngLiftArg v
rngAntilift = const
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
%%% Name supply, without uniqueness required
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs export(hsnLclSupply,hsnLclSupplyWith)
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
%%[[102
  where
    fevCount x | x `seq` True = cm1 "Fixity"
%%]]

instance ForceEval OrigName where
  forceEval x@(OrigLocal  n) | forceEval x `seq` True = x
  forceEval x@(OrigGlobal n) | forceEval x `seq` True = x
  forceEval x@(OrigFunc   n) | forceEval x `seq` True = x
  forceEval x                                         = x

instance ForceEval HsName where
  forceEval x@(HNm     s) | forceEval s `seq` True = x
  forceEval x@(HNmNr _ n) | forceEval n `seq` True = x
  forceEval x@(HNmQ    l) | forceEval l `seq` True = x
  forceEval x                                      = x
%%[[102
  fevCount (HNm   _ s) = cm1 "HNm"   `cmUnion` fevCount s
  fevCount (HNmNr i n) = cm1 "HNmNr" `cmUnion` fevCount n
  fevCount (HNmQ  _ l) = cm1 "HNmQ"  `cmUnion` fevCount l
  fevCount (HNPos   p) = cm1 "HNPos" `cmUnion` fevCount p
%%]]

instance ForceEval CTag where
  forceEval x@(CTag tn n t a ma) | forceEval tn `seq` forceEval n `seq` True = x
  forceEval x = x
%%[[102
  fevCount (CTag tn n t a ma) = cmUnions [cm1 "CTag",fevCount tn,fevCount n,fevCount t,fevCount a,fevCount ma]
  fevCount CTagRec            = cm1 "CTagRec"
%%]]

instance ForceEval Range where
  forceEval x@(Range_Range b e) | forceEval b `seq` forceEval e `seq` True = x
  forceEval x = x
%%[[102
  fevCount (Range_Range b e) = cm1 "Range_Range" `cmUnion` fevCount b `cmUnion` fevCount e
  fevCount Range_Unknown     = cm1 "Range_Unknown"
  fevCount Range_Builtin     = cm1 "Range_Builtin"
%%]]

instance ForceEval Pos where
  forceEval x@(Pos l c f) | forceEval l `seq` forceEval c `seq` forceEval f `seq` True = x
%%[[102
  fevCount (Pos l c f) = cm1 "Pos" `cmUnion` fevCount l `cmUnion` fevCount c `cmUnion` fevCount f
%%]]

instance ForceEval IdOcc
%%[[102
  where
    fevCount (IdOcc x y) = cm1 "IdOcc" `cmUnion` fevCount x `cmUnion` fevCount y
%%]]

%%[[102
instance ForceEval IdOccKind where
  fevCount x | x `seq` True = cm1 "IdOccKind_*"
%%]]

instance ForceEval UID where
  forceEval x@(UID _ l) | forceEval l `seq` True = x
%%[[102
  fevCount (UID _ l) = cm1 "UID" `cmUnion` fevCount l
%%]]

instance ForceEval a => ForceEval (RLList a) where
  forceEval x@(RLList l) | forceEval l `seq` True = x
%%[[102
  fevCount (RLList l) = cm1 "RLList" `cmUnion` fevCount l
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fake AG dependency: first param is not used, only introduces an AG dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
%%]
agFakeDependOn :: a -> b -> b
agFakeDependOn _ x = x

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
withLkupLiftCyc2 :: (t -> Maybe UID) -> (t -> UIDS) -> (UID -> Maybe t) -> x -> (UIDS -> t -> x) -> (t -> x) -> UIDS -> UID -> x
withLkupLiftCyc2 get noVisit lookup dflt yes no vsVisited v
  = case lookup v of
      Just t | not (v `Set.member` vsVisited)
        -> yes (Set.insert v $ Set.union (noVisit t) vsVisited) t
      _ -> dflt
%%]

%%[2 hs export(withLkupLiftCyc1,withLkupChkVisitLift,withLkupLift)
withLkupLiftCyc1 :: (t -> Maybe UID) -> (t -> UIDS) -> (UID -> Maybe t) -> (UIDS -> t -> x) -> (t -> x) -> UIDS -> t -> x
withLkupLiftCyc1 get noVisit lookup yes no vsVisited t
  = maybe dflt (withLkupLiftCyc2 get noVisit lookup dflt yes no vsVisited) $ get t
  where dflt = no t

withLkupChkVisitLift :: (t -> Maybe UID) -> (t -> UIDS) -> (UID -> Maybe t) -> (t -> x) -> (t -> x) -> t -> x
withLkupChkVisitLift get noVisit lookup yes no t
  = withLkupLiftCyc1 get noVisit lookup (\_ t -> yes t) no Set.empty t

withLkupLift :: (t -> Maybe UID) -> (UID -> Maybe t) -> (t -> x) -> (t -> x) -> t -> x
withLkupLift get
  = withLkupChkVisitLift get (const Set.empty)
%%]

%%[2 hs
lookupLiftCyc1 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x' -> (x->x') -> x -> x'
lookupLiftCyc1 get lookup dflt found x
  = lk Set.empty dflt found x
  where lk s dflt found x = withLkupLiftCyc1 get (const Set.empty) lookup (\s t -> lk s (found t) found t) (const dflt) s x

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Combinations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Combine [[x1..xn],..,[y1..ym]] to [[x1..y1],[x1..y2],..,[xn..ym]].
Each element [xi..yi] is distinct based on the the key k in xi==(k,_)

%%[9 export(combineToDistinguishedElts)
combineToDistinguishedElts :: Eq k => [AssocL k v] -> [AssocL k v]
combineToDistinguishedElts []     = []
combineToDistinguishedElts [[]]   = []
combineToDistinguishedElts [x]    = map (:[]) x
combineToDistinguishedElts (l:ls)
  = combine l $ combineToDistinguishedElts ls
  where combine l ls
          = concatMap (\e@(k,_)
                         -> mapMaybe (\ll -> maybe (Just (e:ll)) (const Nothing) $ lookup k ll)
                                     ls
                      ) l
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run length encoded list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(RLList(..))
newtype RLList a
  = RLList [(a,Int)]
  deriving (Eq)

instance Ord a => Ord (RLList a) where
  (RLList [])           `compare` (RLList [])           = EQ
  (RLList [])           `compare` (RLList _ )           = LT
  (RLList _ )           `compare` (RLList [])           = GT
  (RLList ((x1,c1):l1)) `compare` (RLList ((x2,c2):l2)) | x1 == x2 = if c1 == c2
                                                                     then RLList l1 `compare` RLList l2
                                                                     else c1 `compare` c2
                                                        | x1 <  x2 = LT
                                                        | x1 >  x2 = GT
%%]

%%[9 export(rllConcat,rllSingleton,rllEmpty,rllToList,rllFromList)
rllConcat :: Eq a => RLList a -> RLList a -> RLList a
rllConcat (RLList []) rll2  = rll2
rllConcat rll1 (RLList [])  = rll1
rllConcat (RLList l1) (RLList l2@(h2@(x2,c2):t2))
                            | x1 == x2  = RLList (h1 ++ [(x1,c1+c2)] ++ t2)
                            | otherwise = RLList (l1 ++ l2)
                            where (h1,t1@(x1,c1)) = fromJust (initlast l1)

rllEmpty :: RLList a
rllEmpty = RLList []

rllSingleton :: a -> RLList a
rllSingleton x = RLList [(x,1)]

rllToList :: RLList a -> [a]
rllToList (RLList l) = concatMap (\(x,c) -> replicate c x) l

rllFromList :: Eq a => [a] -> RLList a
rllFromList l = RLList [ (x,length g) | g@(x:_) <- group l ]
%%]

%%[9 export(rllLength,rllNull)
rllLength :: RLList a -> Int
rllLength (RLList l) = sum $ map snd l

rllNull :: RLList a -> Bool
rllNull (RLList []) = True
rllNull (RLList _ ) = False
%%]

%%[9 export(rllIsPrefixOf)
rllIsPrefixOf :: Eq a => RLList a -> RLList a -> Bool
rllIsPrefixOf (RLList []) _ = True
rllIsPrefixOf _ (RLList []) = False
rllIsPrefixOf (RLList ((x1,c1):l1)) (RLList ((x2,c2):l2))
                            | x1 == x2  = if c1 < c2
                                          then True
                                          else if c1 > c2
                                          then False
                                          else rllIsPrefixOf (RLList l1) (RLList l2)
                            | otherwise = False
%%]

%%[9 export(rllInits,rllInit,rllInitLast)
rllInitLast :: Eq a => RLList a -> Maybe (RLList a,a)
rllInitLast (RLList l ) = il [] l
                        where il acc [(x,1)]    = Just (RLList (reverse acc),x)
                              il acc [(x,c)]    = Just (RLList (reverse ((x,c-1):acc)),x)
                              il acc (a:as)     = il (a:acc) as
                              il _   _          = Nothing

rllInit :: Eq a => RLList a -> RLList a
rllInit = fst . fromJust . rllInitLast

rllInits :: Eq a => RLList a -> [RLList a]
rllInits = map rllFromList . inits . rllToList
%%]

%%[9 export(rllHeadTail)
rllHeadTail :: RLList a -> Maybe (a,RLList a)
rllHeadTail (RLList [])        = Nothing
rllHeadTail (RLList ((x,1):t)) = Just (x,RLList t)
rllHeadTail (RLList ((x,c):t)) = Just (x,RLList ((x,c-1):t))
%%]

%%[9
instance Show a => Show (RLList a) where
  show = show . rllToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AlwaysEq
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

General purpose info for which comparison always yields EQ.
This is to fool 'deriving' when info is added for debugging purposes only.

%%[1 export(AlwaysEq(..))
data AlwaysEq a = AlwaysEq a

instance Eq (AlwaysEq a) where
  _ == _ = True

instance Ord (AlwaysEq a) where
  _ `compare` _ = EQ

instance Show a => Show (AlwaysEq a) where
  show (AlwaysEq x) = show x

instance PP a => PP (AlwaysEq a) where
  pp (AlwaysEq x) = pp x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Package name
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(PkgName)
type PkgName = String
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derivation tree ways of printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(DerivTreeWay(..))
data DerivTreeWay
  = DerivTreeWay_Infer		-- follow order of inference when printing type variables
  | DerivTreeWay_Final		-- use final mapping of type variables instead
  | DerivTreeWay_None		-- no printing
  deriving Eq
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Row specific
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 hs export(rowCanonOrderBy)
-- order on ...
rowCanonOrderBy :: (o -> o -> Ordering) -> AssocL o a -> AssocL o a
rowCanonOrderBy cmp = sortByOn cmp fst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Meta levels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2 hs export(MetaLev)
type MetaLev = Int
%%]

%%[4 hs export(metaLevVal)
metaLevVal :: MetaLev
metaLevVal = 0
%%]

%%[6 hs export(metaLevTy, metaLevKi, metaLevSo)
metaLevTy, metaLevKi, metaLevSo :: MetaLev
metaLevTy  = metaLevVal + 1
metaLevKi  = metaLevTy  + 1
metaLevSo  = metaLevKi  + 1
%%]



