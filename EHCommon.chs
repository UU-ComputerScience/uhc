% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(HsName(..), hsnWild, hsnArrow, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar)
%%]

%%[1 export(AssocL, hdAndTl, ppAssocL)
%%]

%%[1 import(UU.Pretty, List) export(PP_DocL, ppListSep, ppCommaList, ppListSepFill, ppSpaced, ppAppTop, ppCon, ppCmt)
%%]

%%[1 export(MkConApp, mkApp, mkConApp, mkArrow)
%%]

%%[1.mkProdApp.exp export(mkProdApp)
%%]

%%[7.mkProdApp.exp -1.mkProdApp.exp
%%]

%%[1 export(assocLKeys)
%%]

%%[1 export(ParNeed(..), ParNeedL, parNeedApp, ppParNeed)
%%]

%%[2 export(UID, mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, uidNext, mkNewUID, mkNewUIDL, uidStart)
%%]

%%[2 export(assocLMapElt,assocLMapKey)
%%]

%%[2 import(List) export(unionL)
%%]

%%[3 export(hsnUn, hsnIsUn, hsnUnUn)
%%]

%%[4 export(listCombineUniq)
%%]

%%[4 export(CoContraVariance(..), cocoOpp)
%%]

%%[4 export(FIMode(..),fimOpp,fimSwapCoCo)
%%]

%%[6 export(hsnStar)
%%]

%%[7 export(hsnRow,hsnRec,hsnSum,hsnRowEmpty,hsnIsRec,hsnIsSum)
%%]

%%[7 export(hsnORow,hsnCRow,hsnORec,hsnCRec,hsnOSum,hsnCSum)
%%]

%%[7 export(positionalFldNames,ppFld,mkExtAppPP,mkPPAppFun)
%%]

%%[7 export(assocLElts,uidHNm)
%%]

%%[8 import (FPath,IO,Char) export(putPPLn,putWidthPPLn,putPPFile,Verbosity(..),putCompileMsg)
%%]

%%[8 export(hsnPrefix,hsnSuffix,hsnConcat)
%%]

%%[8 export(hsnUndefined,hsnPrimAddInt,hsnMain)
%%]

%%[8 export(sortByOn,sortOn,groupOn,groupSortOn)
%%]

%%[8 export(Seq,mkSeq,unitSeq,concatSeq,"(<+>)",seqToList,emptySeq)
%%]

%%[8 import (FiniteMap) export(showPP,ppPair,ppFM)
%%]

%%[8 export(mkNewLevUIDL,mkInfNewLevUIDL)
%%]

%%[8 export(hsnUniqSupplyL,hsnLclSupplyL)
%%]

%%[8 export(CTag(..))
%%]

%%[8 export(CTagsMp)
%%]

%%[9 export(groupSortByOn)
%%]

%%[9 export(hsnOImpl,hsnCImpl,hsnPrArrow,hsnIsPrArrow,hsnIsUnknown)
%%]

%%[9 export(ppListV,ppAssocLV)
%%]

%%[9 hs export(PredOccId(..),mkPrId,poiHNm)
%%]

%%[9 hs export(PrfCtxtId)
%%]

%%[9 hs export(snd3,thd)
%%]

%%[10 export(hsnDynVar)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.HsName.type
data HsName                         =   HNm String
                                    deriving (Eq,Ord)

instance Show HsName where
  show (HNm s) = s
%%]

%%[1
instance PP HsName where
  pp h = pp (show h)
%%]

%%[7.HsName.type -1.HsName.type
data HsName                         =   HNm String
                                    |   HNPos Int
                                    deriving (Eq,Ord)

instance Show HsName where
  show (HNm s    )  = s
  show (HNPos p  )  = show p
%%]

%%[1.HsName.Base
hsnArrow, hsnUnknown, hsnInt, hsnChar, hsnWild :: HsName
hsnArrow                            =   HNm "->"
hsnUnknown                          =   HNm "??"
hsnInt                              =   HNm "Int"
hsnChar                             =   HNm "Char"
hsnWild                             =   HNm "_"

hsnProd                             ::  Int -> HsName
hsnProd         i                   =   HNm  (',' : show i)

hsnIsArrow, hsnIsProd               ::  HsName -> Bool
hsnIsArrow      hsn                 =   hsn == hsnArrow
hsnIsProd       (HNm (',':_))       =   True
hsnIsProd       _                   =   False

hsnProdArity                        ::  HsName -> Int
hsnProdArity    (HNm (_:ar))        =   read ar
%%]

%%[3
hsnUn                               ::  HsName -> HsName
hsnUn           nm                  =   HNm ("un" ++ show nm)

hsnIsUn         (HNm ('u':'n':_))   =   True
hsnIsUn         _                   =   False

hsnUnUn         (HNm ('u':'n':nm))  =   HNm nm
%%]

%%[6
hsnStar                             =   HNm "*"
%%]

%%[7
hsnORow                             =   HNm "(|"
hsnCRow                             =   HNm "|)"
hsnOSum                             =   HNm "(<"
hsnCSum                             =   HNm ">)"
hsnORec                             =   HNm "("
hsnCRec                             =   HNm ")"

hsnRow                              =   HNm "Row"
hsnRec                              =   HNm "Rec"
hsnSum                              =   HNm "Var"
hsnRowEmpty                         =   HNm (show hsnORow ++ show hsnCRow)

hsnIsRec, hsnIsSum, hsnIsRow        ::  HsName -> Bool
hsnIsRec        hsn                 =   hsn == hsnRec
hsnIsSum        hsn                 =   hsn == hsnSum
hsnIsRow        hsn                 =   hsn == hsnRow

positionalFldNames                  ::  [HsName]
positionalFldNames                  =   map HNPos [1..]
%%]

%%[8
hsnPrefix                           ::  String -> HsName -> HsName
hsnPrefix   p   hsn                 =   HNm (p ++ show hsn)

hsnSuffix                           ::  HsName -> String -> HsName
hsnSuffix       hsn   p             =   HNm (show hsn ++ p)

hsnConcat                           ::  HsName -> HsName -> HsName
hsnConcat       h1    h2            =   HNm (show h1 ++ show h2)
%%]

%%[8
hsnMain                             =   HNm "main"
hsnUndefined                        =   HNm "undefined"
hsnPrimAddInt						=	HNm "primAddInt"
%%]

%%[9
hsnOImpl                            =   HNm "(!"
hsnCImpl                            =   HNm "!)"
hsnPrArrow                          =   HNm "=>"

hsnIsPrArrow                        ::  HsName -> Bool
hsnIsPrArrow    hsn                 =   hsn == hsnPrArrow
hsnIsUnknown                        =   (==hsnUnknown)
%%]

%%[10
hsnDynVar                           =   HNm "?"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name supply, with/without uniqueness required
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs
hsnUniqSupplyL :: UID -> [HsName]
hsnUniqSupplyL = map uidHNm . iterate uidNext

hsnLclSupplyL :: [HsName]
hsnLclSupplyL = map (\i -> HNm ("_" ++ show i)) [1..]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unique id's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.UID.Base
newtype UID= UID [Int] deriving (Eq,Ord)
%%]

%%[2.UID.UIDL
type UIDL = [UID]
%%]

%%[2.UID.Show
instance Show UID where
  show (UID ls) = concat . intersperse "_" . map show . reverse $ ls
%%]

%%[2.UID.mkNewLevUID
uidNext :: UID -> UID
uidNext (UID (l:ls)) = UID (l+1:ls)

mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u@(UID ls) = (uidNext u,UID (0:ls))
%%]

%%[2.UID.Utils
mkNewLevUID2 u = let { (u',u1)          = mkNewLevUID   u; (u'',u2)         = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3)         = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4)      = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4,u5)   = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)

uidStart :: UID
uidStart = UID [0]

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

instance PP UID where
  pp = text . show
%%]

%%[7
uidHNm :: UID -> HsName
uidHNm = HNm . show
%%]

%%[8
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred occurrence id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 hs
data PredOccId =  PredOccId {poiCxId :: PrfCtxtId, poiId :: UID} deriving (Show,Eq,Ord)

mkPrId :: PrfCtxtId -> UID -> PredOccId
mkPrId ci u = PredOccId ci u

poiHNm :: PredOccId -> HsName
poiHNm = uidHNm . poiId

instance PP PredOccId where
  pp poi = "Cx:" >|< pp (poiCxId poi) >|< "/Pr:" >|< pp (poiId poi)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ordered sequence, 'delayed concat' list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
newtype Seq a = Seq ([a] -> [a])

emptySeq :: Seq a
emptySeq = Seq id

mkSeq :: [a] -> Seq a
mkSeq l = Seq (l++)

unitSeq :: a -> Seq a
unitSeq e = Seq (e:)

concatSeq :: Seq a -> Seq a -> Seq a
concatSeq (Seq s1) (Seq s2) = Seq (s1.s2)

infixr 5 <+>

(<+>) :: Seq a -> Seq a -> Seq a
(<+>) = concatSeq

seqToList :: Seq a -> [a]
seqToList (Seq s) = s []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Building specific structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.MkConApp
type MkConApp t = (HsName -> t,t -> t -> t,t -> t,t -> t)
%%]

%%[1.mkApp.Base
mkApp :: MkConApp t -> [t] -> t
mkApp (_,app,top,_) ts
  =  case ts of
       [t]  ->  t
       _    ->  top (foldl1 app ts)
%%]

%%[1.mkApp.mkConApp
mkConApp :: MkConApp t -> HsName -> [t] -> t
mkConApp alg@(con,_,_,_) c ts = mkApp alg (con c : ts)
%%]

%%[1.mkApp.mkProdApp
mkProdApp :: MkConApp t -> [t] -> t
mkProdApp alg ts = mkConApp alg (hsnProd (length ts)) ts
%%]

%%[7 -1.mkApp.mkProdApp
%%]

%%[1.mkApp.mkArrow
mkArrow :: MkConApp t -> t -> t -> t
mkArrow alg@(con,_,_,_) a r = mkApp alg [con hsnArrow,a,r]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[ppAppTop.1
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) args dflt
  =  if       hsnIsArrow conNm  then     ppListSep "" "" (" " >|< con >|< " ") args
     else if  hsnIsProd conNm   then     ppListSep "(" ")" "," args
%%]

%%[1.PP.ppAppTop
%%@ppAppTop.1
                                else     dflt
%%]

%%[7.PP.ppAppTop -1.PP.ppAppTop
%%@ppAppTop.1
     else if  hsnIsRec  conNm   then     ppListSep (hsnORec >|< con) hsnCRec "," args
     else if  hsnIsSum  conNm   then     ppListSep (hsnOSum >|< con) hsnCSum "," args
     else if  hsnIsRow  conNm   then     ppListSep (hsnORow >|< con) hsnCRow "," args
                                else     dflt
%%]

%%[1.PP.NeededByExpr
type PP_DocL = [PP_Doc]

ppListSep :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSep o c s pps = o >|< hlist (intersperse (pp s) (map pp pps)) >|< c

ppCon :: HsName -> PP_Doc
ppCon nm =  if    hsnIsProd nm
            then  pp_parens (text (replicate (hsnProdArity nm - 1) ','))
            else  pp nm

ppCmt :: PP_Doc -> PP_Doc
ppCmt p = "{-" >#< p >#< "-}"
%%]

%%[1.PP.Rest
ppCommaList :: PP a => [a] -> PP_Doc
ppCommaList = ppListSep "[" "]" ","

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
ppFld :: String -> HsName -> HsName -> PP_Doc -> PP_Doc
ppFld sep positionalNm nm f
  = if nm == positionalNm then f else nm >#< sep >#< f

mkPPAppFun :: HsName -> PP_Doc -> PP_Doc
mkPPAppFun c p = if c == hsnRowEmpty then empty else p >|< "|"

mkExtAppPP :: (HsName,PP_Doc,PP_DocL) -> (HsName,PP_Doc,PP_DocL,PP_Doc) -> (PP_Doc,PP_DocL)
mkExtAppPP (funNm,funNmPP,funPPL) (argNm,argNmPP,argPPL,argPP)
  =  if hsnIsRec funNm || hsnIsSum funNm
     then (mkPPAppFun argNm argNmPP,argPPL)
     else (funNmPP,funPPL ++ [argPP])
%%]

%%[4
instance PP a => PP (Maybe a) where
  pp m = maybe (pp "?") pp m

instance PP Bool where
  pp b = pp (show b)
%%]

%%[9
instance (PP a, PP b) => PP (a,b) where
  pp (a,b) = ppListSep "(" ")" "," [pp a,pp b]
%%]

%%[8
ppPair :: (PP a, PP b) => (a,b) -> PP_Doc
ppPair (x,y) = pp_parens (pp x >|< "," >|< pp y)
%%]

%%[8
showPP :: PP a => a -> String
showPP x = disp (pp x) 100 ""
%%]

%%[8
ppFM :: (PP k,PP v) => FiniteMap k v -> PP_Doc
ppFM = ppAssocL . fmToList
%%]

%%[9
ppListV :: PP a => [a] -> PP_Doc
ppListV = vlist . map pp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Putting stuff on output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
putWidthPPLn :: Int -> PP_Doc -> IO ()
putWidthPPLn w pp = putStrLn (disp pp w "")

putPPLn :: PP_Doc -> IO ()
putPPLn = putWidthPPLn 4000

putCompileMsg :: Verbosity -> Verbosity -> String -> Maybe String -> HsName -> FPath -> IO ()
putCompileMsg v optsVerbosity msg mbMsg2 modNm fNm
  = if optsVerbosity >= v
    then putStrLn (strBlankPad 25 msg ++ " " ++ strBlankPad 15 (show modNm) ++ " (" ++ fpathToStr fNm ++ maybe "" (\m -> ", " ++ m) mbMsg2 ++ ")")
    else return ()

putPPFile :: String -> PP_Doc -> Int -> IO ()
putPPFile fn pp wid
  =  do  {  h <- openFile fn WriteMode
         ;  hPutStrLn h (disp pp wid "")
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
              | otherwise           =  (ParNeeded,repeat ParNeededHigh)
     in   pr

ppParNeed :: PP p => ParNeed -> ParNeed -> p -> PP_Doc
ppParNeed locNeed globNeed p
  = par (pp p)
  where par = if globNeed > locNeed then pp_parens else id
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
%%% Tags (of data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs
data CTag
  = CTagRec
  | CTag {ctagTyNm :: HsName, ctagNm :: HsName, ctagTag :: Int, ctagArity :: Int}
  deriving (Show,Eq,Ord)
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
type AssocL k v = [(k,v)]
%%]

%%[1.ppAssocL
ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL al = ppListSepFill "[ " " ]" ", " (map (\(k,v) -> pp k >|< ":" >|< pp v) al)
%%]

%%[9.ppAssocL -1.ppAssocL
ppAssocL' :: (PP k, PP v) => ([PP_Doc] -> PP_Doc) -> AssocL k v -> PP_Doc
ppAssocL' ppL al = ppL (map (\(k,v) -> pp k >|< ":" >|< pp v) al)

ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL = ppAssocL' (pp_block "[" "]" ",")

ppAssocLV :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocLV = ppAssocL' vlist
%%]

%%[2
assocLMap :: (k -> v -> (k',v')) -> AssocL k v -> AssocL k' v'
assocLMap f = map (uncurry f)

assocLMapElt :: (v -> v') -> AssocL k v -> AssocL k v'
assocLMapElt f = assocLMap (\k v -> (k,f v))

assocLMapKey :: (k -> k') -> AssocL k v -> AssocL k' v
assocLMapKey f = assocLMap (\k v -> (f k,v))
%%]

%%[1
assocLKeys :: AssocL k v -> [k]
assocLKeys = map fst
%%]

%%[7
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
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Misc
hdAndTl :: [a] -> (a,[a])
hdAndTl (a:as) = (a,as)
%%]

%%[2.unionL
unionL :: Eq a => [[a]] -> [a]
unionL = foldr union []
%%]

%%[4.listCombineUniq
listCombineUniq :: Eq a => [[a]] -> [a]
listCombineUniq = nub . concat
%%]

%%[8
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortByOn compare

sortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [a]
sortByOn cmp sel = sortBy (\e1 e2 -> sel e1 `cmp` sel e2)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn sel = groupBy (\e1 e2 -> sel e1 == sel e2)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn sel = groupOn sel . sortOn sel
%%]

%%[9
groupByOn :: (b -> b -> Bool) -> (a -> b) -> [a] -> [[a]]
groupByOn eq sel = groupBy (\e1 e2 -> sel e1 `eq` sel e2)

groupSortByOn :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [[a]]
groupSortByOn cmp sel = groupByOn (\e1 e2 -> cmp e1 e2 == EQ) sel . sortByOn cmp sel
%%]

%%[8
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

%%[8
data Verbosity
  = VerboseQuiet | VerboseNormal | VerboseALot
  deriving (Eq,Ord)
%%]

