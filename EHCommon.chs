% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(HsName(..), hsnWild, hsnArrow, hsnProd, hsnProdArity, hsnUnknown, hsnIsArrow, hsnIsProd, hsnInt, hsnChar)
%%]

%%[1 export(AssocL, hdAndTl)
%%]

%%[1 import(UU.Pretty, List) export(PP_DocL, ppListSep, ppListSepFill, ppAppTop, ppCon, ppCmt)
%%]

%%[1 import(GetOpt) export(EHCOpts(..), defaultEHCOpts, cmdLineOpts)
%%]

%%[1 export(MkConAppAlg, mkApp, mkConApp, mkArrow)
%%]

%%[1.mkProdApp.exp export(mkProdApp)
%%]

%%[7.mkProdApp.exp -1.mkProdApp.exp
%%]

%%[1 export(ParNeed(..), ParNeedL, parNeedApp, ppParNeed)
%%]

%%[2 export(UID, mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, nextUID, mkNewUID, mkNewUIDL, startUID)
%%]

%%[2 import(List) export(unionL)
%%]

%%[3 export(hsnUn, hsnIsUn, hsnUnUn)
%%]

%%[4 export(listCombineUniq)
%%]

%%[4 export(CoContraVariance(..), cocoOpp)
%%]

%%[6 export(hsnStar)
%%]

%%[7 export(hsnRow,hsnRec,hsnSum,hsnRowEmpty,hsnIsRec,hsnIsSum)
%%]

%%[7 export(hsnORow,hsnCRow,hsnORec,hsnCRec,hsnOSum,hsnCSum)
%%]

%%[7 export(positionalFldNames,ppFld)
%%]

%%[8 export(ppAssocL)
%%]

%%[8 export(hsnUndefined)
%%]

%%[8 export(hsnPrefix)
%%]

%%[9 export(hsnOImpl,hsnCImpl)
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
hsnPrefix						 	::  String -> HsName -> HsName
hsnPrefix	p	hsn					=	HNm (p ++ show hsn)
%%]

%%[8
hsnUndefined                        =   HNm "undefined"
%%]

%%[9
hsnOImpl                            =   HNm "(#"
hsnCImpl                            =   HNm "#)"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unique id's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.UID.Base
newtype UID= UID [Int] deriving (Eq,Ord)
%%]

%%[2.UID.Rest
type UIDL = [UID]

instance Show UID where
  show (UID (l:ls)) = foldl (\ls l -> show l ++ "_" ++ ls) (show l) ls
%%]

%%[2.UID.mkNewLevUID
nextUID :: UID -> UID
nextUID (UID (l:ls)) = UID (l+1:ls)

mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u@(UID ls) = (nextUID u,UID (0:ls))
%%]

%%[2.UID.Utils
mkNewLevUID2 u = let { (u',u1)     = mkNewLevUID   u; (u'',u2)        = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)  = mkNewLevUID2  u; (u'',u3)        = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)  = mkNewLevUID2  u; (u'',u3,u4)     = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)  = mkNewLevUID2  u; (u'',u3,u4,u5)  = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)

startUID :: UID
startUID = UID [0]

mkNewUID :: UID -> (UID,UID)
mkNewUID   uid = (nextUID uid,uid)

mkNewUIDL :: Int -> UID -> (UID,[UID]) -- assume sz > 0
mkNewUIDL sz uid
  =  let  l = take sz . iterate (\(nxt,uid) -> mkNewUID nxt) . mkNewUID $ uid
     in   (fst (last l),map snd l)

instance PP UID where
  pp uid = text (show uid)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Building specific structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.mkApp.Base
type MkConAppAlg t = (HsName -> t,t -> t -> t,t -> t,t -> t)

mkApp :: MkConAppAlg t -> [t] -> t
mkApp (_,app,top,_) ts
  =  case ts of
       [t]  ->  t
       _    ->  top (foldl1 app ts)
%%]

%%[1.mkApp.mkConApp
mkConApp :: MkConAppAlg t -> HsName -> [t] -> t
mkConApp alg@(con,_,_,_) c ts = mkApp alg (con c : ts)
%%]

%%[1.mkApp.mkProdApp
mkProdApp :: MkConAppAlg t -> [t] -> t
mkProdApp alg ts = mkConApp alg (hsnProd (length ts)) ts
%%]

%%[7 -1.mkApp.mkProdApp
%%]

%%[1.mkApp.Rest
mkArrow :: MkConAppAlg t -> t -> t -> t
mkArrow alg@(con,_,_,_) a r = mkApp alg [con hsnArrow,a,r]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[ppAppTop.1
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) args dflt
  =  if       hsnIsArrow conNm  then     ppListSep "" "" (" " ++ show hsnArrow ++ " ") args
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
%%]

%%[8
ppAssocL :: (PP k,PP v) => AssocL k v -> PP_Doc
ppAssocL = ppListSep "(" ")" "," . map (\(k,v) -> k >|< ":" >|< v)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Prio computation for need of parenthesis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ParNeed
data ParNeed =  ParNotNeeded | ParNeededLow | ParNeeded | ParNeededHigh | ParOverrideNeeded
                deriving (Eq,Ord)

type ParNeedL = [ParNeed]

parNeedApp :: HsName -> (ParNeed,ParNeed,ParNeedL)
parNeedApp conNm
  =  let  pr  | hsnIsArrow  conNm   =  (ParNeededLow,ParNotNeeded,[ParNotNeeded,ParNeeded])
              | hsnIsProd   conNm   =  (ParOverrideNeeded,ParNotNeeded,repeat ParNotNeeded)
              | otherwise           =  (ParNeeded,ParNeededHigh,repeat ParNeededHigh)
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

%%[4.cocoOpp
cocoOpp :: CoContraVariance -> CoContraVariance
cocoOpp  CoVariant      =   ContraVariant
cocoOpp  ContraVariant  =   CoVariant
cocoOpp  _              =   CoContraVariant
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[EHCOpts.1
data EHCOpts    = EHCOptions    {  ehcoptDumpPP         ::  Maybe String
                                ,  ehcoptShowTopTyPP    ::  Bool
                                ,  ehcoptHelp           ::  Bool
                                ,  ehcoptDebug          ::  Bool
%%]

%%[EHCOpts.8
                                ,  ehcoptCode           ::  Bool
                                ,  ehcoptCodeJava       ::  Bool
%%]

%%[defaultEHCOpts.1
defaultEHCOpts  = EHCOptions    {  ehcoptDumpPP         =   Just "pp"
                                ,  ehcoptShowTopTyPP    =   False
                                ,  ehcoptHelp           =   False
                                ,  ehcoptDebug          =   False
%%]

%%[defaultEHCOpts.8
                                ,  ehcoptCode           =   True
                                ,  ehcoptCodeJava       =   False
%%]

%%[cmdLineOptsA.1
cmdLineOpts  
  =  [  Option "p"  ["pretty"]        (OptArg oPretty "pp|ast|no")
          "do output pretty printed version of src, default=pp"
     ,  Option "d"  ["debug"]         (NoArg oDebug)
          "include debug info, for now: dump extra info in ast pretty print"
     ,  Option ""   ["show-top-ty"]   (OptArg oShowTopTy "yes|no")
          "show top ty, default=no"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
%%]

%%[cmdLineOptsA.8
     ,  Option "c"  ["code"]   (OptArg oCode "java")
          "dump code (java -> .java) on file, default=code (-> .code)"
%%]

%%[cmdLineOptsB.1
  where  oPretty     ms  o =  case ms of
                                Just "no"   -> o { ehcoptDumpPP        = Nothing   }
                                Just p      -> o { ehcoptDumpPP        = Just p    }
                                _           -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcoptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcoptHelp          = True    }
         oDebug          o =  o { ehcoptDebug         = True    }
%%]

%%[cmdLineOptsB.8
         oCode       ms  o =  case ms of
                                Just "java"  -> o { ehcoptCodeJava     = True      }
                                _            -> o { ehcoptCode         = True      }
%%]

%%[1.Options
%%@EHCOpts.1
                                }

%%@defaultEHCOpts.1
                                }

%%@cmdLineOptsA.1
     ]
%%@cmdLineOptsB.1
%%]

%%[8.Options -1.Options
%%@EHCOpts.1
%%@EHCOpts.8
                                }

%%@defaultEHCOpts.1
%%@defaultEHCOpts.8
                                }

%%@cmdLineOptsA.1
%%@cmdLineOptsA.8
     ]
%%@cmdLineOptsB.1
%%@cmdLineOptsB.8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AssocL
type AssocL k v = [(k,v)]
%%]

%%[1.Misc
hdAndTl :: [a] -> (a,[a])
hdAndTl (a:as) = (a,as)
%%]

%%[2.Misc
unionL :: Eq a => [[a]] -> [a]
unionL = foldr union []
%%]

%%[4.listCombineUniq
listCombineUniq :: Eq a => [[a]] -> [a]
listCombineUniq = nub . concat
%%]

