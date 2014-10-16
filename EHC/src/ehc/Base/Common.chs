%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Common}
%%]

%%[1 import(UHC.Util.Utils)
%%]

%%[1 import({%{EH}Base.HsName},{%{EH}Base.HsName.Builtin}) export(module {%{EH}Base.HsName})
%%]

%%[1 import({%{EH}Base.Range}) export(module {%{EH}Base.Range})
%%]

%%[1 import({%{EH}Base.UID}) export(module {%{EH}Base.UID})
%%]

%%[1 import(UHC.Util.AssocL) export(module UHC.Util.AssocL)
%%]

%%[1 import(UHC.Util.Pretty, Data.List)
%%]

%%[1 import(Control.Applicative((<|>)))
%%]

%%[1 import(UHC.Util.ScanUtils)
%%]

%%[1.Token hs import(UU.Scanner.Token)
%%]

%%[1 import(qualified Data.Set as Set)
%%]

%%[5 -1.Token hs import({%{EH}Scanner.Token}, {%{EH}Scanner.Machine(scanpredIsIdChar, scanpredIsKeywExtra)})
%%]

%%[7777 export(Seq,mkSeq,unitSeq,concatSeq,"(<+>)",seqToList,emptySeq,concatSeqs,filterSeq)
%%]

%%[7_2 import(qualified Data.Map as Map, Data.Map(Map), Data.Set(Set))
%%]

%%[7_2 export(threadMap,Belowness(..), groupAllBy, mergeListMap)
%%]

%%[8 import (UHC.Util.FPath,System.IO,System.Environment,System.Exit,Data.Char,Data.Maybe,Numeric)
%%]

%%[8 import({%{EH}Base.Fld}) export(module {%{EH}Base.Fld})
%%]

%%[8 import({%{EH}CodeGen.Tag}) export(module {%{EH}CodeGen.Tag})
%%]

%%[8 import (qualified Data.Map as Map)
%%]

%%[(8 codegen || hmtyinfer || hmtyast) import({%{EH}Base.Strictness}) export(module {%{EH}Base.Strictness})
%%]

%%[1 import(Control.Monad)
%%]
%%[8 import(qualified Control.Monad.State as ST)
%%]
%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[9 import({%{EH}Base.RLList}) export(module {%{EH}Base.RLList})
%%]

%%[9999 import({%{EH}Base.Hashable})
%%]
%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Printing of names with non-alpha numeric constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ppHsnNonAlpha,ppHsnEscaped,hsnEscapeeChars,ppHsnEscapeWith,hsnOkChars, hsnNotOkStrs)
ppHsnEscapeWith :: Char -> (Char -> Bool) -> (String -> Bool) -> (HsName -> Bool) -> HsName -> (PP_Doc,Bool)
ppHsnEscapeWith escChar okChars notOkStr leaveAsIs n = flip ST.runState False $ do
    let shown = hsnShow' showUIDParseable show (\s -> "{" ++ s ++ "}") "." "``" n
    if leaveAsIs n
      then return $ pp n
      else do cs <- fmap concat $ forM shown esc
              isEscaped <- ST.get
              return $ pp $ if isEscaped || notOkStr shown then escChar:cs else cs
  where esc c | okChars c = return [c]
              | otherwise = ST.put True >> return [escChar,c]

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

hsnOkChars :: Char -> ScanOpts -> Char -> Bool
hsnOkChars escChar scanOpts c
  = c /= escChar && (scanpredIsIdChar c || scanpredIsKeywExtra scanOpts c)

hsnNotOkStrs :: ScanOpts -> String -> Bool
hsnNotOkStrs scanOpts s = s `Set.member` scoKeywordsTxt scanOpts

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
%%% Pred occurrence id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 hs export(PredOccId(..))
newtype PredOccId
  = PredOccId
      { poiId       :: UID
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
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.PP.ppAppTop export(ppAppTop)
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) argL dflt
  =  if       (  hsnIsArrow conNm
%%[[9
              || hsnIsPrArrow conNm
%%]]
              ) && length argL == 2
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

%%[1.PP.NeededByExpr export(ppCon, ppCmt)
ppCon :: HsName -> PP_Doc
ppCon nm =  if    hsnIsProd nm
            then  ppParens (text (replicate (hsnProdArity nm - 1) ','))
            else  pp nm

ppCmt :: PP_Doc -> PP_Doc
ppCmt p = "{-" >#< p >#< "-}"
%%]

%%[1.PP.Rest export(ppSpaced)

ppSpaced :: PP a => [a] -> PP_Doc
ppSpaced = ppListSep "" "" " "

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
%%]
instance (PP a, PP b) => PP (a,b) where
  pp (a,b) = ppParensCommas' [pp a,pp b]

%%[8 export(ppPair)
ppPair :: (PP a, PP b) => (a,b) -> PP_Doc
ppPair (x,y) = ppParens (pp x >|< "," >|< pp y)
%%]

%%[8 export(showPP)
showPP :: PP a => a -> String
showPP x = disp (pp x) 100 ""
%%]

%%[8 export(ppFM)
ppFM :: (PP k,PP v) => Map.Map k v -> PP_Doc
ppFM = ppAssocL . Map.toList
%%]

%%[9 export(ppListV)
ppListV :: PP a => [a] -> PP_Doc
ppListV = vlist . map pp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Putting stuff on output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(putCompileMsg)
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

%%[1.ParNeed export(ParNeed(..), ParNeedL, parNeedApp)
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
%%% Label for expr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(CLbl(..),clbl)
-- | Expressions in a CBound position optionally may be labelled
data CLbl
  = CLbl_None
  | CLbl_Nm
      { clblNm		:: !HsName
      }
  | CLbl_Tag
      { clblTag		:: !CTag
      }
  deriving (Show,Eq,Ord)

clbl :: a -> (HsName -> a) -> (CTag -> a) -> CLbl -> a
clbl f _ _  CLbl_None   = f
clbl _ f _ (CLbl_Nm  n) = f n
clbl _ _ f (CLbl_Tag t) = f t
%%]

%%[8 hs
instance PP CLbl where
  pp = clbl empty pp pp
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
%%% List related, should move in time to general library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.unions export(unions)
unions :: Eq a => [[a]] -> [a]
unions = foldr union []
%%]

%%[4.listCombineUniq export(listCombineUniq)
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

%%[8 export(strPadLeft, strBlankPad)
strPadLeft :: Char -> Int -> String -> String
strPadLeft c n s = replicate (n - length s) c ++ s

strBlankPad :: Int -> String -> String
strBlankPad n s = s ++ replicate (n - length s) ' '
%%]

%%[9 export(snd3,thd)
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

%%[1 export(Fixity(..))
data Fixity
  = Fixity_Infix | Fixity_Infixr | Fixity_Infixl
  deriving (Eq,Ord,Show,Enum)

instance PP Fixity where
  pp Fixity_Infix  = pp "infix"
  pp Fixity_Infixl = pp "infixl"
  pp Fixity_Infixr = pp "infixr"
%%]

%%[1 export(fixityMaxPrio)
fixityMaxPrio :: Int
fixityMaxPrio = 9
%%]

%%[91 export(fixityAppPrio)
fixityAppPrio :: Int
fixityAppPrio = fixityMaxPrio + 1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instance variant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(InstVariant(..))
data InstVariant
  = InstNormal | InstDefault
%%[[91
  | InstDeriving InstDerivingFrom
%%]]
  deriving (Eq,Ord,Show)
%%]

%%[91 export(InstDerivingFrom(..))
-- | Either a deriving combined from a datatype directly or a standalone
data InstDerivingFrom
  = InstDerivingFrom_Datatype
  | InstDerivingFrom_Standalone
  deriving (Eq,Ord,Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Levels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs export(NmLev,nmLevAbsent, nmLevBuiltin, nmLevOutside, nmLevModule)
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

%%[1.tokenVal hs export(tokenVal)
tokenVal = genTokVal
%%]

%%[5 -1.tokenVal hs
%%]

%%[1 hs export(tokMkInt,tokMkStr)
-- Assumption: tokTpIsInt (genTokTp t) == True
tokMkInt :: Token -> Int
tokMkInt t
  = case genTokTp t of
      Just TkInteger10 -> read v
      _                -> 0
  where v = tokenVal t

tokMkStr :: Token -> String
tokMkStr = tokenVal
%%]

%%[1.tokMkQName hs export(tokMkQName)
tokMkQName :: Token -> HsName
tokMkQName = hsnFromString . tokenVal
%%]

%%[7 -1.tokMkQName hs export(tokMkQName)
tokMkQName :: Token -> HsName
tokMkQName t
  = case genTokTp t of
      Just tp | tokTpIsInt tp -> mkHNmPos $ tokMkInt t
      _                       -> mkHNm $ map hsnFromString $ tokenVals t
%%]
      _                       -> mkHNm $ concat $ intersperse "." $ tokenVals t		-- ok
      _                       -> mkHNm $ concat $ tokenVals t						-- not ok

%%[1 hs export(tokMkQNames)
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
%%% Hex printing, dissecting numbers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(splitByRadix)
splitByRadix :: (Integral b) => Int -> Int -> b -> (Int,[Int])
splitByRadix len radix num
  = ( fromIntegral $ signum num
    , replicate difflen 0 ++ drop (-difflen) repr
    )
  where radix' = fromIntegral radix
        repr = reverse $
               unfoldr
                 (\b -> if b == 0
                        then Nothing
                        else let (q,r) = b `divMod` radix'
                             in  Just (fromIntegral r, q))
                 (abs num)
        difflen = len - length repr
%%]

%%[8 export(strHex)
strHex :: (Show a, Integral a) => Int -> a -> String
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
%%% Substitutable name (used by CHR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(VarUIDHsName(..),vunmNm)
data VarUIDHsName
  = VarUIDHs_Name       { vunmId :: !UID, vunmNm' :: !HsName }
  | VarUIDHs_UID        { vunmId :: !UID }
  | VarUIDHs_Var        !UID
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

%%[9 export(combineToDistinguishedElts)
-- | Combine [[x1..xn],..,[y1..ym]] to [[x1..y1],[x2..y1],..,[xn..ym]].
--   Each element [xi..yi] is distinct based on the the key k in xi==(k,_)
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

%%[99 export(PkgName, emptyPkgName)
type PkgName = String

emptyPkgName = ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Linking style
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(LinkingStyle(..))
-- | How to do linking/packaging
data LinkingStyle
  = LinkingStyle_None			-- ^ no linking (e.g. indicated by --compile-only flag)
  | LinkingStyle_Exec			-- ^ executable linking
%%[[99
  | LinkingStyle_Pkg			-- ^ package linking
%%]]
  deriving (Eq,Ord,Enum,Bounded)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derivation tree ways of printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 tyderivtree) export(DerivTreeWay(..))
data DerivTreeWay
  = DerivTreeWay_Infer      -- follow order of inference when printing type variables
  | DerivTreeWay_Final      -- use final mapping of type variables instead
  | DerivTreeWay_None       -- no printing
  deriving Eq
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Row specific
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7777 hs export(rowCanonOrderBy)
-- order on ...
rowCanonOrderBy :: (o -> o -> Ordering) -> AssocL o a -> AssocL o a
rowCanonOrderBy cmp = sortByOn cmp fst
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Meta levels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2 hs import(UHC.Util.VarLookup(MetaLev,metaLevVal)) export(MetaLev,metaLevVal)
%%]

%%[6 hs export(metaLevTy, metaLevKi, metaLevSo)
metaLevTy, metaLevKi, metaLevSo :: MetaLev
metaLevTy  = metaLevVal + 1
metaLevKi  = metaLevTy  + 1
metaLevSo  = metaLevKi  + 1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UID derivatives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(VarId, VarIdS)
-- | Use as variable id
type VarId    = UID
type VarIdS   = Set.Set UID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HsName functionality for UID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(uidHNm)
uidHNm :: UID -> HsName
uidHNm = mkHNm -- hsnFromString . show
%%]

%%[7 export(uidQualHNm)
uidQualHNm :: HsName -> UID -> HsName
uidQualHNm modnm uid =
%%[[50
                        hsnPrefixQual modnm $
%%]]
                        uidHNm uid
%%]

%%[1
%%]
instance HSNM UID where
  mkHNm x = hsnFromString ('_' : show x)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants as appearing directly from the source text, without class related toInteger (etc) interpretation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97 export(SrcConst(..))
data SrcConst
  = SrcConst_Int    Integer
  | SrcConst_Char   Char
  | SrcConst_Ratio  Integer Integer
  deriving (Eq,Show,Ord)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(fmap2Tuple)
fmap2Tuple :: Functor f => snd -> f x -> f (x,snd)
fmap2Tuple snd = fmap (\x -> (x,snd))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monad abbreviations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(whenM, unlessM)
-- | Variation of `when` where Boolean condition is computed in a monad
whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = do
  c' <- c
  when c' m
{-# INLINE whenM #-}

-- | Variation of `unless` where Boolean condition is computed in a monad
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c m = do
  c' <- c
  unless c' m
{-# INLINE unlessM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name generation for variables, mapping from arbitrary to concise name from ['a' .. ]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(genNmMap)
genNmMap :: Ord x => (String->s) -> [x] -> Map.Map x s -> (Map.Map x s, [s])
genNmMap mk xs m
  = (m',reverse ns)
  where (m',_,ns)
          = foldl (\(m,sz,ns) x
                    -> case Map.lookup x m of
                         Just n -> (m, sz, n:ns)
                         _      -> (Map.insert x n m, sz+1, n:ns)
                                where n = mk $ ch sz
                  )
                  (m,Map.size m,[]) xs
        ch x | x < 26    = [chr $ ord 'a' + x]
             | otherwise = let (q,r) = x `quotRem` 26 in ch q ++ ch r
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Variation of Maybe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(MaybeOk(..),isJustOk,isNotOk,maybeOk,fromJustOk,fromNotOk)
data MaybeOk a
  = JustOk  a
  | NotOk   String
  deriving (Eq,Ord,Show)

isJustOk (JustOk _) = True
isJustOk _          = False

fromJustOk (JustOk x) = x
fromJustOk _          = panic "fromJustOk"

isNotOk (NotOk _) = True
isNotOk _         = False

fromNotOk (NotOk x) = x
fromNotOk _         = panic "fromNotOk"

maybeOk :: (String -> x) -> (a -> x) -> MaybeOk a -> x
maybeOk _ j (JustOk x) = j x
maybeOk n _ (NotOk  x) = n x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Visit as graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(graphVisit)
-- | Abstract graph visit, over arbitrary structures
graphVisit
  :: (Ord node)
     => (thr -> graph -> node -> (thr,Set.Set node))        -- fun: visit node, get new thr and nodes to visit next
     -> (Set.Set node -> Set.Set node -> Set.Set node)      -- fun: combine new to visit + already known to visit (respectively)
     -> thr                                                 -- the accumulator, threaded as state
     -> Set.Set node                                        -- root/start
     -> graph                                               -- graph over which we visit
     -> thr                                                 -- accumulator is what we are interested in
graphVisit visit unionUnvisited thr start graph
  = snd $ v ((Set.empty,start),thr)
  where v st@((visited,unvisited),thr)
          | Set.null unvisited = st
          | otherwise          = let (n,unvisited2)      = Set.deleteFindMin unvisited
                                     (thr',newUnvisited) = visit thr graph n
                                     visited'            = Set.insert n visited
                                     unvisited3          = unionUnvisited (newUnvisited `Set.difference` visited') unvisited2
                                 in  v ((visited',unvisited3),thr')
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Known primitives, encoding semantics of particular primitives in a FFI decl, propagated to backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(KnownPrim(..))
data KnownPrim
%%[[(8 codegen)
  =
    -- platform Int
    KnownPrim_AddI
  | KnownPrim_SubI
  | KnownPrim_MulI

%%[[97
    -- platform Float
  | KnownPrim_AddF
  | KnownPrim_SubF
  | KnownPrim_MulF

    -- platform Double
  | KnownPrim_AddD
  | KnownPrim_SubD
  | KnownPrim_MulD

    -- 8 bit
  | KnownPrim_Add8          -- add: 1 byte / 8 bit, etc etc
  | KnownPrim_Sub8
  | KnownPrim_Mul8

    -- 16 bit
  | KnownPrim_Add16
  | KnownPrim_Sub16
  | KnownPrim_Mul16

    -- 32 bit
  | KnownPrim_Add32
  | KnownPrim_Sub32
  | KnownPrim_Mul32

    -- 64 bit
  | KnownPrim_Add64
  | KnownPrim_Sub64
  | KnownPrim_Mul64
%%]]
%%][8
  = KnownPrim_NONE			-- nada
%%]]
  deriving (Show,Eq,Enum,Bounded)
%%]

%%[8
instance PP KnownPrim where
  pp = pp . show
%%]

%%[8 export(allKnownPrimMp)
allKnownPrimMp :: Map.Map String KnownPrim
allKnownPrimMp
%%[[(8 codegen)
  = Map.fromList [ (drop prefixLen $ show t, t) | t <- [ minBound .. maxBound ] ]
  where prefixLen = length "KnownPrim_"
%%][8
  = Map.empty 		-- nada
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mapping from String to something, provided enough meta info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(str2stMp, str2stMpWithOmit, showStr2stMp)
str2stMpWithOmit :: (Show opt, Enum opt, Bounded opt, Eq opt) => [opt] -> Map.Map String opt
str2stMpWithOmit omits = Map.fromList [ (show o, o) | o <- [minBound .. maxBound] \\ omits ]

str2stMp :: (Show opt, Enum opt, Bounded opt, Eq opt) => Map.Map String opt
str2stMp = str2stMpWithOmit []

showStr2stMp :: Map.Map String opt -> String
showStr2stMp = concat . intersperse " " . Map.keys
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
deriving instance Data KnownPrim
deriving instance Typeable KnownPrim
%%]

%%[50
deriving instance Typeable VarUIDHsName
deriving instance Data VarUIDHsName

deriving instance Typeable TagDataInfo
deriving instance Data TagDataInfo

deriving instance Typeable Fixity
deriving instance Data Fixity

deriving instance Typeable1 AlwaysEq
deriving instance Data x => Data (AlwaysEq x)

deriving instance Typeable PredOccId
deriving instance Data PredOccId

deriving instance Typeable CLbl
deriving instance Data CLbl

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Binary KnownPrim where
  put = putEnum8
  get = getEnum8

instance Serialize KnownPrim where
  sput = sputPlain
  sget = sgetPlain

instance Serialize TagDataInfo where
  sput (TagDataInfo a b) = sput a >> sput b
  sget = liftM2 TagDataInfo sget sget

instance Serialize VarUIDHsName where
  sput (VarUIDHs_Name a b) = sputWord8 0 >> sput a >> sput b
  sput (VarUIDHs_UID  a  ) = sputWord8 1 >> sput a
  sput (VarUIDHs_Var  a  ) = sputWord8 2 >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM2 VarUIDHs_Name sget sget
              1 -> liftM  VarUIDHs_UID  sget
              2 -> liftM  VarUIDHs_Var  sget

instance Serialize CLbl where
  sput (CLbl_Nm   a  ) = sputWord8 0 >> sput a
  sput (CLbl_Tag  a  ) = sputWord8 1 >> sput a
  sput (CLbl_None    ) = sputWord8 2
  sget = do t <- sgetWord8
            case t of
              0 -> liftM  CLbl_Nm 	sget
              1 -> liftM  CLbl_Tag  sget
              2 -> return CLbl_None

instance Binary Fixity where
  put = putEnum8
  get = getEnum8

instance Serialize Fixity where
  sput = sputPlain
  sget = sgetPlain

instance Binary x => Binary (AlwaysEq x) where
  put (AlwaysEq x) = put x
  get = liftM AlwaysEq get

instance Serialize x => Serialize (AlwaysEq x) where
  sput (AlwaysEq x) = sput x
  sget = liftM AlwaysEq sget

instance Binary PredOccId where
  put (PredOccId a) = put a
  get = liftM PredOccId get

instance Serialize PredOccId where
  sput = sputPlain
  sget = sgetPlain

%%]

