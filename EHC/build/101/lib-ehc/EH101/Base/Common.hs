module EH101.Base.Common
( module EH101.Base.HsName
, module EH101.Base.UID
, module EH101.Base.AssocL
, ppSpaced, ppCon, ppCmt
, ParNeed (..), ParNeedL, parNeedApp
, Fixity (..)
, Range (..), emptyRange, builtinRange, mkRange1, mkRange2
, NmLev, nmLevAbsent, nmLevBuiltin, nmLevOutside, nmLevModule
, tokMkQName, tokMkQNames, tokMkInt, tokMkStr
, SemApp (..)
, mkRngProdOpt
, mkRngParApp
, ppAppTop
, ppParNeed
, CompilePoint (..)
, fixityMaxPrio
, isEmptyRange
, hsnLclSupply, hsnLclSupplyWith
, AlwaysEq (..)
, unions
, withLkupLiftCyc1, withLkupChkVisitLift, withLkupLift
, lookupLiftCycMb1, lookupLiftCycMb2
, MetaLev
, listCombineUniq
, metaLevVal
, rangeUnion, rangeUnions
, metaLevTy, metaLevKi, metaLevSo
, uidHNm, uidQualHNm
, ppFld, mkPPAppFun, mkPPAppFun'
, mkExtAppPP, mkExtAppPP'
, rowCanonOrderBy
, putCompileMsg
, showPP, ppPair, ppFM
, ctag, ppCTag, ppCTagInt
, module EH101.Base.Strictness
, ppHsnNonAlpha, ppHsnEscaped, hsnEscapeeChars
, writePP, writeToFile
, CTag (..), ctagIsRec, ctagTag, ctagChar, ctagInt, emptyCTag
, Unbox (..)
, CTagsMp, emptyCTagsMp
, replicateBy
, strPadLeft, strBlankPad
, Verbosity (..)
, splitByRadix
, strHex
, Backend (..)
, Presence (..)
, fmap2Tuple
, genNmMap
, MaybeOk (..), isJustOk, isNotOk, maybeOk, fromJustOk, fromNotOk
, KnownPrim (..)
, allKnownPrimMp
, ppListV
, snd3, thd
, PredOccId (..)
, mkPrId, poiHNm
, mkPrIdCHR
, emptyPredOccId
, mkClassCTag
, CHRScoped (..)
, InstVariant (..)
, VarUIDHsName (..), vunmNm
, vunmMbVar
, combineToDistinguishedElts
, RLList (..)
, rllConcat, rllSingleton, rllEmpty, rllToList, rllFromList
, rllLength, rllNull
, rllIsPrefixOf
, rllInits, rllInit, rllInitLast
, rllHeadTail
, fixityAppPrio
, InstDerivingFrom (..)
, SrcConst (..)
, ppAppTop'
, RngLiftArg, rngLift, rngAntilift
, PkgName
, graphVisit )
where
import UU.Scanner.Position
import EH.Util.Utils
import EH101.Base.HsName
import EH101.Base.Builtin
import EH101.Base.UID
import EH101.Base.AssocL
import EH.Util.Pretty
import Data.List
import Control.Applicative ((<|>))
import EH.Util.ScanUtils
import qualified Data.Set as Set
import EH101.Scanner.Token
import EH.Util.FPath
import System.IO
import System.Environment
import System.Exit
import Data.Char
import Data.Maybe
import Numeric
import qualified Data.Map as Map
import EH101.Base.Strictness
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import EH101.Base.Hashable












{-# LINE 101 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 134 "src/ehc/Base/Common.chs" #-}
newtype PredOccId
  = PredOccId
      { poiId       :: UID
      }
  deriving (Show,Eq,Ord)

{-# LINE 142 "src/ehc/Base/Common.chs" #-}
mkPrId :: UID -> PredOccId
mkPrId u = PredOccId u

poiHNm :: PredOccId -> HsName
poiHNm = uidHNm . poiId

{-# LINE 150 "src/ehc/Base/Common.chs" #-}
mkPrIdCHR :: UID -> PredOccId
mkPrIdCHR = mkPrId

{-# LINE 155 "src/ehc/Base/Common.chs" #-}
emptyPredOccId :: PredOccId
emptyPredOccId = mkPrId uidStart

{-# LINE 164 "src/ehc/Base/Common.chs" #-}
class SemApp a where
  -- basic semantics
  semApp            ::  a -> a -> a
  semAppTop         ::  a -> a
  semVar            ::  (Position n,HSNM n) => n -> a
  semCon            ::  (Position n,HSNM n) => n -> a
  semParens         ::  a -> a
  -- basic semantics with Range
  semRngApp         ::  Range -> a -> a -> a
  semRngAppTop      ::  Range -> a -> a
  semRngVar         ::  (Position n,HSNM n) => Range -> n -> a
  semRngCon         ::  (Position n,HSNM n) => Range -> n -> a
  semRngParens      ::  Range -> a -> a
  -- constructing
  mkApp             ::  [a] -> a
  mkConApp          ::  (Position n,HSNM n) => n -> [a] -> a
  mkProdApp         ::  [a] -> a
  mk1Arrow          ::  a -> a -> a
  mkArrow           ::  [a] -> a -> a
  mk1App            ::  a -> a -> a
  mk1ConApp         ::  (Position n,HSNM n) => n -> a -> a
  -- constructin with Range
  mk1RngApp         ::  Range -> a -> a -> a
  mkRngApp          ::  Range -> [a] -> a
  mkRngProd         ::  Range -> [a] -> a

  -- inspection/deconstruction
  unTop				:: a -> a
  isCon             :: a -> Maybe (HsName)
  isApp1            :: a -> Maybe (a,a)
  isApp             :: a -> Maybe (a,[a])
  isConApp          :: a -> Maybe (HsName,[a])
  isArrow           :: a -> Maybe ([a],a)
  unArrow           :: a -> ([a],a)

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
  mkRngProd rng     =   mkProdApp               -- to be done
  mk1RngApp rng a r =   mkRngApp rng [a,r]
  mkRngApp  rng as  =   case as of
                          [a] -> a
                          _   -> semRngAppTop rng (foldl1 (semRngApp rng) as)

  -- default inspection
  unTop             = id
  isCon             = const Nothing
  isApp1            = const Nothing
  isApp     x       = do { (f1,a) <- isApp1 $ unTop x
                         ; (do {(f2,as) <- isApp f1; return (f2,as++[a])}) <|> (return (f1,[a]))
                         }
  isConApp  x       = do { (f,as) <- isApp x
                         ; c <- isCon f
                         ; return (c,as)
                         }
  unArrow   x       = case isApp x of
                        Just (fx,asx) -> case isCon fx of
                                           Just con | hsnIsArrow con -> (arg:as,r)
                                                                     where [arg,res] = asx
                                                                           (as,r) = unArrow res
                                           _                         -> dflt
                        _             -> dflt
                    where dflt = ([],x)
  isArrow   x       = case unArrow x of
                        a@((_:_),_) -> Just a
                        _           -> Nothing


{-# LINE 255 "src/ehc/Base/Common.chs" #-}
mkRngProdOpt :: SemApp e => Range -> [e] -> e
mkRngProdOpt r [e] = e
mkRngProdOpt r es  = mkRngProd r es

{-# LINE 261 "src/ehc/Base/Common.chs" #-}
mkRngParApp :: SemApp e => Range -> [e] -> e
mkRngParApp r [a] = a
mkRngParApp r as  = semRngParens r (mkRngApp r as)

{-# LINE 271 "src/ehc/Base/Common.chs" #-}
ppAppTop :: PP arg => (HsName,arg) -> [arg] -> PP_Doc -> PP_Doc
ppAppTop (conNm,con) argL dflt
  =  if       (  hsnIsArrow conNm
              || hsnIsPrArrow conNm
              ) && length argL == 2
                                then  ppListSep "" "" (" " >|< con >|< " ") argL
     else if  hsnIsProd  conNm  then  ppParensCommas argL
     else if  hsnIsList  conNm  then  ppBracketsCommas argL
     else if  hsnIsRec   conNm  then  ppListSep (hsnORec >|< con) hsnCRec "," argL
     else if  hsnIsSum   conNm  then  ppListSep (hsnOSum >|< con) hsnCSum "," argL
     else if  hsnIsRow   conNm  then  ppListSep (hsnORow >|< con) hsnCRow "," argL
                                else  dflt

{-# LINE 292 "src/ehc/Base/Common.chs" #-}
ppAppTop' :: PP arg => (HsName,arg) -> [arg] -> [Bool] -> PP_Doc -> PP_Doc
ppAppTop' cc@(conNm,_) [_,a] [True,_] _ | hsnIsArrow conNm || hsnIsPrArrow conNm    = pp a
ppAppTop' cc argL _ dflt                                                            = ppAppTop cc argL dflt

{-# LINE 298 "src/ehc/Base/Common.chs" #-}
ppCon :: HsName -> PP_Doc
ppCon nm =  if    hsnIsProd nm
            then  ppParens (text (replicate (hsnProdArity nm - 1) ','))
            else  pp nm

ppCmt :: PP_Doc -> PP_Doc
ppCmt p = "{-" >#< p >#< "-}"

{-# LINE 308 "src/ehc/Base/Common.chs" #-}

ppSpaced :: PP a => [a] -> PP_Doc
ppSpaced = ppListSep "" "" " "


{-# LINE 315 "src/ehc/Base/Common.chs" #-}
ppFld :: String -> Maybe HsName -> HsName -> PP_Doc -> PP_Doc -> PP_Doc
ppFld sep positionalNm nm nmPP f
  = case positionalNm of
      Just pn | pn == nm -> f
      _                  -> nmPP >#< sep >#< f

mkPPAppFun' :: String -> HsName -> PP_Doc -> PP_Doc
mkPPAppFun' sep c p = if c == hsnRowEmpty then empty else p >|< sep

mkPPAppFun :: HsName -> PP_Doc -> PP_Doc
mkPPAppFun = mkPPAppFun' "|"

{-# LINE 329 "src/ehc/Base/Common.chs" #-}
mkExtAppPP' :: String -> (HsName,PP_Doc,[PP_Doc]) -> (HsName,PP_Doc,[PP_Doc],PP_Doc) -> (PP_Doc,[PP_Doc])
mkExtAppPP' sep (funNm,funNmPP,funPPL) (argNm,argNmPP,argPPL,argPP)
  =  if hsnIsRec funNm || hsnIsSum funNm
     then (mkPPAppFun' sep argNm argNmPP,argPPL)
     else (funNmPP,funPPL ++ [argPP])

mkExtAppPP :: (HsName,PP_Doc,[PP_Doc]) -> (HsName,PP_Doc,[PP_Doc],PP_Doc) -> (PP_Doc,[PP_Doc])
mkExtAppPP = mkExtAppPP' "|"

{-# LINE 340 "src/ehc/Base/Common.chs" #-}
instance (PP a, PP b) => PP (a,b) where
  pp (a,b) = ppParensCommas' [pp a,pp b]

{-# LINE 345 "src/ehc/Base/Common.chs" #-}
ppPair :: (PP a, PP b) => (a,b) -> PP_Doc
ppPair (x,y) = ppParens (pp x >|< "," >|< pp y)

{-# LINE 350 "src/ehc/Base/Common.chs" #-}
showPP :: PP a => a -> String
showPP x = disp (pp x) 100 ""

{-# LINE 355 "src/ehc/Base/Common.chs" #-}
ppFM :: (PP k,PP v) => Map.Map k v -> PP_Doc
ppFM = ppAssocL . Map.toList

{-# LINE 360 "src/ehc/Base/Common.chs" #-}
ppListV :: PP a => [a] -> PP_Doc
ppListV = vlist . map pp

{-# LINE 369 "src/ehc/Base/Common.chs" #-}
putCompileMsg :: Verbosity -> Verbosity -> String -> Maybe String -> HsName -> FPath -> IO ()
putCompileMsg v optsVerbosity msg mbMsg2 modNm fNm
  = if optsVerbosity >= v
    then do { hPutStrLn stdout (strBlankPad 40 msg ++ " " ++ strBlankPad 22 (show modNm) ++ " (" ++ fpathToStr fNm ++ maybe "" (\m -> ", " ++ m) mbMsg2 ++ ")")
            ; hFlush stdout
            }
    else return ()

{-# LINE 379 "src/ehc/Base/Common.chs" #-}
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


{-# LINE 404 "src/ehc/Base/Common.chs" #-}
data ParNeed =  ParNotNeeded | ParNeededLow | ParNeeded | ParNeededHigh | ParOverrideNeeded
                deriving (Eq,Ord)

type ParNeedL = [ParNeed]

parNeedApp :: HsName -> (ParNeed,ParNeedL)
parNeedApp conNm
  =  let  pr  | hsnIsArrow  conNm   =  (ParNeededLow,[ParNotNeeded,ParNeeded])
              | hsnIsProd   conNm   =  (ParOverrideNeeded,repeat ParNotNeeded)
              | hsnIsList   conNm   =  (ParOverrideNeeded,[ParNotNeeded])
              | hsnIsRec    conNm   =  (ParOverrideNeeded,[ParNotNeeded])
              | hsnIsSum    conNm   =  (ParOverrideNeeded,[ParNotNeeded])
              | hsnIsRow    conNm   =  (ParOverrideNeeded,repeat ParNotNeeded)
              | otherwise           =  (ParNeeded,repeat ParNeededHigh)
     in   pr

{-# LINE 426 "src/ehc/Base/Common.chs" #-}
ppParNeed :: PP p => ParNeed -> ParNeed -> p -> PP_Doc
ppParNeed locNeed globNeed p
  = par (pp p)
  where par = if globNeed > locNeed then ppParens else id

{-# LINE 452 "src/ehc/Base/Common.chs" #-}
data CTag
  = CTagRec
  | CTag
      { ctagTyNm        :: !HsName
      , ctagNm          :: !HsName
      , ctagTag'        :: !Int
      , ctagArity       :: !Int
      , ctagMaxArity    :: !Int
      }
  deriving (Show,Eq,Ord)

ctagIsRec :: CTag -> Bool
ctagIsRec CTagRec = True
ctagIsRec t       = False

ctagTag :: CTag -> Int
ctagTag CTagRec = 0
ctagTag t       = ctagTag' t

ctagInt  =  CTag hsnInt  hsnInt  0 1 1
ctagChar =  CTag hsnChar hsnChar 0 1 1

emptyCTag = CTag hsnUnknown hsnUnknown 0 0 0

{-# LINE 478 "src/ehc/Base/Common.chs" #-}
-- only used when `not ehcCfgClassViaRec'
mkClassCTag :: HsName -> Int -> CTag
mkClassCTag n sz = CTag n n 0 sz sz

{-# LINE 484 "src/ehc/Base/Common.chs" #-}
ctag :: a -> (HsName -> HsName -> Int -> Int -> Int -> a) -> CTag -> a
ctag n t tg = case tg of {CTag tn cn i a ma -> t tn cn i a ma; _ -> n}

ppCTag :: CTag -> PP_Doc
ppCTag = ctag (pp "Rec") (\tn cn t a ma -> pp t >|< "/" >|< pp cn >|< "/" >|< pp a >|< "/" >|< pp ma)

ppCTagInt :: CTag -> PP_Doc
ppCTagInt = ctag (pp "-1") (\_ _ t _ _ -> pp t)

instance PP CTag where
  pp = ppCTag

{-# LINE 502 "src/ehc/Base/Common.chs" #-}
data Unbox
  = Unbox_FirstField
  | Unbox_Tag         !Int
  | Unbox_None

{-# LINE 513 "src/ehc/Base/Common.chs" #-}
type CTagsMp = AssocL HsName (AssocL HsName CTag)

emptyCTagsMp :: CTagsMp
emptyCTagsMp = []

{-# LINE 524 "src/ehc/Base/Common.chs" #-}
unions :: Eq a => [[a]] -> [a]
unions = foldr union []

{-# LINE 529 "src/ehc/Base/Common.chs" #-}
listCombineUniq :: Eq a => [[a]] -> [a]
listCombineUniq = nub . concat

{-# LINE 549 "src/ehc/Base/Common.chs" #-}
replicateBy :: [a] -> b -> [b]
replicateBy l e = replicate (length l) e

{-# LINE 558 "src/ehc/Base/Common.chs" #-}
strPadLeft :: Char -> Int -> String -> String
strPadLeft c n s = replicate (n - length s) c ++ s

strBlankPad :: Int -> String -> String
strBlankPad n s = s ++ replicate (n - length s) ' '

{-# LINE 566 "src/ehc/Base/Common.chs" #-}
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd :: (a,b,c) -> c
thd (a,b,c) = c

{-# LINE 578 "src/ehc/Base/Common.chs" #-}
data Verbosity
  = VerboseQuiet | VerboseMinimal | VerboseNormal | VerboseALot | VerboseDebug
  deriving (Eq,Ord,Enum)

{-# LINE 588 "src/ehc/Base/Common.chs" #-}
data CHRScoped
  = CHRScopedInstOnly | CHRScopedMutualSuper | CHRScopedAll
  deriving (Eq,Ord)

{-# LINE 598 "src/ehc/Base/Common.chs" #-}
data CompilePoint
  = CompilePoint_Imports
  | CompilePoint_Parse
  | CompilePoint_AnalHS
  | CompilePoint_AnalEH
  | CompilePoint_Core
  | CompilePoint_All
  deriving (Eq,Ord,Show)

{-# LINE 615 "src/ehc/Base/Common.chs" #-}
data Fixity
  = Fixity_Infix | Fixity_Infixr | Fixity_Infixl
  deriving (Eq,Ord,Show,Enum)

instance PP Fixity where
  pp Fixity_Infix  = pp "infix"
  pp Fixity_Infixl = pp "infixl"
  pp Fixity_Infixr = pp "infixr"

{-# LINE 626 "src/ehc/Base/Common.chs" #-}
fixityMaxPrio :: Int
fixityMaxPrio = 9

{-# LINE 631 "src/ehc/Base/Common.chs" #-}
fixityAppPrio :: Int
fixityAppPrio = fixityMaxPrio + 1

{-# LINE 640 "src/ehc/Base/Common.chs" #-}
instance Eq Pos where
  p1 == p2 = line p1 == line p2 && column p1 == column p2

instance Ord Pos where
  compare p1 p2
    = case compare (line p1) (line p2) of
        EQ -> compare (column p1) (column p2)
        c  -> c

{-# LINE 655 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 677 "src/ehc/Base/Common.chs" #-}
show2Pos :: Pos -> Pos -> String
show2Pos p1 p2
  | p1 /= p2 && p2 /= noPos  = if line p1 == line p2
                               then mk (show (line p1))                          (Just $ show (column p1) ++ "-" ++ show (column p2))
                               else mk (show (line p1) ++ "-" ++ show (line p2)) Nothing
  | otherwise                =      mk (show (line p1))                          (Just $ show (column p1))
  where mk l c = file p1 ++ ":" ++ l ++ maybe "" (":" ++) c

{-# LINE 687 "src/ehc/Base/Common.chs" #-}
instance Show Range where
  show (Range_Range p q) = show2Pos p q
  show Range_Unknown     = "??"
  show Range_Builtin     = "builtin"

instance PP Range where
  pp = pp . show

{-# LINE 697 "src/ehc/Base/Common.chs" #-}
isEmptyRange :: Range -> Bool
isEmptyRange  Range_Unknown    = True
isEmptyRange (Range_Range p _) = p == noPos
isEmptyRange  _                = False

{-# LINE 706 "src/ehc/Base/Common.chs" #-}
instance Eq Range where
  _ == _ = True             -- a Range is ballast, not a criterium to decide equality for

instance Ord Range where
  _ `compare` _ = EQ        -- a Range is ballast, not a criterium to decide equality for

{-# LINE 714 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 727 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 758 "src/ehc/Base/Common.chs" #-}
type RngLiftArg  x = Range -> x
type RngLift     x = Range -> RngLiftArg x -> x

rngLift :: RngLift v
rngLift r mkv
  = x `seq` x
  where x = mkv r

rngAntilift :: v -> RngLiftArg v
rngAntilift = const

{-# LINE 775 "src/ehc/Base/Common.chs" #-}
data InstVariant
  = InstNormal | InstDefault
  | InstDeriving InstDerivingFrom
  deriving (Eq,Ord,Show)

{-# LINE 784 "src/ehc/Base/Common.chs" #-}
-- | Either a deriving combined from a datatype directly or a standalone
data InstDerivingFrom
  = InstDerivingFrom_Datatype
  | InstDerivingFrom_Standalone
  deriving (Eq,Ord,Show)

{-# LINE 796 "src/ehc/Base/Common.chs" #-}
type NmLev = Int

nmLevAbsent, nmLevBuiltin, nmLevOutside, nmLevModule :: NmLev
nmLevAbsent  = -3
nmLevBuiltin = -2
nmLevOutside = -1
nmLevModule  =  0


{-# LINE 817 "src/ehc/Base/Common.chs" #-}
-- Assumption: tokTpIsInt (genTokTp t) == True
tokMkInt :: Token -> Int
tokMkInt t
  = case genTokTp t of
      Just TkInteger10 -> read v
      _                -> 0
  where v = tokenVal t

tokMkStr :: Token -> String
tokMkStr = tokenVal

{-# LINE 835 "src/ehc/Base/Common.chs" #-}
tokMkQName :: Token -> HsName
tokMkQName t
  = case genTokTp t of
      Just tp | tokTpIsInt tp -> mkHNmPos $ tokMkInt t
      _                       -> mkHNm $ map hsnFromString $ tokenVals t

{-# LINE 845 "src/ehc/Base/Common.chs" #-}
tokMkQNames :: [Token] -> [HsName]
tokMkQNames = map tokMkQName

instance HSNM Token where
  mkHNm = tokMkQName

{-# LINE 857 "src/ehc/Base/Common.chs" #-}
hsnLclSupplyWith :: HsName -> [HsName]
hsnLclSupplyWith n = map (\i -> hsnSuffix n $ "_" ++ show i) [1..]

hsnLclSupply :: [HsName]
hsnLclSupply = hsnLclSupplyWith (hsnFromString "")

{-# LINE 869 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 886 "src/ehc/Base/Common.chs" #-}
strHex :: (Show a, Integral a) => Int -> a -> String
strHex prec x
  = replicate (prec - length h) '0' ++ h
  where h = showHex x []

{-# LINE 897 "src/ehc/Base/Common.chs" #-}
data Backend
  = BackendGrinByteCode
  | BackendSilly
  deriving (Eq, Ord)


{-# LINE 993 "src/ehc/Base/Common.chs" #-}
data VarUIDHsName
  = VarUIDHs_Name       { vunmId :: !UID, vunmNm' :: !HsName }
  | VarUIDHs_UID        { vunmId :: !UID }
  | VarUIDHs_Var        !UID
  deriving (Eq, Ord)

vunmNm :: VarUIDHsName -> HsName
vunmNm (VarUIDHs_Name _ n) = n
vunmNm (VarUIDHs_UID  i  ) = mkHNm i
vunmNm _                   = panic "Common.assnmNm"

{-# LINE 1006 "src/ehc/Base/Common.chs" #-}
vunmMbVar :: VarUIDHsName -> Maybe UID
vunmMbVar (VarUIDHs_Var v) = Just v
vunmMbVar _                = Nothing

{-# LINE 1012 "src/ehc/Base/Common.chs" #-}
instance Show VarUIDHsName where
  show (VarUIDHs_Name _ n) = show n
  show (VarUIDHs_UID  i  ) = show i
  show (VarUIDHs_Var  i  ) = show i

instance PP VarUIDHsName where
  pp a = pp $ show a

{-# LINE 1026 "src/ehc/Base/Common.chs" #-}
withLkupLiftCyc2 :: (t -> Maybe UID) -> (t -> UIDS) -> (UID -> Maybe t) -> x -> (UIDS -> t -> x) -> (t -> x) -> UIDS -> UID -> x
withLkupLiftCyc2 get noVisit lookup dflt yes no vsVisited v
  = case lookup v of
      Just t | not (v `Set.member` vsVisited)
        -> yes (Set.insert v $ Set.union (noVisit t) vsVisited) t
      _ -> dflt

{-# LINE 1035 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1050 "src/ehc/Base/Common.chs" #-}
lookupLiftCyc1 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x' -> (x->x') -> x -> x'
lookupLiftCyc1 get lookup dflt found x
  = lk Set.empty dflt found x
  where lk s dflt found x = withLkupLiftCyc1 get (const Set.empty) lookup (\s t -> lk s (found t) found t) (const dflt) s x

lookupLiftCyc2 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x' -> (x->x') -> UID -> x'
lookupLiftCyc2 get lookup dflt found x
  = maybe dflt (\x -> lookupLiftCyc1 get lookup (found x) found x) $ lookup x

{-# LINE 1061 "src/ehc/Base/Common.chs" #-}
lookupLiftCycMb1 :: (x -> Maybe UID) -> (UID -> Maybe x) -> x -> Maybe x
lookupLiftCycMb1 get lookup x = lookupLiftCyc1 get lookup Nothing Just x

lookupLiftCycMb2 :: (x -> Maybe UID) -> (UID -> Maybe x) -> UID -> Maybe x
lookupLiftCycMb2 get lookup x = lookupLiftCyc2 get lookup Nothing Just x

{-# LINE 1073 "src/ehc/Base/Common.chs" #-}
data Presence = Present | Absent deriving (Eq,Ord,Show)

{-# LINE 1081 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1101 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1117 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1139 "src/ehc/Base/Common.chs" #-}
rllLength :: RLList a -> Int
rllLength (RLList l) = sum $ map snd l

rllNull :: RLList a -> Bool
rllNull (RLList []) = True
rllNull (RLList _ ) = False

{-# LINE 1148 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1161 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1176 "src/ehc/Base/Common.chs" #-}
rllHeadTail :: RLList a -> Maybe (a,RLList a)
rllHeadTail (RLList [])        = Nothing
rllHeadTail (RLList ((x,1):t)) = Just (x,RLList t)
rllHeadTail (RLList ((x,c):t)) = Just (x,RLList ((x,c-1):t))

{-# LINE 1183 "src/ehc/Base/Common.chs" #-}
instance Show a => Show (RLList a) where
  show = show . rllToList

{-# LINE 1195 "src/ehc/Base/Common.chs" #-}
data AlwaysEq a = AlwaysEq a

instance Eq (AlwaysEq a) where
  _ == _ = True

instance Ord (AlwaysEq a) where
  _ `compare` _ = EQ

instance Show a => Show (AlwaysEq a) where
  show (AlwaysEq x) = show x

instance PP a => PP (AlwaysEq a) where
  pp (AlwaysEq x) = pp x

{-# LINE 1215 "src/ehc/Base/Common.chs" #-}
type PkgName = String

{-# LINE 1235 "src/ehc/Base/Common.chs" #-}
-- order on ...
rowCanonOrderBy :: (o -> o -> Ordering) -> AssocL o a -> AssocL o a
rowCanonOrderBy cmp = sortByOn cmp fst

{-# LINE 1245 "src/ehc/Base/Common.chs" #-}
type MetaLev = Int

{-# LINE 1249 "src/ehc/Base/Common.chs" #-}
metaLevVal :: MetaLev
metaLevVal = 0

{-# LINE 1254 "src/ehc/Base/Common.chs" #-}
metaLevTy, metaLevKi, metaLevSo :: MetaLev
metaLevTy  = metaLevVal + 1
metaLevKi  = metaLevTy  + 1
metaLevSo  = metaLevKi  + 1

{-# LINE 1265 "src/ehc/Base/Common.chs" #-}
deriving instance Typeable VarUIDHsName
deriving instance Data VarUIDHsName

deriving instance Typeable Fixity
deriving instance Data Fixity

deriving instance Typeable1 AlwaysEq
deriving instance Data x => Data (AlwaysEq x)

deriving instance Typeable PredOccId
deriving instance Data PredOccId

deriving instance Typeable1 RLList
deriving instance Data x => Data (RLList x)

deriving instance Typeable CTag
deriving instance Data CTag

deriving instance Typeable Range
deriving instance Data Range

deriving instance Typeable Pos
deriving instance Data Pos


{-# LINE 1296 "src/ehc/Base/Common.chs" #-}
uidHNm :: UID -> HsName
uidHNm = mkHNm -- hsnFromString . show

{-# LINE 1301 "src/ehc/Base/Common.chs" #-}
uidQualHNm :: HsName -> UID -> HsName
uidQualHNm modnm uid =
                        hsnPrefixQual modnm $
                        uidHNm uid

{-# LINE 1310 "src/ehc/Base/Common.chs" #-}
instance HSNM UID where
  mkHNm x = hsnFromString ('_' : show x)

{-# LINE 1319 "src/ehc/Base/Common.chs" #-}
instance Serialize VarUIDHsName where
  sput (VarUIDHs_Name a b) = sputWord8 0 >> sput a >> sput b
  sput (VarUIDHs_UID  a  ) = sputWord8 1 >> sput a
  sput (VarUIDHs_Var  a  ) = sputWord8 2 >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM2 VarUIDHs_Name sget sget
              1 -> liftM  VarUIDHs_UID  sget
              2 -> liftM  VarUIDHs_Var  sget

instance Binary Fixity where
  put = putEnum8
  get = getEnum8

instance Serialize Fixity where
  sput = sputPlain
  sget = sgetPlain

instance Binary KnownPrim where
  put = putEnum8
  get = getEnum8

instance Serialize KnownPrim where
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

instance Binary a => Binary (RLList a) where
  put (RLList a) = put a
  get = liftM RLList get

instance Serialize CTag where
  sput = sputShared
  sget = sgetShared
  sputNested (CTagRec          ) = sputWord8 0
  sputNested (CTag    a b c d e) = sputWord8 1 >> sput a >> sput b >> sput c >> sput d >> sput e
  sgetNested
    = do t <- sgetWord8
         case t of
           0 -> return CTagRec
           1 -> liftM5 CTag    sget sget sget sget sget

instance Binary Range where
  put (Range_Unknown    ) = putWord8 0
  put (Range_Builtin    ) = putWord8 1
  put (Range_Range   a b) = putWord8 2 >> put a >> put b
  get = do t <- getWord8
           case t of
             0 -> return Range_Unknown
             1 -> return Range_Builtin
             2 -> liftM2 Range_Range get get

instance Serialize Range where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain

instance Binary Pos where
  put (Pos a b c) = put a >> put b >> put c
  get = liftM3 Pos get get get

{-# LINE 1402 "src/ehc/Base/Common.chs" #-}
data SrcConst
  = SrcConst_Int    Integer
  | SrcConst_Char   Char
  | SrcConst_Ratio  Integer Integer
  deriving (Eq,Show,Ord)

{-# LINE 1414 "src/ehc/Base/Common.chs" #-}
fmap2Tuple :: Functor f => snd -> f x -> f (x,snd)
fmap2Tuple snd = fmap (\x -> (x,snd))

{-# LINE 1423 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1443 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1470 "src/ehc/Base/Common.chs" #-}
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

{-# LINE 1495 "src/ehc/Base/Common.chs" #-}
data KnownPrim
  =
    -- platform Int
    KnownPrim_AddI
  | KnownPrim_SubI
  | KnownPrim_MulI

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
  deriving (Show,Eq,Enum,Bounded)

{-# LINE 1537 "src/ehc/Base/Common.chs" #-}
deriving instance Data KnownPrim
deriving instance Typeable KnownPrim

{-# LINE 1542 "src/ehc/Base/Common.chs" #-}
instance PP KnownPrim where
  pp = pp . show

{-# LINE 1547 "src/ehc/Base/Common.chs" #-}
allKnownPrimMp :: Map.Map String KnownPrim
allKnownPrimMp
  = Map.fromList [ (drop prefixLen $ show t, t) | t <- [ minBound .. maxBound ] ]
  where prefixLen = length "KnownPrim_"

