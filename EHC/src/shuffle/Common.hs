-------------------------------------------------------------------------
-- Common stuff
-------------------------------------------------------------------------

module Common
  ( module Data.Maybe
  , module Data.Char
  , module EH.Util.Nm
  , module EH.Util.FPath
  , module EH.Util.Pretty
  , module AspectExpr

  , Err(..), ErrM, ppErr, showUndef

  , openURI

  , Opts(..), defaultOpts, optsHasNoVariantRefOrder

  , URef
  , CRef, CPos(..)

  , ChKind(..), ChDest(..), ChWrap(..)

  , VariantRef(..)
  , AspectRefs(..)
  , variantReqmRef, mbVariantReqmRef
  , variantRefFromTop
  , variantReqmUpdRef
  , VariantOffer(..), VariantReqm(..)
  , variantOfferFromRef, variantReqmFromRef
  , variantOfferFromTop
  , variantOfferRef, variantOfferRefTop
  , VariantRefOrder

  , ChunkRef(..)
  , chunkRefFromOfferNm

  , variantReqmMatchOffer
  , VariantRefOrderMp, sortOnVariantRefOrderMp, sortOnVariantRefOrderMp'

  , KVMap

  , CompilerRestriction(..)
  
  , t2tChKinds
  )
  where

import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map(Map)
import Data.Set(Set)
import Network.URI
import IO
import System.Directory
import System.Console.GetOpt
import EH.Util.Pretty
import EH.Util.FPath
import EH.Util.Utils
import EH.Util.Nm
import AspectExpr
import AspectExprEval

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm     CPos String [Nm]
  | Err_UndefURI    CPos String
  | Err_Exec        CPos String String
  deriving Show

type ErrM = Map.Map CPos Err

ppErr :: CPos -> PP_Doc -> PP_Doc
ppErr pos p
  = "*** ERROR ***"
    >-< show pos >|< ":"
    >-< indent 4 p

instance PP Err where
  pp (Err_UndefNm pos knd nmL)
    = ppErr pos (knd >|< "(s) are undefined:" >#< ppCommas' nmL)
  pp (Err_UndefURI pos u)
    = ppErr pos ("could not open:" >#< u)
  pp (Err_Exec pos f e)
    = ppErr pos (   "execution of:" >#< f
                >-< "failed      :" >#< e
                )

showUndef :: Show r => r -> String
showUndef r = "<<<<" ++ show r ++ ">>>>"

-------------------------------------------------------------------------
-- URI handling
-------------------------------------------------------------------------

openURI :: URI -> IO (Maybe Handle)
openURI u
  = case uriScheme u of
      "file:" -> do { ex <- doesFileExist p
                    ; if ex
                      then do { h <- openFile p ReadMode
                              ; return (Just h)
                              }
                      else return Nothing
                    }
      _       -> return Nothing
  where p = uriPath u

-------------------------------------------------------------------------
-- Key/value pair handling
-------------------------------------------------------------------------

type KVMap = Map.Map String String

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

data Opts 
  = Opts
      { optAG           		:: Bool						-- generate AG
      , optHS           		:: Bool						-- generate Haskell
      , optPlain        		:: Bool						-- leave as is
      , optLaTeX        		:: Bool						-- generate latex
      , optPreamble     		:: Bool						-- include preamble
      , optLinePragmas  		:: Bool						-- include line pragmas
      , optIndex        		:: Bool
      , optCompiler     		:: [Int]
      , optHelp         		:: Bool
      , optGenDeps      		:: Bool
      , optGenText2Text   		:: Bool						-- include text2text text type annotation
      , optChDest       		:: (ChDest,String)
      , optGenReqm   			:: VariantReqm
      , optBaseName     		:: Maybe String
      , optBaseFPath    		:: FPath
      , optWrapLhs2tex  		:: ChWrap
      , optMbXRefExcept 		:: Maybe String
      , optVariantRefOrder		:: VariantRefOrder
      , optDefs         		:: KVMap
      , optDepNamePrefix 		:: String
      , optDepSrcVar     		:: String
      , optDepDstVar     		:: String
      , optDepMainVar    		:: String
      , optDepDpdsVar    		:: String
      , optDepOrigDpdsVar 		:: String
      , optDepBaseDir     		:: String
      , optDepTerm        		:: Map String [String]
      , optDepIgn         		:: Set String
      , optAGModHeader    		:: Bool
      } deriving (Show)

defaultOpts
  = Opts
      { optAG           		=  False
      , optHS           		=  False
      , optLaTeX        		=  False
      , optPreamble     		=  True
      , optLinePragmas  		=  False
      , optPlain        		=  False
      , optIndex        		=  False
      , optCompiler     		=  []
      , optHelp         		=  False
      , optGenDeps      		=  False
      , optGenText2Text			=  False
      , optChDest       		=  (ChHere,"")
      , optGenReqm   			=  VReqmNone
      , optBaseName     		=  Nothing
      , optBaseFPath    		=  emptyFPath
      , optWrapLhs2tex  		=  ChWrapCode
      , optMbXRefExcept 		=  Nothing
      , optVariantRefOrder	 	=  [[]]
      , optDefs					=  Map.empty
      , optDepNamePrefix 		=  error "optDepNamePrefix not set"
      , optDepSrcVar     		=  error "optDepSrcVar not set"
      , optDepDstVar     		=  error "optDepDstVar not set"
      , optDepMainVar    		=  error "optDepMainVar not set"
      , optDepDpdsVar    		=  error "optDepDpdsVar not set"
      , optDepOrigDpdsVar 		=  error "optDepOrigDpdsVar not set"
      , optDepBaseDir     		=  error "optDepBaseDir not set"
      , optDepTerm        		=  Map.empty
      , optDepIgn         		=  Set.empty
      , optAGModHeader    		=  False
      }

optsHasNoVariantRefOrder :: Opts -> Bool
optsHasNoVariantRefOrder = null . head . optVariantRefOrder

-------------------------------------------------------------------------
-- URI ref
-------------------------------------------------------------------------

type URef = String

-------------------------------------------------------------------------
-- Chunk ref, position (in file)
-------------------------------------------------------------------------

type CRef = Nm
data CPos = CPos FPath Int
          deriving (Eq,Ord)

instance Show CPos where
  show (CPos fp l) = fpathToStr fp ++ ":" ++ show l

-------------------------------------------------------------------------
-- Chunk kind, purpose/destination
-------------------------------------------------------------------------

data ChKind
  = ChAG
  | ChHS
  | ChPlain
  | ChDocLaTeX		-- restricted LaTeX for documentation
  -- | ChTexInfo
  -- | ChHtml
  -- | ChTwiki
  | ChHaddock
  deriving (Show,Eq,Ord)

data ChDest
  = ChHere | ChHide
  deriving (Show,Eq,Ord)

data ChWrap
  = ChWrapCode
  | ChWrapBoxCode 			(Maybe String)
  | ChWrapBeamerBlockCode 	String
  | ChWrapTT
  | ChWrapTTtiny
  | ChWrapVerbatim
  | ChWrapVerbatimSmall
  | ChWrapPlain
  | ChWrapT2T				ChKind				-- wrap for text2text
  | ChWrapComp				ChWrap ChWrap		-- compose
  | ChWrapNone
  deriving (Show,Eq,Ord)

-------------------------------------------------------------------------
-- For which ChKind's are text2text annotation generated, if requested
-------------------------------------------------------------------------

t2tChKinds :: Map.Map ChKind String
t2tChKinds
  = Map.fromList
      [ ( ChDocLaTeX, "doclatex" )
      ]

-------------------------------------------------------------------------
-- Variant reference
-------------------------------------------------------------------------

data VariantRef
  = VarRef 	{vrefRefs :: ![Int]}
  deriving (Show,Eq,Ord)

instance NM VariantRef where
  mkNm (VarRef l)     = nmApdL $ map mkNm l

variantRefFromTop :: Int -> VariantRef
variantRefFromTop i = VarRef [i]

-------------------------------------------------------------------------
-- Aspect reference
-------------------------------------------------------------------------

data AspectRefs
  = AspectAll
  | AspectRefs          !AspectRefReqd
  | AspectOfferExpr     !AspectExpr
  deriving (Show,Eq,Ord)

aspectRefsMatch :: AspectRefs -> AspectRefs -> Bool
aspectRefsMatch AspectAll            _               = True
aspectRefsMatch _                    AspectAll       = True
aspectRefsMatch (AspectRefs      r1) (AspectRefs r2) = Set.isSubsetOf r1 r2 -- not (Set.null (Set.intersection r1 r2))
aspectRefsMatch (AspectOfferExpr r1) (AspectRefs r2) = aspexpIsAccepted r2 r1

-------------------------------------------------------------------------
-- Variant offering, ordering
-------------------------------------------------------------------------

data VariantOfferForCompare
  = VariantOfferForCompare !Int !AspectRefs
  deriving (Eq,Ord)

-------------------------------------------------------------------------
-- Variant offering, available version
-------------------------------------------------------------------------

data VariantOffer
  = VOfferAll
  | VOfferPre
  | VOfferRef 	{vofferVariant :: !VariantRef, vofferAspect :: !AspectRefs}
  deriving (Show,Eq,Ord)

{-
instance Ord VariantOffer where
  VOfferAll         `compare` VOfferAll          = EQ
  VOfferAll         `compare` _                  = LT
  VOfferPre         `compare` VOfferPre          = EQ
  VOfferPre         `compare` _                  = LT
  _                 `compare` VOfferAll          = GT
  _                 `compare` VOfferPre          = GT
  (VOfferRef r1 a1) `compare` (VOfferRef r2 a2)  = case r1 `compare` r2 of
                    								 EQ -> case (a1,a2) of
                    								         (AspectAll,AspectAll) -> EQ
                    								         (_        ,AspectAll) ->
-}

type VariantRefOrder   = [[VariantRef]]
type VariantRefOrderMp = Map.Map VariantRef Int

variantOfferFromRef :: VariantRef -> VariantOffer
variantOfferFromRef   (VarRef (0:_ )) = VOfferPre
variantOfferFromRef r@(VarRef (i:is)) = VOfferRef r AspectAll

variantOfferFromTop :: Int -> VariantOffer
variantOfferFromTop i = variantOfferFromRef (variantRefFromTop i)

variantOfferRef :: VariantOffer -> VariantRef
variantOfferRef  VOfferPre      = VarRef [0]
variantOfferRef (VOfferRef r _) = r

variantOfferAsp :: VariantOffer -> AspectRefs
variantOfferAsp  VOfferPre      = AspectAll
variantOfferAsp (VOfferRef _ a) = a

variantOfferRefTop :: VariantOffer -> Int
variantOfferRefTop (VOfferRef (VarRef (i:_)) _) = i

{-
variantOfferIsOffered :: VariantOffer -> VariantRefOrderMp -> Bool
variantOfferIsOffered VOfferAll _ = True
variantOfferIsOffered v         s = Map.member (variantOfferRef v) s
-}

sortOnVariantRefOrderMp' :: VariantRefOrderMp -> [(VariantOffer,x)] -> [((VariantOffer,Bool),x)]
sortOnVariantRefOrderMp' m l
  = map snd
  $ sortOn fst
  $ [ ( VariantOfferForCompare (maybe 0 id o) (variantOfferAsp v)
      , ((v,isJust o || v == VOfferAll),x)
      )
    | (v,x) <- l, let o = Map.lookup (variantOfferRef v) m
    ]

sortOnVariantRefOrderMp :: VariantRefOrderMp -> [(VariantOffer,x)] -> [x]
-- sortOnVariantRefOrderMp m = map snd . sortOn fst . map (\(v,x) -> (Map.findWithDefault 0 (variantOfferRef v) m,x))
sortOnVariantRefOrderMp m vo
  = map snd
  $ sortOn fst
  $ [ ( VariantOfferForCompare o (variantOfferAsp v)
      , x
      )
    | (v,x) <- vo, let o = Map.findWithDefault 0 (variantOfferRef v) m
    ]

instance NM VariantOffer where
  mkNm VOfferPre         = mkNm "pre"
  mkNm VOfferAll         = mkNm "*"
  mkNm (VOfferRef r _)   = mkNm r

-------------------------------------------------------------------------
-- Variant selection, required version
-------------------------------------------------------------------------

data VariantReqm
  = VReqmAll
  | VReqmNone
  | VReqmRef 	{ vreqmVariant :: !VariantRef, vreqmAspects :: !AspectRefs }
  deriving (Show,Eq,Ord)

-- type VariantReqm = VariantOffer

variantReqmFromRef :: VariantRef -> VariantReqm
variantReqmFromRef r = VReqmRef r AspectAll

mbVariantReqmRef :: VariantReqm -> Maybe VariantRef
mbVariantReqmRef (VReqmRef r _) = Just r
mbVariantReqmRef _              = Nothing

variantReqmRef :: VariantReqm -> VariantRef
variantReqmRef = maybe (error "variantReqmRef") id . mbVariantReqmRef

variantReqmUpdRef :: VariantReqm -> VariantRef-> VariantReqm
variantReqmUpdRef v@(VReqmRef _ _) r = v {vreqmVariant = r}
variantReqmUpdRef v                _ = v

variantReqmMatchOffer :: Maybe VariantRefOrderMp -> VariantReqm -> VariantOffer -> Bool
variantReqmMatchOffer _        VReqmAll         _                 = True
variantReqmMatchOffer _        VReqmNone        _                 = False
variantReqmMatchOffer _        _                VOfferAll         = True
variantReqmMatchOffer Nothing  (VReqmRef rr ra) (VOfferRef or oa) = rr == or && aspectRefsMatch oa ra
variantReqmMatchOffer (Just m) (VReqmRef rr ra) (VOfferRef or oa) = Map.member or m && aspectRefsMatch oa ra

instance NM VariantReqm where
  mkNm VReqmAll          = mkNm "*"
  mkNm VReqmNone         = mkNm "-"
  mkNm (VReqmRef r _)    = mkNm r

-------------------------------------------------------------------------
-- Chunk reference
-------------------------------------------------------------------------

data ChunkRef
  = ChunkRef {chunkRefVar :: !VariantRef, chunkRefNm :: !Nm}
  deriving (Show,Eq,Ord)

chunkRefFromOfferNm :: VariantOffer -> Nm -> ChunkRef
chunkRefFromOfferNm o n = ChunkRef (variantOfferRef o) n

instance NM ChunkRef where
  mkNm (ChunkRef v n)     = mkNm v `nmApd` n

-------------------------------------------------------------------------
-- Compiler restrictions
-------------------------------------------------------------------------

data CompilerRestriction
  = Restricted (Maybe [Int]) (Maybe [Int])
  deriving Show
