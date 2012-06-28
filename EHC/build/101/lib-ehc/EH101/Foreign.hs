

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Foreign.ag)
module EH101.Foreign(ForeignEnt (..), CCall (..), PlainCall (..), PrimCall (..), JavaScriptCall (..)
, ForeignExpr (..), ForeignExprs
, ForeignExprAGItf (..), ForeignAGItf (..)
, foreignexprEval
, ForeignDirection (..)) where

import EH101.Base.Common
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH.Util.Utils
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import Data.Typeable (Typeable)
import Data.Generics (Data)



















-- | evaluation of a ForeignExpr has arguments, known by index [1..], and a foreign entity

data IE e
  = IE { ieArgs         :: [e]
       }

data OE e
  = OE { oeEL           :: [e]
       , oeUsed         :: Set.Set Int
       }

mkOE :: e -> Set.Set Int -> OE e
mkOE e s = OE [e] s

oeE :: OE e -> e
oeE = head . oeEL

foreignexprEval
  ::                            -- the (partial) algebra for constructing an e
        ( e -> e -> e           -- select
        , e -> e -> e           -- index
        , e -> [e] -> e         -- call
        , e -> e                -- as var/ptr
        , [e] -> e              -- object
        , e -> e                -- new object
        , String -> e           -- name
        , String -> e           -- string
        )
     -> ForeignExpr
     -> e                       -- ent
     -> [e]                     -- args
     -> e
foreignexprEval
     (mkSel,mkInx,mkCall,mkPtr,mkObj,mkNewObj,mkNm,mkStr)
     fexpr ent args
  = oeE oe
  where argl = zip [1..] args
        env  = Map.fromList argl
        oe   = ev fexpr (IE [a | (i,a) <- argl, not $ i `Set.member` oeUsed oe])
        ev (ForeignExpr_Ent    ) ie =   let
                                        in  mkOE ent
                                                 ( Set.empty )
        ev (ForeignExpr_EntNm n) ie =   let
                                        in  mkOE ( mkNm n )
                                                 ( Set.empty )
        ev (ForeignExpr_Str   s) ie =   let
                                        in  mkOE ( mkStr s )
                                                 ( Set.empty )
        ev (ForeignExpr_Arg a  ) ie =   let
                                        in  mkOE ( Map.findWithDefault (panic "foreignexprEval.Arg") a env )
                                                 ( Set.singleton a )
        ev (ForeignExpr_AllArg ) ie =   let
                                        in  OE ( ieArgs ie )
                                               ( Set.empty )
        ev (ForeignExpr_Sel e s) ie =   let eo = ev e ie
                                            so = ev s ie
                                        in  mkOE ( mkSel (oeE eo) (oeE so) )
                                                 ( oeUsed eo `Set.union` oeUsed so )
        ev (ForeignExpr_Inx e i) ie =   let eo = ev e ie
                                            io = ev i ie
                                        in  mkOE ( mkInx (oeE eo) (oeE io) )
                                                 ( oeUsed eo `Set.union` oeUsed io )
        ev (ForeignExpr_Ptr e  ) ie =   let eo = ev e ie
                                        in  eo {oeEL = [mkPtr $ oeE eo]}
        ev (ForeignExpr_Call e ) ie =   let eo = ev e ie
                                        in  eo {oeEL = [mkCall (oeE eo) (ieArgs ie)]}
        ev (ForeignExpr_CallArgs f a)
                                 ie =   let fo = ev  f ie
                                            ao = evs a ie
                                        in  mkOE ( mkCall (oeE fo) (concatMap oeEL ao) )
                                               ( Set.unions $ oeUsed fo : map oeUsed ao )
        ev (ForeignExpr_ObjData) ie =   let
                                        in  mkOE ( mkObj (ieArgs ie) )
                                                 ( Set.empty )
        ev (ForeignExpr_NewObj e) ie =  let eo = ev e ie
                                        in eo { oeEL = [mkNewObj $ oeE eo] }

        evs feL                  ie =   map (\e -> ev e ie) feL



data ForeignDirection
  = ForeignDirection_Import
  | ForeignDirection_Export
  deriving (Eq,Ord)



instance Serialize ForeignExpr where
  sput (ForeignExpr_Ent         ) = sputWord8 0
  sput (ForeignExpr_EntNm    a  ) = sputWord8 1  >> sput a
  sput (ForeignExpr_Arg      a  ) = sputWord8 2  >> sput a
  sput (ForeignExpr_Sel      a b) = sputWord8 3  >> sput a >> sput b
  sput (ForeignExpr_Inx      a b) = sputWord8 4  >> sput a >> sput b
  sput (ForeignExpr_Ptr      a  ) = sputWord8 5  >> sput a
  sput (ForeignExpr_Call     a  ) = sputWord8 6  >> sput a
  sput (ForeignExpr_CallArgs a b) = sputWord8 7  >> sput a >> sput b
  sput (ForeignExpr_AllArg      ) = sputWord8 8
  sput (ForeignExpr_Str      a  ) = sputWord8 9  >> sput a
  sput (ForeignExpr_ObjData     ) = sputWord8 10
  sput (ForeignExpr_NewObj   a  ) = sputWord8 11 >> sput a
  sget = do
    t <- sgetWord8
    case t of
        0  -> return ForeignExpr_Ent
        1  -> liftM  ForeignExpr_EntNm       sget
        2  -> liftM  ForeignExpr_Arg         sget
        3  -> liftM2 ForeignExpr_Sel         sget sget
        4  -> liftM2 ForeignExpr_Inx         sget sget
        5  -> liftM  ForeignExpr_Ptr         sget
        6  -> liftM  ForeignExpr_Call        sget
        7  -> liftM2 ForeignExpr_CallArgs    sget sget
        8  -> return ForeignExpr_AllArg
        9  -> liftM  ForeignExpr_Str         sget
        10 -> return ForeignExpr_ObjData
        11 -> liftM  ForeignExpr_NewObj      sget


instance Serialize ForeignEnt where
  sput (ForeignEnt_CCall        	a) = sputWord8 0 >> sput a
  sput (ForeignEnt_PlainCall    	a) = sputWord8 1 >> sput a
  sput (ForeignEnt_PrimCall     	a) = sputWord8 2 >> sput a
  sput (ForeignEnt_JavaScriptCall  	a) = sputWord8 3 >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM ForeignEnt_CCall       	sget
              1 -> liftM ForeignEnt_PlainCall   	sget
              2 -> liftM ForeignEnt_PrimCall    	sget
              3 -> liftM ForeignEnt_JavaScriptCall 	sget

instance Serialize CCall where
  sput (CCall_Id      a b c d) = sputWord8 0 >> sput a >> sput b >> sput c >> sput d
  sput (CCall_Dynamic        ) = sputWord8 1
  sput (CCall_Wrapper        ) = sputWord8 2
  sget = do t <- sgetWord8
            case t of
              0 -> liftM4 CCall_Id      sget sget sget sget
              1 -> return CCall_Dynamic
              2 -> return CCall_Wrapper

instance Serialize PlainCall where
  sput (PlainCall_Id      a) = sputWord8 0 >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM  PlainCall_Id sget

instance Serialize PrimCall where
  sput (PrimCall_Id      a b) = sputWord8 0 >> sput a >> sput b
  sget = do t <- sgetWord8
            case t of
              0 -> liftM2 PrimCall_Id sget sget

{-
instance Serialize JavaScriptCall where
  sput (JavaScriptCall_Id      a b c d) = sputWord8 0 >> sput a >> sput b >> sput c >> sput d
  sget = do t <- sgetWord8
            case t of
              0 -> liftM4 JavaScriptCall_Id sget sget sget sget
-}
instance Serialize JavaScriptCall where
  sput (JavaScriptCall_Id          a b) = sputWord8 0 >> sput a >> sput b
  sput (JavaScriptCall_Dynamic        ) = sputWord8 1
  sput (JavaScriptCall_Wrapper        ) = sputWord8 2
  sget = do t <- sgetWord8
            case t of
              0 -> liftM2 JavaScriptCall_Id sget sget
              1 -> return JavaScriptCall_Dynamic
              2 -> return JavaScriptCall_Wrapper


-- CCall -------------------------------------------------------
data CCall  = CCall_Dynamic 
            | CCall_Id !(Bool) !((Maybe String)) !(Bool) !(String) 
            | CCall_Wrapper 
            deriving ( Data,Eq,Show,Typeable)
-- ForeignAGItf ------------------------------------------------
data ForeignAGItf  = ForeignAGItf_AGItf !(ForeignEnt ) 
-- ForeignEnt --------------------------------------------------
data ForeignEnt  = ForeignEnt_CCall !(CCall ) 
                 | ForeignEnt_JavaScriptCall !(JavaScriptCall ) 
                 | ForeignEnt_PlainCall !(PlainCall ) 
                 | ForeignEnt_PrimCall !(PrimCall ) 
                 deriving ( Data,Eq,Show,Typeable)
-- ForeignExpr -------------------------------------------------
data ForeignExpr  = ForeignExpr_AllArg 
                  | ForeignExpr_Arg !(Int) 
                  | ForeignExpr_Call !(ForeignExpr ) 
                  | ForeignExpr_CallArgs !(ForeignExpr ) !(ForeignExprs ) 
                  | ForeignExpr_Empty 
                  | ForeignExpr_Ent 
                  | ForeignExpr_EntNm !(String) 
                  | ForeignExpr_Inx !(ForeignExpr ) !(ForeignExpr ) 
                  | ForeignExpr_NewObj !(ForeignExpr ) 
                  | ForeignExpr_ObjData 
                  | ForeignExpr_Ptr !(ForeignExpr ) 
                  | ForeignExpr_Sel !(ForeignExpr ) !(ForeignExpr ) 
                  | ForeignExpr_Str !(String) 
                  deriving ( Data,Eq,Show,Typeable)
-- ForeignExprAGItf --------------------------------------------
data ForeignExprAGItf  = ForeignExprAGItf_AGItf !(ForeignExpr ) 
-- ForeignExprs ------------------------------------------------
type ForeignExprs  = [ForeignExpr ]
-- JavaScriptCall ----------------------------------------------
data JavaScriptCall  = JavaScriptCall_Dynamic 
                     | JavaScriptCall_Id !(String) !((Maybe ForeignExpr)) 
                     | JavaScriptCall_Wrapper 
                     deriving ( Data,Eq,Show,Typeable)
-- PlainCall ---------------------------------------------------
data PlainCall  = PlainCall_Id !(String) 
                deriving ( Data,Eq,Show,Typeable)
-- PrimCall ----------------------------------------------------
data PrimCall  = PrimCall_Id !(String) !((Maybe KnownPrim)) 
               deriving ( Data,Eq,Show,Typeable)