%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Representation of constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}CodeGen.Const}
%%]

%%[(8 codegen) hs import({%{EH}Base.Common},{%{EH}Base.HsName.Builtin})
%%]

%%[(8 codegen) hs import({%{EH}CodeGen.RefGenerator})
%%]

%%[(8 codegen) hs import(UHC.Util.Utils,UHC.Util.Pretty as Pretty,Data.Bits,Data.Maybe,qualified UHC.Util.FastSeq as Seq,qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[(8 codegen) hs import(Control.Monad, Control.Monad.State)
%%]

%%[(8 codegen) hs import({%{EH}Base.BasicAnnot}) export(module {%{EH}Base.BasicAnnot})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CFld, ConstRef(..))
-- | Ref as Fld for Const, made up of 2 indices, global for all constants, and per category
data ConstRef = ConstRef
  { constrefGlobal		:: !Int
  , constrefInCateg		:: !Int
  }

instance Show ConstRef where
  show r = "CRef_" ++ show (constrefGlobal r) ++ "_" ++ show (constrefInCateg r)

type CFld = Fld' ConstRef

instance RefOfFld CFld Int where
  refOfFld = constrefGlobal . fromJust . _fldInx

instance RefOfFld CFld HsName where
  refOfFld = fromJust . _fldNm
%%]

%%[(8 codegen) hs export(cfldGlobInx)
-- | Global index of CFld
cfldGlobInx :: CFld -> Int
cfldGlobInx = refOfFld
{-# INLINE cfldGlobInx #-}
%%]

%%[(8 codegen) hs export(Const(..))
-- | Representation of constant info to be stored globally, if a further codegen phase chooses to.
--   Later reference than is via a CFld.
data Const
  = Const_FFIFunction
  	  { constFFIFunNm 		:: !String
  	  }
  | Const_FFICallEncWrapper
  	  { constFFIArgSizes  	:: ![BasicSize]
  	  }
  | Const_String
  	  { constString  		:: !String
  	  }
  | Const_Nm
  	  { constNm  			:: !HsName
  	  }
  deriving(Eq,Ord,Show)
%%]

%%[(8 codegen) hs export(isFFIConst)
-- | Is FFI related Const?
isFFIConst :: Const -> Bool
isFFIConst (Const_FFIFunction 			{})	= True
isFFIConst (Const_FFICallEncWrapper 	{})	= True
isFFIConst _								= False
%%]

%%[(8 codegen) hs
-- | String representation of const category, to be used for counting and name gen
categ :: Const -> String
categ (Const_FFIFunction 		{})	= "funffi"
categ (Const_FFICallEncWrapper 	{})	= "encffi"
categ (Const_String 			{})	= "str"
categ (Const_Nm 				{})	= "nm"
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State like info for building ConstMp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(ConstSt(..), emptyConstSt)
data ConstSt = ConstSt
  { conststMp			:: !(Map.Map Const CFld)
  , conststGlobalCnt	:: !Int
  , conststInCategCntMp	:: !(Map.Map String Int)
  }

emptyConstSt = ConstSt Map.empty 0 Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Adding etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(add)
-- | Add a Const, reusing if already exists, monadically
addM :: Const -> State ConstSt CFld
addM c = do
  st <- get
  case Map.lookup c (conststMp st) of
    Just r -> return r
    _      -> do put $ st
                   { conststMp 				= Map.insert c ref (conststMp st)
                   , conststGlobalCnt 		= ginx+1
                   , conststInCategCntMp 	= Map.insert cat (cinx+1) (conststInCategCntMp st)
                   }
                 return ref
      where ginx = conststGlobalCnt st
            cat  = categ c
            cinx = Map.findWithDefault 0 cat $ conststInCategCntMp st
            ref  = Fld (Just $ mkHNm $ cat ++ show cinx) (Just $ ConstRef ginx cinx)

-- | Add a Const, reusing if already exists
add :: Const -> ConstSt -> (CFld,ConstSt)
add c = runState $ addM c

%%]

%%[(8 codegen) hs export(addStr, addNm, addFFIFun, addFFICallEncWrapper)
addStr :: String -> ConstSt -> (CFld,ConstSt)
addStr s = add (Const_String s)

addNm :: HsName -> ConstSt -> (CFld,ConstSt)
addNm nm = add (Const_Nm nm)

addFFIFun :: String -> ConstSt -> (CFld,ConstSt)
addFFIFun nm = add (Const_FFIFunction nm)

addFFICallEncWrapper :: [BasicSize] -> ConstSt -> (CFld,ConstSt)
addFFICallEncWrapper szs = add (Const_FFICallEncWrapper szs)
%%]

