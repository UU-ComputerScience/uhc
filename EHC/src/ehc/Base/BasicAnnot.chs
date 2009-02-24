%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Annotation for size info related to builtin types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}Base.BasicAnnot}
%%]

%%[(8 codegen) hs import(Data.Bits)
%%]

%%[(8 codegen) hs import(EH.Util.Pretty)
%%]

%%[(8 codegen) hs import(qualified {%{EH}Config} as Cfg)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicAnnot types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(BasicTy(..))
-- the defs in basicTyGBTy must be at the beginning, as Enum uses the relative ordering
data BasicTy
  = BasicTy_Word		-- base case: pointer, word, int, ...
%%[[97
  | BasicTy_Float		-- C: float
  | BasicTy_Double		-- C: double
%%]]
%%[[(8 jazy)
  | BasicTy_Int			-- Java: Integer/int
  | BasicTy_Char		-- Java: Character/char
  | BasicTy_Object		-- Java: Object
%%]]
  deriving (Eq,Ord,Enum)
%%]

The Show of BasicTy should returns strings of which the first letter is unique for the type.
When used to pass to code, only this first letter is used.

%%[(8 codegen) hs
instance Show BasicTy where
  show BasicTy_Word   = "word"
%%[[97
  show BasicTy_Float  = "float"
  show BasicTy_Double = "double"
%%]]
%%[[(8 jazy)
  show BasicTy_Int    = "int"
  show BasicTy_Char   = "char"
  show BasicTy_Object = "Object"
%%]]
%%]

%%[(8 codegen) export(basicGrinTyCharEncoding)
basicGrinTyCharEncoding :: BasicTy -> Char
basicGrinTyCharEncoding = head . show
%%]

%%[(8 codegen grin) export(basicTyGBTy)
basicTyGBTy :: BasicTy -> String
basicTyGBTy BasicTy_Word   = "GB_Word"
%%[[97
basicTyGBTy BasicTy_Float  = "GB_Float"
basicTyGBTy BasicTy_Double = "GB_Double"
%%]]
%%]

%%[(8 codegen) export(allGrinBasicTy)
allGrinBasicTy :: [BasicTy]
allGrinBasicTy
  = [ BasicTy_Word
%%[[97
    , BasicTy_Float
    , BasicTy_Double
%%]]
    ]
%%]

Encoding of arguments

%%[(8 codegen grin) export(basicGrinTyLEncoding)
basicGrinTyLEncoding :: [BasicTy] -> Integer
basicGrinTyLEncoding
  = foldr (.|.) 0 . zipWith (\sh t -> toInteger (fromEnum t + 1) `shiftL` sh) [0,shInc..]
%%[[8
  where shInc = 1
%%][97
  where shInc = 2
%%]]
%%]

%%[(8 codegen) hs
instance PP BasicTy where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicAnnot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(BasicAnnot(..),defaultGrinBasicAnnot)
data BasicAnnot
  = BasicAnnot_Size        Int BasicTy
  | BasicAnnot_FromTaggedPtr
  | BasicAnnot_ToTaggedPtr
  | BasicAnnot_Dflt
  deriving (Show,Eq)

defaultGrinBasicAnnot :: BasicAnnot
defaultGrinBasicAnnot = BasicAnnot_Size Cfg.sizeofWord BasicTy_Word
%%]

%%[(8 codegen grin) hs export(grinBasicAnnotSizeInBytes)
grinBasicAnnotSizeInBytes :: BasicAnnot -> Int
grinBasicAnnotSizeInBytes (BasicAnnot_Size          s _) = s
grinBasicAnnotSizeInBytes (BasicAnnot_FromTaggedPtr    ) = Cfg.sizeofWord
grinBasicAnnotSizeInBytes (BasicAnnot_ToTaggedPtr      ) = Cfg.sizeofWord
grinBasicAnnotSizeInBytes (BasicAnnot_Dflt             ) = Cfg.sizeofWord
%%]

%%[(8 codegen grin) hs export(grinBasicAnnotTy)
grinBasicAnnotTy :: BasicAnnot -> BasicTy
grinBasicAnnotTy (BasicAnnot_Size          _ t) = t
grinBasicAnnotTy (BasicAnnot_FromTaggedPtr    ) = BasicTy_Word
grinBasicAnnotTy (BasicAnnot_ToTaggedPtr      ) = BasicTy_Word
grinBasicAnnotTy (BasicAnnot_Dflt             ) = BasicTy_Word
%%]

%%[(8 codegen) hs
instance PP BasicAnnot where
  pp (BasicAnnot_Size          s t) = pp s >#< pp t
  pp (BasicAnnot_FromTaggedPtr    ) = pp "annotfromtaggedptr"
  pp (BasicAnnot_ToTaggedPtr      ) = pp "annottotaggedptr"
  pp (BasicAnnot_Dflt             ) = pp "annotdflt"
%%]

