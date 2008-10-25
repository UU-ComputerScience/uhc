%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Annotation for size info related to builtin types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs module {%{EH}Base.BasicAnnot}
%%]

%%[(8 codegen grin) hs import(Data.Bits)
%%]

%%[(8 codegen grin) hs import(EH.Util.Pretty)
%%]

%%[(8 codegen grin) hs import(qualified {%{EH}Config} as Cfg)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicAnnot types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs export(BasicTy(..))
data BasicTy
  = BasicTy_Word		-- base case: pointer, word, int, ...
%%[[97
  | BasicTy_Float		-- C: float
  | BasicTy_Double		-- C: double
%%]]
  deriving (Eq,Ord,Enum)
%%]

The Show of BasicTy should returns strings of which the first letter is unique for the type.
When used to pass to code, only this first letter is used.

%%[(8 codegen grin) hs
instance Show BasicTy where
  show BasicTy_Word   = "word"
%%[[97
  show BasicTy_Float  = "float"
  show BasicTy_Double = "double"
%%]]
%%]

%%[(8 codegen grin) export(basicTyChar)
basicTyChar :: BasicTy -> Char
basicTyChar = head . show
%%]

%%[(8 codegen grin) export(basicTyGBTy)
basicTyGBTy :: BasicTy -> String
basicTyGBTy BasicTy_Word   = "GB_Word"
%%[[97
basicTyGBTy BasicTy_Float  = "GB_Float"
basicTyGBTy BasicTy_Double = "GB_Double"
%%]]
%%]

%%[(8 codegen grin) export(allBasicTy)
allBasicTy :: [BasicTy]
allBasicTy
  = [ BasicTy_Word
%%[[97
    , BasicTy_Float
    , BasicTy_Double
%%]]
    ]
%%]

Encoding of arguments

%%[(8 codegen grin) export(basicTyLEncoding)
basicTyLEncoding :: [BasicTy] -> Integer
basicTyLEncoding
  = foldr (.|.) 0 . zipWith (\sh t -> toInteger (fromEnum t + 1) `shiftL` sh) [0,shInc..]
%%[[8
  where shInc = 1
%%][97
  where shInc = 2
%%]]
%%]

%%[(8 codegen grin) hs
instance PP BasicTy where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicAnnot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs export(BasicAnnot(..),defaultBasicAnnot)
data BasicAnnot
  = BasicAnnot_Size        Int BasicTy
  | BasicAnnot_FromTaggedPtr
  | BasicAnnot_ToTaggedPtr
  | BasicAnnot_Dflt
  deriving (Show,Eq)

defaultBasicAnnot :: BasicAnnot
defaultBasicAnnot = BasicAnnot_Size Cfg.sizeofWord BasicTy_Word
%%]

%%[(8 codegen grin) hs export(basicAnnotSizeInBytes)
basicAnnotSizeInBytes :: BasicAnnot -> Int
basicAnnotSizeInBytes (BasicAnnot_Size          s _) = s
basicAnnotSizeInBytes (BasicAnnot_FromTaggedPtr    ) = Cfg.sizeofWord
basicAnnotSizeInBytes (BasicAnnot_ToTaggedPtr      ) = Cfg.sizeofWord
basicAnnotSizeInBytes (BasicAnnot_Dflt             ) = Cfg.sizeofWord
%%]

%%[(8 codegen grin) hs export(basicAnnotTy)
basicAnnotTy :: BasicAnnot -> BasicTy
basicAnnotTy (BasicAnnot_Size          _ t) = t
basicAnnotTy (BasicAnnot_FromTaggedPtr    ) = BasicTy_Word
basicAnnotTy (BasicAnnot_ToTaggedPtr      ) = BasicTy_Word
basicAnnotTy (BasicAnnot_Dflt             ) = BasicTy_Word
%%]

%%[(8 codegen grin) hs
instance PP BasicAnnot where
  pp (BasicAnnot_Size          s t) = pp s >#< pp t
  pp (BasicAnnot_FromTaggedPtr    ) = pp "annotfromtaggedptr"
  pp (BasicAnnot_ToTaggedPtr      ) = pp "annottotaggedptr"
  pp (BasicAnnot_Dflt             ) = pp "annotdflt"
%%]

