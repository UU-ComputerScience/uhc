module EH101.EHC.CompileGroup
( EHCompileGroup (..), emptyECG )
where
import EH101.EHC.Common

{-# LINE 21 "src/ehc/EHC/CompileGroup.chs" #-}
data EHCompileGroup
  = EHCompileGroup
      { ecgNm                :: HsName
      , ecgModL              :: [HsName]
      }

emptyECG :: EHCompileGroup
emptyECG
  = EHCompileGroup
      { ecgNm                = hsnUnknown
      , ecgModL              = []
      }


