module EH101.Gam.ClassDefaultGam
( ClassDefaultGamInfo (..)
, ClassDefaultGam
, clDfGamLookupDefault )
where
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Gam
import EH101.Ty
import EH101.VarMp
import EH101.Ty.FitsInCommon2
import EH101.Ty.FitsIn
import Data.Maybe
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize



{-# LINE 32 "src/ehc/Gam/ClassDefaultGam.chs" #-}
data ClassDefaultGamInfo
  = ClassDefaultGamInfo
      { cldiDefaultTypes	:: [Ty]
      }
      deriving (Data,Typeable)

{-# LINE 42 "src/ehc/Gam/ClassDefaultGam.chs" #-}
type ClassDefaultGam = Gam HsName ClassDefaultGamInfo

{-# LINE 50 "src/ehc/Gam/ClassDefaultGam.chs" #-}
-- | Lookup a matching default for a predicate
clDfGamLookupDefault
  :: ( VarLookup gm TyVarId VarMpInfo
     -- , VarLookup gm Ty VarMpInfo
     , VarLookupCmb VarMp gm
     )
     => FIIn' gm -> Pred -> ClassDefaultGam
     -> Maybe VarMp
clDfGamLookupDefault fi pr clDfGam
  = case pr of
      Pred_Class t | isJust mbConArgs
        -> do (ClassDefaultGamInfo {cldiDefaultTypes = (tg:_)}) <- gamLookup nm clDfGam
              (_,tyVarMp) <- fitPredIntoPred fi pr (Pred_Class $ mk1ConApp nm tg)
              return tyVarMp
        where mbConArgs@(~(Just (nm,args))) = tyMbAppConArgs t
      _ -> Nothing

{-# LINE 73 "src/ehc/Gam/ClassDefaultGam.chs" #-}
instance Serialize ClassDefaultGamInfo where
  sput (ClassDefaultGamInfo a) = sput a
  sget = liftM ClassDefaultGamInfo sget

