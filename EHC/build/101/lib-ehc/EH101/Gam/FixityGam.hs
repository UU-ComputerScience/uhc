module EH101.Gam.FixityGam
( FixityGam, FixityGamInfo (..), defaultFixityGamInfo
, fixityGamLookup
, initFixityGam )
where
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Ty
import EH101.Ty.Pretty
import EH101.Gam
import EH101.Error
import qualified Data.Set as Set
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty.Trf.Quantify
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize





{-# LINE 40 "src/ehc/Gam/FixityGam.chs" #-}
data FixityGamInfo = FixityGamInfo { fgiPrio :: !Int, fgiFixity :: !Fixity } deriving Show

defaultFixityGamInfo = FixityGamInfo fixityMaxPrio Fixity_Infixl

type FixityGam = Gam HsName FixityGamInfo

{-# LINE 48 "src/ehc/Gam/FixityGam.chs" #-}
deriving instance Typeable FixityGamInfo
deriving instance Data FixityGamInfo


{-# LINE 54 "src/ehc/Gam/FixityGam.chs" #-}
fixityGamLookup :: HsName -> FixityGam -> FixityGamInfo
fixityGamLookup nm fg = maybe defaultFixityGamInfo id $ gamLookup nm fg

{-# LINE 63 "src/ehc/Gam/FixityGam.chs" #-}
initFixityGam :: FixityGam
initFixityGam
  = assocLToGam
      [ (hsnArrow  ,  FixityGamInfo 1 Fixity_Infixr)
      , (hsnPrArrow,  FixityGamInfo 2 Fixity_Infixr)
      , (hsnEqTilde,  FixityGamInfo 3 Fixity_Infix )
      ]

{-# LINE 89 "src/ehc/Gam/FixityGam.chs" #-}
instance Serialize FixityGamInfo where
  sput (FixityGamInfo a b) = sput a >> sput b
  sget = liftM2 FixityGamInfo sget sget
