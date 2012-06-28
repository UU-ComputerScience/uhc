module EH101.Gam.KiGam
( KiGam, KiGamInfo (..)
, initKiGam )
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




{-# LINE 37 "src/ehc/Gam/KiGam.chs" #-}
data KiGamInfo
  = KiGamInfo
      { kgiKi :: Ty }
      deriving Show

type KiGam = Gam HsName KiGamInfo

{-# LINE 52 "src/ehc/Gam/KiGam.chs" #-}
initKiGam :: KiGam
initKiGam
  = assocLToGam
      [ (hsnArrow   ,   KiGamInfo (Ty_Con hsnArrow))
      , (hsnKindStar,   KiGamInfo kiStar)
      , (hsnKindRow ,   KiGamInfo kiRow)
      ]

