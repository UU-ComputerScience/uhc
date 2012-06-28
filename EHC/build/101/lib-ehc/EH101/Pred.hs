module EH101.Pred
( ClGamInfo (..), ClGam, emptyCLGI
, initClGam
, ClsFuncDep (..) )
where
import EH101.Base.Builtin
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH.Util.Pretty
import EH101.Gam.Full
import EH101.Base.Common
import EH101.Opts.Base
import EH101.Ty
import EH101.Ty.Pretty
import EH101.Ty.FitsInCommon
import EH101.Ty.Trf.Quantify
import EH101.VarMp
import EH101.Substitutable
import EH101.Core
import EH101.Core.Pretty
import EH101.Core.Subst
import EH101.Core.Utils
import EH101.Base.Debug
import EH101.Error
import EH101.Ty.Ftv
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize







{-# LINE 45 "src/ehc/Pred.chs" #-}
data ClsFuncDep = ClsFuncDep [Int] [Int] deriving Show

instance PP ClsFuncDep where
  pp (ClsFuncDep f t) = ppBracketsCommas f >|< "->" >|< ppBracketsCommas t

{-# LINE 56 "src/ehc/Pred.chs" #-}
data ClGamInfo
  =  ClGamInfo
       { clgiPrToEvidTy         :: !Ty                  -- mapping from predicate type -> dictionary structure record, encoded as function
       , clgiRuleTy             :: !Ty
       , clgiDfltDictNm         :: !HsName              -- dictionary name of default instance fields constructing function
       , clgiDictTag            :: !CTag                -- tag of dictionary
       , clgiGenerDerivableL    :: [(HsName,HsName)]    -- list of fields with default value which can be derived using generic deriving
       }
       deriving Show

type ClGam     = Gam HsName ClGamInfo

emptyCLGI
  = ClGamInfo
      Ty_Any Ty_Any hsnUnknown CTagRec
      []

{-# LINE 87 "src/ehc/Pred.chs" #-}
deriving instance Typeable ClGamInfo
deriving instance Data ClGamInfo

{-# LINE 92 "src/ehc/Pred.chs" #-}
instance PP ClGamInfo where
  pp clgi = pp (clgiDfltDictNm clgi) >#< "::" >#< ppTy (clgiRuleTy clgi) >#< "::" >#< ppTy (clgiPrToEvidTy clgi)

{-# LINE 97 "src/ehc/Pred.chs" #-}
initClGam
  = assocLToGam
      [ (hsnPrArrow,    emptyCLGI)
      ]

{-# LINE 112 "src/ehc/Pred.chs" #-}
instance Serialize ClGamInfo where
  sput (ClGamInfo a b c d e) = sput a >> sput b >> sput c >> sput d >> sput e
  sget = liftM5 ClGamInfo sget sget sget sget sget

