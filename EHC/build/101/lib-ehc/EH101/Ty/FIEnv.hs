module EH101.Ty.FIEnv
( FIEnv (..)
, emptyFE )
where
import EH101.Base.Common
import EH101.Opts
import qualified Data.Set as Set
import EH101.Gam
import EH101.Gam.AppSpineGam
import EH.Util.Pretty
import EH101.VarMp
import EH101.AbstractCore
import EH101.Ty
import EH.Util.Utils
import EH101.Gam.TyGam
import EH101.Gam.PolGam
import EH101.Gam.DataGam








{-# LINE 60 "src/ehc/Ty/FIEnv.chs" #-}
data FIEnv
  =   FIEnv
        {   feAppSpineGam   :: !AppSpineGam			-- ty app spine info
        ,   feEHCOpts       :: !EHCOpts				-- compiler options
        ,   feDontBind      :: !TyVarIdS			-- inhibit type var binding for ...
        ,   fePredScope     :: !PredScope			-- the scope used by predicate resolution
        ,   feTyGam         :: !TyGam				-- type environment, for type synonym expansion
        ,   fePolGam        :: !PolGam				-- polarity environment, for co/contra variance
        ,   feDataGam       :: !DataGam				-- datatype info, for tycore generation, for coercions
        ,   feRange         :: !Range				-- position of source code from where subsumption is invoked
        }

{-# LINE 89 "src/ehc/Ty/FIEnv.chs" #-}
emptyFE
  =   FIEnv
        {   feAppSpineGam   =   emptyGam
        ,   feEHCOpts       =   defaultEHCOpts
        ,   feDontBind      =   Set.empty
        ,   fePredScope     =   initPredScope
        ,   feTyGam         =   emptyGam
        ,   fePolGam        =   emptyGam
        ,   feDataGam       =   emptyGam
        ,   feRange         =   emptyRange
        }

{-# LINE 118 "src/ehc/Ty/FIEnv.chs" #-}
instance Show FIEnv where
  show _ = "FIEnv"

{-# LINE 123 "src/ehc/Ty/FIEnv.chs" #-}
instance PP FIEnv where
  pp e = "FIEnv"
         >#< (empty
             >-< pp (feTyGam e)
             )

