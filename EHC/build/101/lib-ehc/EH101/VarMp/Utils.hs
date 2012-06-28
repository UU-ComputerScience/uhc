module EH101.VarMp.Utils
( varmpGraphVisit )
where
import EH101.Base.Common
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty
import qualified Data.Set as Set
import qualified Data.Map as Map

{-# LINE 32 "src/ehc/VarMp/Utils.chs" #-}
-- | Filter the base level by traversing as a graph
varmpGraphVisit
  :: TyVarIdS
     -> VarMp
     -> VarMp
varmpGraphVisit start (VarMp l (m:ms))
  = VarMp l (m':ms)
  where m'
          = graphVisit (\newm m tvar
                         -> case Map.lookup tvar m of
                              Just i -> (Map.insert tvar i newm, varFreeSet i)
                              _      -> (newm,Set.empty)
                       )
                       Set.union
                       Map.empty
                       start m

