


module EH101.EH.MainAG_TyExprAnn where

import Data.Char
import Data.List as List
import EH.Util.Pretty
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Opts
import EH101.Gam.Full
import EH101.Error
import EH101.Error.Pretty
import EH101.EH
import EH101.Ty.Pretty
import EH101.Ty.FitsInCommon
import EH101.Ty.FitsIn
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils
import EH101.VarMp
import EH101.Substitutable
import Data.Maybe
import EH101.Ty.Utils1
import EH101.Ty.Trf.Quantify
import EH101.Ty.Trf.Instantiate
import EH101.Ty
import EH101.Base.Debug as Debug
import Debug.Trace
import EH101.Ty.FitsInCommon2
import EH101.Ty.FIEnv2
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import EH101.Ty.Trf.FreshVar
import EH101.Ty.Ftv
import EH.Util.Utils (groupSortOn)
import Control.Applicative ((<|>))
import EH101.AbstractCore
import EH101.AbstractCore.Utils
import EH101.Core
import EH101.Core.FFI
import EH101.Core.Utils
import EH101.Core.Pretty
import EH101.Foreign.Extract
import EH101.LamInfo
import Control.Monad.State
import EH101.Ty.Utils2
import EH101.Base.Target
import EH101.Core.Subst
import EH101.Core.Coercion
import EH101.Ty.Trf.MergePreds
import EH101.Ty.Trf.Canonic
import EH101.Pred
import EH101.Pred.RedGraph (redPruneReductionsUntil)
import EH101.CHR
import EH101.CHR.Constraint
import EH101.Pred.CHR
import EH101.Pred.ToCHR
import EH101.Pred.Heuristics
import EH101.CHR.Solve
import EH101.Pred.EvidenceToCore
import EH101.Gam.ClassDefaultGam
import EH101.Ty.Trf.BetaReduce (tyBetaRedFull)
import EH101.Module
import EH101.Ty.UsedNames
import EH101.BuiltinPrims
import EH101.Foreign
import EH101.Foreign
import EH101.Foreign.Pretty
import EH101.Deriving
import EH101.Generics
import EH101.VarMp.Utils

import EH101.EH.MainAG_common

-- TyExprAnn ---------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         ann                  : TyAnn
         isEmpty              : Bool
         mbStrictness         : Maybe Strictness
         pp                   : PP_Doc
   alternatives:
      alternative Empty:
         visit 0:
            local pp          : _
      alternative Strictness:
         child strictness     : {Strictness}
         visit 0:
            local pp          : _
-}
sem_TyExprAnn_Empty :: T_TyExprAnn 

sem_TyExprAnn_Empty  =
    (case (TyAnn_Empty) of
     { _lhsOann | _lhsOann `seq` (True) ->
     (case (True) of
      { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
      (case (Nothing) of
       { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
       (case (empty) of
        { _pp | _pp `seq` (True) ->
        (case (_pp) of
         { _lhsOpp | _lhsOpp `seq` (True) ->
         ( _lhsOann,_lhsOisEmpty,_lhsOmbStrictness,_lhsOpp) }) }) }) }) })

sem_TyExprAnn_Strictness :: Strictness ->
                            T_TyExprAnn 

sem_TyExprAnn_Strictness strictness_  | strictness_ `seq` (True) =
    (case (TyAnn_Strictness strictness_) of
     { _lhsOann | _lhsOann `seq` (True) ->
     (case (False) of
      { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
      (case (Just strictness_) of
       { _lhsOmbStrictness | _lhsOmbStrictness `seq` (True) ->
       (case ("@" >|< show strictness_) of
        { _pp | _pp `seq` (True) ->
        (case (_pp) of
         { _lhsOpp | _lhsOpp `seq` (True) ->
         ( _lhsOann,_lhsOisEmpty,_lhsOmbStrictness,_lhsOpp) }) }) }) }) })

