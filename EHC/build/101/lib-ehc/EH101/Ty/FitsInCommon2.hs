module EH101.Ty.FitsInCommon2
( module EH101.Ty.FIEnv
, ppTyWithFI, ppTyWithFIFO
, FIIn' (..), FIIn
, emptyFI', emptyFI
, fiLookupVar', fiLookupTyVarCyc
, fiLookupReplaceTyCyc
, fiAllowTyVarBind
, fiInitInstRank, fiRankEqInstRank, fiUpdRankByPolarity
, fiPlusVarMp, fiSetVarMp, fiBindTyVar
, TyBetaRedEnv (..), emptyTyBetaRedEnv, emptyTyBetaRedEnv' )
where
import EH101.Base.Common
import EH101.Ty.FitsInCommon
import EH101.Ty
import EH101.Ty.Utils1
import EH101.Substitutable
import EH101.Opts
import EH101.VarMp
import EH101.Ty.FIEnv
import qualified Data.Set as Set
import EH.Util.Pretty

{-# LINE 33 "src/ehc/Ty/FitsInCommon2.chs" #-}
ppTyWithFI :: (VarLookupCmb VarMp gm, VarUpdatable Ty gm) => FIIn' gm -> Ty -> PP_Doc
ppTyWithFI fi t =  ppTyS (fiVarMpLoc fi |+> fiVarMp fi) t

ppTyWithFIFO :: (VarLookupCmb VarMp gm, VarUpdatable Ty gm) => FIIn' gm -> FIOut -> Ty -> PP_Doc
ppTyWithFIFO fi fo t    =  ppTyS (foVarMp fo |+> fiVarMp fi) t

{-# LINE 53 "src/ehc/Ty/FitsInCommon2.chs" #-}
data FIIn' globvm
  = FIIn
      { fiFIOpts          ::  !FIOpts               -- options to fitsIn
      , fiUniq            ::  !UID                  -- unique thread
      , fiVarMp           ::  !globvm               -- global (type) var bindings
      , fiVarMpLoc        ::  !VarMp                    -- locally introduced (type) var bindings
      , fiExpLTvS         ::  !(Set.Set TyVarId)        -- lhs ty vars for which expansion (via VarMp) is inhibited (already done once)
      , fiExpRTvS         ::  !(Set.Set TyVarId)        -- and rhs
      , fiRank            ::  !Int                  -- rank
      , fiMbInstRank      ::  !(Maybe Int)          -- rank where possible deep instantation did start
      , fiTrace           ::  [PP_Doc]              -- ???? 20080110, must be strict otherwise ghc 6.8.1 generates crashing program ????
      , fiEnv             ::  !FIEnv                -- environment (Gam's,...)
      }

type FIIn = FIIn' VarMp

{-# LINE 73 "src/ehc/Ty/FitsInCommon2.chs" #-}
emptyFI' :: gm -> FIIn' gm
emptyFI' m
  = FIIn
      { fiFIOpts          =   strongFIOpts
      , fiUniq            =   uidStart
      , fiVarMp           =   m
      , fiVarMpLoc        =   emptyVarMp
      , fiExpLTvS         =   Set.empty
      , fiExpRTvS         =   Set.empty
      , fiRank            =   1
      , fiMbInstRank      =   Nothing
      , fiTrace           =   []
      , fiEnv             =   emptyFE
      }

-- emptyFI :: forall gm . FIIn' gm
emptyFI = emptyFI' emptyVarMp

{-# LINE 95 "src/ehc/Ty/FitsInCommon2.chs" #-}
-- lookup a tvar subsequently in 2 VarMps
fiLookupVar' :: (v -> m1 -> Maybe x) -> (v -> m2 -> Maybe x) -> v -> m1 -> m2 -> Maybe x
fiLookupVar' lkup1 lkup2 v m1 m2
  = case lkup1 v m1 of
      Nothing -> lkup2 v m2
      j       -> j

-- lookup a tvar in the VarMps of a FIIn
fiLookupTyVarCyc :: VarLookup gm TyVarId VarMpInfo => FIIn' gm -> TyVarId -> Maybe Ty
fiLookupTyVarCyc  fi v  =  fiLookupVar' varmpTyLookupCyc varmpTyLookupCyc v (fiVarMpLoc fi) (fiVarMp fi)

{-# LINE 112 "src/ehc/Ty/FitsInCommon2.chs" #-}
-- lookup a possible tvar in the VarMps of a FIIn, the result being the replacement if any
fiLookupReplaceTyCyc :: VarLookup gm TyVarId VarMpInfo => FIIn' gm -> Ty -> Ty
fiLookupReplaceTyCyc  fi t  =  maybe t (maybe t id . fiLookupTyVarCyc fi) $ tyMbVar t

{-# LINE 126 "src/ehc/Ty/FitsInCommon2.chs" #-}
data TyBetaRedEnv gm
  = TyBetaRedEnv
      { tbredFI		:: FIIn' gm
      }

emptyTyBetaRedEnv' fe = TyBetaRedEnv ((emptyFI {fiEnv = fe}) :: FIIn)
emptyTyBetaRedEnv = TyBetaRedEnv emptyFI

{-# LINE 140 "src/ehc/Ty/FitsInCommon2.chs" #-}
-- Pre: is a tyvar
fiAllowTyVarBind :: FIIn' gm -> Ty -> Bool
fiAllowTyVarBind fi (Ty_Var v f)   =  f `elem` fioBindCategs (fiFIOpts fi) -- f == TyVarCateg_Plain
                                      && not (v `Set.member` fioDontBind (fiFIOpts fi))
fiAllowTyVarBind fi _              =  False

{-# LINE 154 "src/ehc/Ty/FitsInCommon2.chs" #-}
fiInitInstRank :: FIIn' gm -> FIIn' gm
fiInitInstRank fi = maybe (fi {fiMbInstRank = Just (fiRank fi)}) (const fi) (fiMbInstRank fi)

fiRankEqInstRank :: FIIn' gm -> Bool
fiRankEqInstRank fi = maybe True (== fiRank fi) (fiMbInstRank fi)

fiUpdRankByPolarity :: Polarity -> FIIn' gm -> FIIn' gm
fiUpdRankByPolarity pol fi = if polIsContravariant pol then fi {fiRank = fiRank fi + 1} else fi

{-# LINE 169 "src/ehc/Ty/FitsInCommon2.chs" #-}
fiPlusVarMp :: VarMp -> FIIn' gm -> FIIn' gm
fiPlusVarMp c fi = fi {fiVarMpLoc = c |+> fiVarMpLoc fi}

fiSetVarMp :: VarMp -> FIIn' gm -> FIIn' gm
fiSetVarMp  c fi = fi {fiVarMpLoc = c}

fiBindTyVar :: TyVarId -> Ty -> FIIn' gm -> FIIn' gm
fiBindTyVar v t = fiPlusVarMp (v `varmpTyUnit` t)

