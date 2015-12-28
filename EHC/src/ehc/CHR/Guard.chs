%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional Pred admin for Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[(9 hmtyinfer) module {%{EH}CHR.Guard} import(UHC.Util.CHR,{%{EH}CHR.Key})
%%]

%%[(9999 hmtyinfer) import({%{EH}CHR.Constraint}) export(module {%{EH}CHR.Constraint})
%%]

%%[(9999 hmtyinfer) import(qualified Data.Map as Map,qualified Data.Set as Set,Data.Maybe)
%%]

%%[(9 hmtyinfer) import(UHC.Util.Pretty)
%%]

%%[(9 hmtyinfer) import({%{EH}Base.Common})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty})
%%]

%%[(9999 hmtyinfer) import({%{EH}Ty},{%{EH}VarMp},{%{EH}Substitutable},{%{EH}Ty.FitsInCommon2},{%{EH}Ty.FitsIn},{%{EH}Ty.TreeTrieKey})
%%]

%%[(1010 hmtyinfer) import({%{EH}Base.HsName.Builtin})
%%]

%%[(50 hmtyinfer) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Guard, CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(Guard(..))
data Guard
  = HasStrictCommonScope    PredScope PredScope PredScope                   -- have strict/proper common scope?
  | IsVisibleInScope        PredScope PredScope                             -- is visible in 2nd scope?
  | NotEqualScope           PredScope PredScope                             -- scopes are unequal
  | EqualScope              PredScope PredScope                             -- scopes are equal
  | IsStrictParentScope     PredScope PredScope PredScope                   -- parent scope of each other?
%%[[10
  | NonEmptyRowLacksLabel   Ty LabelOffset Ty Label                         -- non empty row does not have label?, yielding its position + rest
%%]]
%%[[41
  | IsCtxNilReduction       Ty Ty
  | EqsByCongruence         Ty Ty PredSeq
  | UnequalTy               Ty Ty
  | EqualModuloUnification  Ty Ty
%%]]
%%[[50
  deriving (Typeable, Generic)
%%]]
%%]

%%[(9 hmtyinfer)
ppGuard :: Guard -> PP_Doc
ppGuard (HasStrictCommonScope   sc1 sc2 sc3) = ppParensCommas' [sc1 >#< "<" >#< sc2,sc1 >#< "<=" >#< sc3]
ppGuard (IsStrictParentScope    sc1 sc2 sc3) = ppParens (sc1 >#< "==" >#< sc2 >#< "/\\" >#< sc2 >#< "/=" >#< sc3)
ppGuard (IsVisibleInScope       sc1 sc2    ) = sc1 >#< "`visibleIn`" >#< sc2
ppGuard (NotEqualScope          sc1 sc2    ) = sc1 >#< "/=" >#< sc2
ppGuard (EqualScope             sc1 sc2    ) = sc1 >#< "==" >#< sc2
%%[[10
ppGuard (NonEmptyRowLacksLabel  r o t l    ) = ppParens (t >#< "==" >#< ppParens (r >#< "| ...")) >#< "\\" >#< l >|< "@" >|< o
%%]]
%%[[41
ppGuard (IsCtxNilReduction t1 t2           ) = t1 >#< "~>" >#< t2
ppGuard (EqsByCongruence t1 t2 ps          ) = t1 >#< "~~" >#< t2 >#< "~>" >#< ps
ppGuard (UnequalTy t1 t2                   ) = t1 >#< "/=" >#< t2
ppGuard (EqualModuloUnification t1 t2      ) = t1 >#< "==" >#< t2
%%]]
%%]
ppGuard (IsStrictParentScope    sc1 sc2 sc3) = ppParens (ppParens (sc1 >#< "==" >#< sc2 >#< "\\/" >#< sc1 >#< "==" >#< sc3 ) >#< "/\\" >#< sc2 >#< "/=" >#< sc3)

%%[(9 hmtyinfer)
instance Show Guard where
  show _ = "CHR Guard"

instance PP Guard where
  pp = ppGuard
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer)
instance Serialize Guard
%%]

%%[(5050 hmtyinfer)
instance Serialize Guard where
  sput (HasStrictCommonScope     a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c
  sput (IsVisibleInScope         a b    ) = sputWord8 1  >> sput a >> sput b
  sput (NotEqualScope            a b    ) = sputWord8 2  >> sput a >> sput b
  sput (EqualScope               a b    ) = sputWord8 3  >> sput a >> sput b
  sput (IsStrictParentScope      a b c  ) = sputWord8 4  >> sput a >> sput b >> sput c
  sput (NonEmptyRowLacksLabel    a b c d) = sputWord8 5  >> sput a >> sput b >> sput c >> sput d
%%[[41
  sput (IsCtxNilReduction        a b    ) = sputWord8 6  >> sput a >> sput b
  sput (EqsByCongruence          a b c  ) = sputWord8 7  >> sput a >> sput b >> sput c
  sput (UnequalTy                a b    ) = sputWord8 8  >> sput a >> sput b
  sput (EqualModuloUnification   a b    ) = sputWord8 9  >> sput a >> sput b
%%]]
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 HasStrictCommonScope     sget sget sget
              1  -> liftM2 IsVisibleInScope         sget sget
              2  -> liftM2 NotEqualScope            sget sget
              3  -> liftM2 EqualScope               sget sget
              4  -> liftM3 IsStrictParentScope      sget sget sget
              5  -> liftM4 NonEmptyRowLacksLabel    sget sget sget sget
%%[[41
              6  -> liftM2 IsCtxNilReduction        sget sget
              7  -> liftM3 EqsByCongruence          sget sget sget
              8  -> liftM2 UnequalTy                sget sget
              9  -> liftM2 EqualModuloUnification   sget sget
%%]]

%%]
