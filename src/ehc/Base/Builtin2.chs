%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The content of this module depends on Base.Builtin and Base.Opts,
hence must be in a separate module.

%%[8 module {%{EH}Base.Builtin2} import({%{EH}Base.HsName},{%{EH}Base.Builtin},{%{EH}Base.Opts})
%%]

%%[8 import(qualified Data.Set as Set)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin names of known boxed types, for which it is known how to box/unbox
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in Builtin, but because of cyclic dependency with Opts it cannot.

%%[8 hs export(builtinMayLiveUnboxedTyNmL)
builtinMayLiveUnboxedTyNmL :: [HsName]
builtinMayLiveUnboxedTyNmL
  = [ hsnInt
    , hsnChar
    ]
%%]

%%[8 hs export(builtinKnownBoxedTyNmL)
builtinKnownBoxedTyNmL :: EHCOpts -> [HsName]
builtinKnownBoxedTyNmL opts
  = builtinMayLiveUnboxedTyNmL
    ++ [ ehbnPackedString $ ehcOptBuiltinNames opts
       ]
%%]

%%[8 hs export(builtinKnownRecTyNmL)
builtinKnownRecTyNmL :: [HsName]
builtinKnownRecTyNmL
  = map hsnProd (0:[2..10])
%%]


