%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The content of this module depends on Base.Builtin and Base.Opts,
hence must be in a separate module.

%%[8 module {%{EH}Base.Builtin2} import({%{EH}Base.HsName},{%{EH}Base.Builtin},{%{EH}Base.Opts})
%%]

%%[8 import({%{EH}Base.BasicAnnot},{%{EH}ConfigDefines})
%%]

%%[8 import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin names of known boxed types, for which it is known how to box/unbox
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in Builtin, but because of cyclic dependency with Opts it cannot.

%%[8 hs export(builtinMayLiveUnboxedTyMp,builtinMayLiveUnboxedTyNmL)
builtinMayLiveUnboxedTyMp :: Map.Map HsName BasicAnnot
builtinMayLiveUnboxedTyMp
  = Map.fromList
      [ ( hsnInt , BasicAnnot_Size sizeofWord BasicTy_Word)
      , ( hsnChar, BasicAnnot_Size sizeofWord BasicTy_Word)
      ]

builtinMayLiveUnboxedTyNmL :: [HsName]
builtinMayLiveUnboxedTyNmL
  = Map.keys builtinMayLiveUnboxedTyMp
%%]

%%[8 hs export(builtinKnownBoxedTyMp,builtinKnownBoxedTyNmL)
builtinKnownBoxedTyMp :: EHCOpts -> Map.Map HsName BasicAnnot
builtinKnownBoxedTyMp opts
  = builtinMayLiveUnboxedTyMp
    `Map.union`
    Map.fromList
       [ ( n ehbnPackedString, BasicAnnot_Size sizeofWord   BasicTy_Word  )
%%[[97
       , ( n ehbnFloat       , BasicAnnot_Size sizeofFloat  BasicTy_Float )
       , ( n ehbnDouble      , BasicAnnot_Size sizeofDouble BasicTy_Double)
%%]]
       ]
  where n f = f $ ehcOptBuiltinNames opts

builtinKnownBoxedTyNmL :: EHCOpts -> [HsName]
builtinKnownBoxedTyNmL opts
  = Map.keys $ builtinKnownBoxedTyMp opts
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Records currently are mapped onto fixed names when generating GRIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This will (have to) change when extensible records are properly supported in the backend.

%%[8 hs export(builtinRecNm)
builtinRecNm :: Int -> HsName
builtinRecNm = hsnProd
%%]

%%[8 hs export(builtinKnownRecTyNmL)
builtinKnownRecTyNmL :: [HsName]
builtinKnownRecTyNmL
  = map builtinRecNm (0:[2..10])
%%]


