%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The content of this module depends on Base.Builtin and Base.Opts,
hence must be in a separate module.

%%[8 module {%{EH}Base.Builtin2} import({%{EH}Base.HsName},{%{EH}Base.Builtin},{%{EH}Base.Opts})
%%]

%%[8 import({%{EH}ConfigDefines})
%%]

%%[(8 codegen) import({%{EH}Base.BasicAnnot})
%%]

%%[8 import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin names of known boxed types, for which it is known how to box/unbox
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in Builtin, but because of cyclic dependency with Opts it cannot.

%%[(8 codegen) hs export(BuiltinInfo(..))
data BuiltinInfo
  = BuiltinInfo
      { biGrinBoxAnnot			:: BasicAnnot
%%[[(8 jazy)
      , biJazyBasicTy			:: BasicTy			-- the basic ty used by jazy target, further (un)boxing done by code generation
      												-- TBD: factor it out to here
%%]]
      }

emptyBuiltinInfo :: BuiltinInfo
emptyBuiltinInfo
  = BuiltinInfo
  	  { biGrinBoxAnnot			= BasicAnnot_Size sizeofWord BasicTy_Word
%%[[(8 jazy)
      , biJazyBasicTy			= BasicTy_Object	-- if unspecified, it is boxed, leave as is
%%]]
  	  }
%%]

%%[(8 codegen) hs export(builtinMayLiveUnboxedTyMp,builtinMayLiveUnboxedTyNmL)
builtinMayLiveUnboxedTyMp :: Map.Map HsName BuiltinInfo
builtinMayLiveUnboxedTyMp
  = Map.fromList
      [ ( hsnInt
        , emptyBuiltinInfo
%%[[(8 jazy)
            { biJazyBasicTy    	= BasicTy_Int
            }
%%]]
        )
      , ( hsnChar
        , emptyBuiltinInfo
%%[[(8 jazy)
            { biJazyBasicTy    	= BasicTy_Char
            }
%%]]
        )
      ]

builtinMayLiveUnboxedTyNmL :: [HsName]
builtinMayLiveUnboxedTyNmL
  = Map.keys builtinMayLiveUnboxedTyMp
%%]

%%[(8 codegen) hs export(builtinKnownBoxedTyMp,builtinKnownBoxedTyNmL)
builtinKnownBoxedTyMp :: EHCOpts -> Map.Map HsName BuiltinInfo
builtinKnownBoxedTyMp opts
  = builtinMayLiveUnboxedTyMp
    `Map.union`
    Map.fromList
      [ ( n ehbnPackedString
        , emptyBuiltinInfo
%%[[(8 jazy)
            { biJazyBasicTy    	= BasicTy_String
            }
%%]]
        )
%%[[97
      , ( n ehbnFloat
        , emptyBuiltinInfo
            { biGrinBoxAnnot 	= BasicAnnot_Size sizeofFloat  BasicTy_Float
%%[[(97 jazy)
            , biJazyBasicTy    	= BasicTy_Float
%%]]
            }
        )
      , ( n ehbnDouble
        , emptyBuiltinInfo
            { biGrinBoxAnnot 	= BasicAnnot_Size sizeofDouble BasicTy_Double
%%[[(97 jazy)
            , biJazyBasicTy    	= BasicTy_Double
%%]]
            }
        )
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

%%[(8 codegen) hs export(builtinRecNm)
builtinRecNm :: Int -> HsName
builtinRecNm = hsnProd
%%]

%%[(8 codegen) hs export(builtinKnownRecTyNmL)
builtinKnownRecTyNmL :: [HsName]
builtinKnownRecTyNmL
  = map builtinRecNm (0:[2..10])
%%]


