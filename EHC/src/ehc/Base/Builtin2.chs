%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The content of this module depends on Base.Builtin and Base.Opts,
hence must be in a separate module.

%%[8 module {%{EH}Base.Builtin2} import({%{EH}Base.HsName},{%{EH}Base.Builtin},{%{EH}Base.Opts})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
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
      , biGbcMayLiveUnboxed		:: Bool
      , biIsSigned		        :: Bool
%%[[(8 jazy)
      , biJazyBasicTy			:: BasicJazy		-- the basic ty used by jazy target, further (un)boxing done by code generation
      												-- TBD: factor it out to here
%%]]
      }

emptyBuiltinInfo :: BuiltinInfo
emptyBuiltinInfo
  = BuiltinInfo
  	  { biGrinBoxAnnot			= BasicAnnot_Size basicSizeWord BasicTy_Word
      , biGbcMayLiveUnboxed		= False
      , biIsSigned              = False
%%[[(8 jazy)
      , biJazyBasicTy			= BasicJazy_Object	-- if unspecified, it is boxed, leave as is
%%]]
  	  }
%%]

%%[(97 codegen) hs
builtin32BitsTyMp :: EHCOpts -> Bool -> Map.Map HsName BuiltinInfo
builtin32BitsTyMp opts livesUnboxed
  = Map.fromList
       [ ( builtinNm opts ehbnInt32
         , emptyBuiltinInfo
             { biGbcMayLiveUnboxed	= Cfg.use64Bits
             , biIsSigned           = True
%%[[(97 jazy)
             , biJazyBasicTy      	= BasicJazy_Int
%%]]
             }
         )
       , ( builtinNm opts ehbnWord32
         , emptyBuiltinInfo
             { biGbcMayLiveUnboxed	= Cfg.use64Bits
%%[[(97 jazy)
             , biJazyBasicTy      	= BasicJazy_Int
%%]]
             }
         )
       ]
%%]

%%[(8 codegen) hs export(builtinMayLiveUnboxedTyMp)
builtinMayLiveUnboxedTyMp :: EHCOpts -> Map.Map HsName BuiltinInfo
builtinMayLiveUnboxedTyMp opts
  = Map.fromList
         [ ( hsnInt
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biIsSigned             = True
%%[[(8 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( hsnChar
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
%%[[(8 jazy)
               , biJazyBasicTy    		= BasicJazy_Char
%%]]
               }
           )
%%[[97
         , ( builtinNm opts ehbnWord
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( builtinNm opts ehbnInt8
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biIsSigned             = True
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Byte
%%]]
               }
           )
         , ( builtinNm opts ehbnWord8
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Byte
%%]]
               }
           )
         , ( builtinNm opts ehbnInt16
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biIsSigned             = True
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Short
%%]]
               }
           )
         , ( builtinNm opts ehbnWord16
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Short
%%]]
               }
           )
%%]]
         ]
%%]]
%%[[97
    `Map.union`
       (if Cfg.use64Bits then builtin32BitsTyMp opts True else Map.empty)
%%]]
%%]

%%[(8 codegen) hs
builtinMayLiveUnboxedTyNmL :: EHCOpts -> [HsName]
builtinMayLiveUnboxedTyNmL opts
  = Map.keys (builtinMayLiveUnboxedTyMp opts)
%%]

%%[(8 codegen) hs export(builtinKnownBoxedTyMp)
builtinKnownBoxedTyMp :: EHCOpts -> Map.Map HsName BuiltinInfo
builtinKnownBoxedTyMp opts
  = builtinMayLiveUnboxedTyMp opts
    `Map.union`
    Map.fromList
         [ ( builtinNm opts ehbnPackedString
           , emptyBuiltinInfo
%%[[(8 jazy)
               { biJazyBasicTy    	= BasicJazy_String
               }
%%]]
           )
%%[[97
         , ( builtinNm opts ehbnFloat
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size basicSizeFloat  BasicTy_Float
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Float
%%]]
               }
           )
         , ( builtinNm opts ehbnDouble
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size basicSizeDouble BasicTy_Double
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Double
%%]]
               }
           )
         , ( builtinNm opts ehbnInt64
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size BasicSize_Word64 BasicTy_Word
               , biIsSigned         = True
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Long
%%]]
               }
           )
         , ( builtinNm opts ehbnWord64
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size BasicSize_Word64 BasicTy_Word
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Long
%%]]
               }
           )
%%]]
         ]
%%[[97
    `Map.union`
       (if Cfg.use32Bits then builtin32BitsTyMp opts False else Map.empty)
%%]]
%%]

%%[(8 codegen) hs export(builtinKnownBoxedTyNmL)
builtinKnownBoxedTyNmL :: EHCOpts -> [HsName]
builtinKnownBoxedTyNmL opts
  = Map.keys $ builtinKnownBoxedTyMp opts
%%]

%%[(8 codegen) hs export(builtinMayLiveUnboxed)
builtinMayLiveUnboxed :: EHCOpts -> HsName -> Maybe BuiltinInfo
builtinMayLiveUnboxed opts
  = \n -> Map.lookup n m
  where m = builtinKnownBoxedTyMp opts
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get builtin name
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
builtinNm :: EHCOpts -> (EHBuiltinNames -> HsName) -> HsName
builtinNm opts f = f $ ehcOptBuiltinNames opts
%%]

