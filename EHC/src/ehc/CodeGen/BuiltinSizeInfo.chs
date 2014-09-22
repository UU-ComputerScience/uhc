%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The content of this module depends on {%{EH}Base.HsName.Builtin} and {%{EH}Opts},
hence must be in a separate module.

%%[(8 codegen) module {%{EH}CodeGen.BuiltinSizeInfo} import({%{EH}Base.HsName},{%{EH}Base.HsName.Builtin},{%{EH}Opts})
%%]

%%[(8 codegen) import(qualified {%{EH}Config} as Cfg)
%%]

%%[(8 codegen) import({%{EH}CodeGen.BasicAnnot})
%%]

%%[(8 codegen) import(qualified Data.Map as Map)
%%]

%%[doesWhat doclatex
BuiltinInfo encodes the mapping from HS types to their representation for a specific target.
For each target some info is maintained:
\begin{itemize}
\item Grin: biGrinBoxAnnot, lower level type.
\item Grin bytecode: biGbcMayLiveUnboxed, whether can live unboxed + tagged.
\item Jazy: biJazyBasicTy, lower level Java type.
\end{itemize}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin names of known boxed types, for which it is known how to box/unbox
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be in Builtin, but because of cyclic dependency with Opts it cannot.

%%[(8 codegen) hs export(BuiltinInfo(..))
data BuiltinInfo
  = BuiltinInfo
      { biIsSigned		        :: Bool
%%[[(8 grin)
      , biGrinBoxAnnot			:: BasicAnnot
      , biGbcMayLiveUnboxed		:: Bool
%%]]
%%[[(8 jazy)
      , biJazyBasicTy			:: BasicJazy		-- the basic ty used by jazy target, further (un)boxing done by code generation
      												-- TBD: factor it out to here
%%]]
      }

emptyBuiltinInfo :: BuiltinInfo
emptyBuiltinInfo
  = BuiltinInfo
      { biIsSigned              = False
%%[[(8 grin)
  	  , biGrinBoxAnnot			= basicAnnotWord
      , biGbcMayLiveUnboxed		= False
%%]]
%%[[(8 jazy)
      , biJazyBasicTy			= BasicJazy_Object	-- if unspecified, it is boxed, leave as is
%%]]
  	  }
%%]

%%[(8 codegen) hs export(BuiltinInfoMp)
type BuiltinInfoMp = Map.Map HsName BuiltinInfo
%%]

%%[(97 grin || jazy || javascript) hs
builtin32BitsTyMp :: EHCOpts -> Bool -> BuiltinInfoMp
builtin32BitsTyMp opts _
  = Map.fromList
       [ ( builtinNm opts ehbnInt32
         , emptyBuiltinInfo
             { biIsSigned           = True
%%[[(97 grin)
             , biGbcMayLiveUnboxed	= livesUnboxed
             , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Int32 annHWord BasicAnnotTagging_None True -- annHWord
%%]]
%%[[(97 jazy)
             , biJazyBasicTy      	= BasicJazy_Int
%%]]
             }
         )
       , ( builtinNm opts ehbnWord32
         , emptyBuiltinInfo
             { biIsSigned           = False
%%[[(97 grin)
             , biGbcMayLiveUnboxed	= livesUnboxed
             , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Word32 annHWord BasicAnnotTagging_None False -- annHWord
%%]]
%%[[(97 jazy)
             , biJazyBasicTy      	= BasicJazy_Int
%%]]
             }
         )
       , ( builtinNm opts ehbnFloat
         , emptyBuiltinInfo
             { biIsSigned           = False
%%[[(97 grin)
             , biGbcMayLiveUnboxed	= livesUnboxed
             , biGrinBoxAnnot 		= BasicAnnot_Size basicSizeFloat BasicTy_Float BasicAnnotTagging_None False
%%]]
%%[[(97 jazy)
             , biJazyBasicTy    	= BasicJazy_Float
%%]]
             }
         )
       ]
%%[[(97 machdep)
  where livesUnboxed = Cfg.use64Bits
        annHWord | Cfg.isSameSizeForIntAndWord = BasicTy_SWord
                 | otherwise                   = BasicTy_SHWord
%%]]
%%]

%%[(8 grin || jazy || javascript) hs export(builtinMayLiveUnboxedTyMp)
builtinMayLiveUnboxedTyMp :: EHCOpts -> BuiltinInfoMp
builtinMayLiveUnboxedTyMp opts
  = Map.fromList
         [ ( hsnInt
           , emptyBuiltinInfo
               { biIsSigned             = True
%%[[(8 grin)
               , biGbcMayLiveUnboxed	= True
%%]]
%%[[(8 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( hsnChar
           , emptyBuiltinInfo
               { biIsSigned             = False
%%[[(8 grin)
               , biGbcMayLiveUnboxed	= True
%%]]
%%[[(8 jazy)
               , biJazyBasicTy    		= BasicJazy_Char
%%]]
               }
           )
%%[[97
         , ( builtinNm opts ehbnWord
           , emptyBuiltinInfo
               { biIsSigned             = False
%%[[(97 grin)
               , biGbcMayLiveUnboxed	= True
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( builtinNm opts ehbnInt8
           , emptyBuiltinInfo
               { biIsSigned             = True
%%[[(97 grin)
               , biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Int8 annHWord BasicAnnotTagging_None True -- annHWord
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( builtinNm opts ehbnWord8
           , emptyBuiltinInfo
               { biIsSigned             = False
%%[[(97 grin)
               , biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Word8 annHWord BasicAnnotTagging_None False -- annHWord
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( builtinNm opts ehbnInt16
           , emptyBuiltinInfo
               { biIsSigned             = True
%%[[(97 grin)
               , biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Int16 annHWord BasicAnnotTagging_None True -- annHWord
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
         , ( builtinNm opts ehbnWord16
           , emptyBuiltinInfo
               { biIsSigned             = False
%%[[(97 grin)
               , biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Word16 annHWord BasicAnnotTagging_None False -- annHWord
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    		= BasicJazy_Int
%%]]
               }
           )
%%]]
         ]
%%[[(97 machdep)
    `Map.union`
       (if Cfg.use64Bits then builtin32BitsTyMp opts True else Map.empty)
%%]]
%%[[(97 machdep)
  where annHWord | Cfg.isSameSizeForIntAndWord = BasicTy_SWord
                 | otherwise                   = BasicTy_SHWord
%%]]
%%]

%%[(8 grin) hs
builtinMayLiveUnboxedTyNmL :: EHCOpts -> [HsName]
builtinMayLiveUnboxedTyNmL opts
  = Map.keys (builtinMayLiveUnboxedTyMp opts)
%%]

%%[(8 grin || jazy || javascript) hs export(builtinKnownBoxedTyMp)
builtinKnownBoxedTyMp :: EHCOpts -> BuiltinInfoMp
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
%%[[(98 jazy)
         , ( builtinNm opts ehbnHandle
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_None
               , biJazyBasicTy    	= BasicJazy_Handle
               }
           )
         , ( builtinNm opts ehbnByteArray
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_None
               , biJazyBasicTy    	= BasicJazy_ByteArray
               }
           )
%%]]
%%[[97
         , ( hsnInteger
           , emptyBuiltinInfo
               { biIsSigned         = False
%%[[(97 grin)
               , biGrinBoxAnnot 	= BasicAnnot_None
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Integer
%%]]
               }
           )
         , ( builtinNm opts ehbnDouble
           , emptyBuiltinInfo
               { biIsSigned         = False
%%[[(97 grin)
               , biGrinBoxAnnot 	= BasicAnnot_Size basicSizeDouble BasicTy_Double BasicAnnotTagging_None False
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Double
%%]]
               }
           )
         , ( builtinNm opts ehbnInt64
           , emptyBuiltinInfo
               { biIsSigned         = True
%%[[(97 grin)
               , biGrinBoxAnnot 	= BasicAnnot_Size BasicSize_Int64 BasicTy_Word BasicAnnotTagging_None True
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Long
%%]]
               }
           )
         , ( builtinNm opts ehbnWord64
           , emptyBuiltinInfo
               { biIsSigned         = False
%%[[(97 grin)
               , biGrinBoxAnnot 	= BasicAnnot_Size BasicSize_Word64 BasicTy_Word BasicAnnotTagging_None False
%%]]
%%[[(97 jazy)
               , biJazyBasicTy    	= BasicJazy_Long
%%]]
               }
           )
%%]]
%%[[99
         , ( builtinNm opts ehbnAddr
           , emptyBuiltinInfo
               { biIsSigned         = False
%%[[(99 grin)
               , biGrinBoxAnnot 	= BasicAnnot_Size (if Cfg.use32Bits then BasicSize_Word32 else BasicSize_Word64) BasicTy_Word BasicAnnotTagging_None False
%%]]
%%[[(99 jazy)
               , biJazyBasicTy    	= BasicJazy_Int
%%]]
               }
           )
%%]]
         ]
%%[[(97 machdep)
    `Map.union`
       (if Cfg.use32Bits then builtin32BitsTyMp opts False else Map.empty)
%%]]
%%]

%%[(8 grin) hs export(builtinIsForGrin)
builtinIsForGrin :: BuiltinInfo -> Bool
builtinIsForGrin bi
  = biGrinBoxAnnot bi /= BasicAnnot_None
%%]

%%[(8 grin) hs export(builtinKnownGrinBoxedTyNmL)
builtinKnownGrinBoxedTyNmL :: EHCOpts -> [HsName]
builtinKnownGrinBoxedTyNmL opts
  = [ k | (k,bi) <- Map.toList $ builtinKnownBoxedTyMp opts, builtinIsForGrin bi ]
%%]

%%[(8 grin) hs export(builtinGrinInfo)
builtinGrinInfo :: EHCOpts -> HsName -> Maybe BuiltinInfo
builtinGrinInfo opts
  = \n -> case Map.lookup n m of
            mbi@(Just bi) | builtinIsForGrin bi -> mbi
            _                                   -> Nothing
  where m = builtinKnownBoxedTyMp opts
%%]

%%[(8 grin) hs export(builtinGrinMayLiveUnboxed)
builtinGrinMayLiveUnboxed :: EHCOpts -> HsName -> Maybe BuiltinInfo
builtinGrinMayLiveUnboxed = builtinGrinInfo
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
builtinNm = ehcOptBuiltin
%%]

