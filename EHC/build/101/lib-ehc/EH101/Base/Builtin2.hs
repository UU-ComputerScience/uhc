module EH101.Base.Builtin2
( BuiltinInfo (..)
, BuiltinInfoMp
, builtinMayLiveUnboxedTyMp
, builtinKnownBoxedTyMp
, builtinIsForGrin
, builtinKnownGrinBoxedTyNmL
, builtinGrinInfo
, builtinGrinMayLiveUnboxed
, builtinRecNm
, builtinKnownRecTyNmL )
where
import EH101.Base.HsName
import EH101.Base.Builtin
import EH101.Opts
import qualified EH101.Config as Cfg
import EH101.Base.BasicAnnot
import qualified Data.Map as Map




{-# LINE 37 "src/ehc/Base/Builtin2.chs" #-}
data BuiltinInfo
  = BuiltinInfo
      { biGrinBoxAnnot			:: BasicAnnot
      , biGbcMayLiveUnboxed		:: Bool
      , biIsSigned		        :: Bool
      }

emptyBuiltinInfo :: BuiltinInfo
emptyBuiltinInfo
  = BuiltinInfo
  	  { biGrinBoxAnnot			= basicAnnotWord
      , biGbcMayLiveUnboxed		= False
      , biIsSigned              = False
  	  }

{-# LINE 61 "src/ehc/Base/Builtin2.chs" #-}
type BuiltinInfoMp = Map.Map HsName BuiltinInfo

{-# LINE 65 "src/ehc/Base/Builtin2.chs" #-}
builtin32BitsTyMp :: EHCOpts -> Bool -> BuiltinInfoMp
builtin32BitsTyMp opts _
  = Map.fromList
       [ ( builtinNm opts ehbnInt32
         , emptyBuiltinInfo
             { biGbcMayLiveUnboxed	= livesUnboxed
             , biIsSigned           = True
             , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Int32 annHWord BasicAnnotTagging_None True -- annHWord
             }
         )
       , ( builtinNm opts ehbnWord32
         , emptyBuiltinInfo
             { biGbcMayLiveUnboxed	= livesUnboxed
             , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Word32 annHWord BasicAnnotTagging_None False -- annHWord
             }
         )
       , ( builtinNm opts ehbnFloat
         , emptyBuiltinInfo
             { biGbcMayLiveUnboxed	= livesUnboxed
             , biGrinBoxAnnot 		= BasicAnnot_Size basicSizeFloat BasicTy_Float BasicAnnotTagging_None False
             }
         )
       ]
  where livesUnboxed = Cfg.use64Bits
        annHWord | Cfg.isSameSizeForIntAndWord = BasicTy_SWord
                 | otherwise                   = BasicTy_SHWord

{-# LINE 105 "src/ehc/Base/Builtin2.chs" #-}
builtinMayLiveUnboxedTyMp :: EHCOpts -> BuiltinInfoMp
builtinMayLiveUnboxedTyMp opts
  = Map.fromList
         [ ( hsnInt
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biIsSigned             = True
               }
           )
         , ( hsnChar
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               }
           )
         , ( builtinNm opts ehbnWord
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               }
           )
         , ( builtinNm opts ehbnInt8
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Int8 annHWord BasicAnnotTagging_None True -- annHWord
               , biIsSigned             = True
               }
           )
         , ( builtinNm opts ehbnWord8
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Word8 annHWord BasicAnnotTagging_None False -- annHWord
               }
           )
         , ( builtinNm opts ehbnInt16
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Int16 annHWord BasicAnnotTagging_None True -- annHWord
               , biIsSigned             = True
               }
           )
         , ( builtinNm opts ehbnWord16
           , emptyBuiltinInfo
               { biGbcMayLiveUnboxed	= True
               , biGrinBoxAnnot 	    = BasicAnnot_Size BasicSize_Word16 annHWord BasicAnnotTagging_None False -- annHWord
               }
           )
         ]
{-# LINE 184 "src/ehc/Base/Builtin2.chs" #-}
    `Map.union`
       (if Cfg.use64Bits then builtin32BitsTyMp opts True else Map.empty)
{-# LINE 188 "src/ehc/Base/Builtin2.chs" #-}
  where annHWord | Cfg.isSameSizeForIntAndWord = BasicTy_SWord
                 | otherwise                   = BasicTy_SHWord

{-# LINE 194 "src/ehc/Base/Builtin2.chs" #-}
builtinMayLiveUnboxedTyNmL :: EHCOpts -> [HsName]
builtinMayLiveUnboxedTyNmL opts
  = Map.keys (builtinMayLiveUnboxedTyMp opts)

{-# LINE 200 "src/ehc/Base/Builtin2.chs" #-}
builtinKnownBoxedTyMp :: EHCOpts -> BuiltinInfoMp
builtinKnownBoxedTyMp opts
  = builtinMayLiveUnboxedTyMp opts
    `Map.union`
    Map.fromList
         [ ( builtinNm opts ehbnPackedString
           , emptyBuiltinInfo
           )
         , ( hsnInteger
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_None
               }
           )
         , ( builtinNm opts ehbnDouble
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size basicSizeDouble BasicTy_Double BasicAnnotTagging_None False
               }
           )
         , ( builtinNm opts ehbnInt64
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size BasicSize_Int64 BasicTy_Word BasicAnnotTagging_None True
               , biIsSigned         = True
               }
           )
         , ( builtinNm opts ehbnWord64
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size BasicSize_Word64 BasicTy_Word BasicAnnotTagging_None False
               }
           )
         , ( builtinNm opts ehbnAddr
           , emptyBuiltinInfo
               { biGrinBoxAnnot 	= BasicAnnot_Size (if Cfg.use32Bits then BasicSize_Word32 else BasicSize_Word64) BasicTy_Word BasicAnnotTagging_None False
               }
           )
         ]
    `Map.union`
       (if Cfg.use32Bits then builtin32BitsTyMp opts False else Map.empty)

{-# LINE 279 "src/ehc/Base/Builtin2.chs" #-}
builtinIsForGrin :: BuiltinInfo -> Bool
builtinIsForGrin bi
  = biGrinBoxAnnot bi /= BasicAnnot_None

{-# LINE 285 "src/ehc/Base/Builtin2.chs" #-}
builtinKnownGrinBoxedTyNmL :: EHCOpts -> [HsName]
builtinKnownGrinBoxedTyNmL opts
  = [ k | (k,bi) <- Map.toList $ builtinKnownBoxedTyMp opts, builtinIsForGrin bi ]

{-# LINE 291 "src/ehc/Base/Builtin2.chs" #-}
builtinGrinInfo :: EHCOpts -> HsName -> Maybe BuiltinInfo
builtinGrinInfo opts
  = \n -> case Map.lookup n m of
            mbi@(Just bi) | builtinIsForGrin bi -> mbi
            _                                   -> Nothing
  where m = builtinKnownBoxedTyMp opts

{-# LINE 300 "src/ehc/Base/Builtin2.chs" #-}
builtinGrinMayLiveUnboxed :: EHCOpts -> HsName -> Maybe BuiltinInfo
builtinGrinMayLiveUnboxed = builtinGrinInfo

{-# LINE 311 "src/ehc/Base/Builtin2.chs" #-}
builtinRecNm :: Int -> HsName
builtinRecNm = hsnProd

{-# LINE 316 "src/ehc/Base/Builtin2.chs" #-}
builtinKnownRecTyNmL :: [HsName]
builtinKnownRecTyNmL
  = map builtinRecNm (0:[2..10])

{-# LINE 326 "src/ehc/Base/Builtin2.chs" #-}
builtinNm :: EHCOpts -> (EHBuiltinNames -> HsName) -> HsName
builtinNm = ehcOptBuiltin

