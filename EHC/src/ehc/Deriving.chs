%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deriving info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95 module {%{EH}Deriving} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Gam},{%{EH}Ty},{%{EH}Core},{%{EH}Core.Utils})
%%]

%%[95 import(qualified Data.Map as Map,Data.List,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Table for each derivable field of each derivable class: type as well as codegen info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[95 export(DerivCls(..),DerivClsFld(..),DerivClsMp)
data DerivClsExtraCtxt
  = DerivClsExtraCtxt_Fixed                                                     -- fixed predicate, independent of type over which is derived
      { dccClsClNm                  :: !HsName                                  -- class name
      , dccClsTyNm                  :: !HsName                                  -- and its type arg
      }

data DerivCls
  = DerivCls
      { dclNm                       :: !HsName                                  -- name of class
      , dclFldL                     :: ![DerivClsFld]                           -- field info
      , dclExtraCtxt                :: ![DerivClsExtraCtxt]                     -- additional instances to prove (and use)
      }

data DerivClsFld
  = DerivClsFld
      { dcfNm                       :: !HsName                                  -- name of field
      , dcfTy                       :: !Ty                                      -- type of field
      , dcfMkCPat                   ::                                          -- make the pattern for each alternative
                                       Int                                      -- index of alternative (the actual Enum ordering)
                                       -> CTag                                  -- tag
                                       -> Int                                   -- arity
                                       -> Maybe [HsName]                        -- optional list of names for constituents
                                       -> CPat
      , dcfInitialArgL              :: ![HsName]                                -- initial arguments for function, passed to dcfFoldSubsCExpr
      , dcfInitialSubArgL           :: DataTagInfo -> [CExpr]                   -- initial arguments for constituents
      , dcfNrOmitTailArg            :: !Int                                     -- nr of args not passed, i.e. how much to omit for partial app
      , dcfFoldSubsCExpr            :: UID                                      -- construct out of constituents
                                       -> RCEEnv
                                       -> DataTagInfo                           -- this tag info
                                       -> (Int,Int)                             -- (index (a la toEnum), nr of alts)
                                       -> [HsName]                              -- names of initial args (usually same as dcfInitialArgL)
                                       -> [CExpr]                               -- constituents
                                       -> CExpr
      , dcfNoArgSubsCExpr           :: [(DataTagInfo,[CExpr])]                  -- all tag info + subs
                                       -> Int                                   -- nr of alts
                                       -> CExpr                                 -- instance under construction
                                       -> [CExpr]                               -- instances corresponding to dclExtraCtxt
                                       -> [HsName]                              -- names of initial args (usually same as dcfInitialArgL)
                                       -> CExpr                                 -- when class member takes no args
      , dcfAllTagLtCExpr            :: !CExpr                                   -- when tags are < previous
      , dcfAllTagGtCExpr            :: !CExpr                                   -- when tags are > previous
      , dcfWrapCase                 ::                                          -- wrap case expr in derived function
                                       EHCOpts
                                       -> DataGamInfo                           -- info of data ty
                                       -> Int                                   -- nr of alts
                                       -> HsName                                -- name of expr inspected by case expr
                                       -> CExpr                                 -- case expr
                                       -> CExpr
      }

type DerivClsMp = Map.Map HsName DerivCls
%%]

%%[95 export(emptyDerivCls)
emptyDerivCls :: DerivCls
emptyDerivCls = DerivCls hsnUnknown [] []
%%]

%%[95 export(dccMkTy)
dccMkTy :: DerivClsExtraCtxt -> Ty
dccMkTy (DerivClsExtraCtxt_Fixed clNm tyNm) = mkConApp clNm [semCon tyNm]
%%]

The table is dependent on builtin names, stored in EHCOpts.

%%[95 export(mkDerivClsMp)
mkDerivClsMp :: EHCOpts -> ValGam -> DataGam -> DerivClsMp
mkDerivClsMp opts valGam dataGam
  = Map.unions
    $ map (uncurry Map.singleton)
    $ [
      -- Eq
         mkc ehbnClassEq []
           [
           -- Eq((==))
             mkf ehbnClassEqFldEq
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ _ _ _ _ vs
                     -> case vs of
                          [] -> true
                          _  -> foldr1 and vs
                )
                Nothing -- no zero arg
                false false
                nowrap
           ]
           
      -- Ord
      ,  mkc ehbnClassOrd []
           [
           -- Ord(compare)
             mkf ehbnClassOrdFldCompare
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\uniq env _ _ _ vs
                     -> case vs of
                          [] -> eq
                          _  -> foldr1 (\l r -> mkCExprStrictSatCase env (Just nStrict) l
                                                  [ CAlt_Alt (mkCPatCon (orderingTag eqNm) 0 Nothing) r
                                                  , CAlt_Alt (mkCPatCon (orderingTag ltNm) 0 Nothing) lt
                                                  , CAlt_Alt (mkCPatCon (orderingTag gtNm) 0 Nothing) gt
                                                  ]
                                       ) vs
                             where n = mkHNm uniq
                                   nStrict = hsnSuffix n "!"
                )
                Nothing -- no zero arg
                gt lt
                nowrap
           ]
           
      -- Enum
      ,  mkc ehbnClassEnum [DerivClsExtraCtxt_Fixed (fn ehbnClassEnum) hsnInt]
           [
           -- Enum(enumFrom)
             mkf ehbnClassEnumFldEnumFrom
                Nothing -- only data constructor patterns
                (take 1 hsnLclSupply)
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \dtiSubs nrOfAlts self _ [argFrom]
                     -> let (dti,_) = last dtiSubs
                        in  cbuiltinApp opts ehbnClassEnumFldEnumFromTo [self,CExpr_Var argFrom,CExpr_Tup (dtiCTag dti)]
                )
                undef undef
                nowrap
           -- Enum(enumFromThen)
           , mkf ehbnClassEnumFldEnumFromThen
                Nothing -- only data constructor patterns
                (take 2 hsnLclSupply)
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \dtiSubs nrOfAlts self _ [argFrom,argThen]
                     -> let (dti1,_) = head dtiSubs
                            (dti2,_) = last dtiSubs
                        in  cbuiltinApp opts ehbnClassEnumFldEnumFromThenTo
                              [ self
                              , CExpr_Var argFrom
                              , CExpr_Var argThen
                              , mkCIf opts Nothing
                                  (cbuiltinApp opts ehbnPrimGtInt
                                     [ cbuiltinApp opts ehbnClassEnumFldFromEnum [self, CExpr_Var argFrom]
                                     , cbuiltinApp opts ehbnClassEnumFldFromEnum [self, CExpr_Var argThen]
                                     ]
                                  )
                                  (CExpr_Tup (dtiCTag dti1))
                                  (CExpr_Tup (dtiCTag dti2))
                              ]
                )
                undef undef
                nowrap
           -- Enum(enumFromTo)
{-
           , mkf ehbnClassEnumFldEnumFromTo
                Nothing -- only data constructor patterns
                (take 2 hsnLclSupply)
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \dtiSubs nrOfAlts self [dictEnumInt] [argFrom,argTo]
                     -> cbuiltinApp opts ehbnMap
                          [ cbuiltinApp opts ehbnClassEnumFldToEnum [self]
                          , cbuiltinApp opts ehbnClassEnumFldEnumFromTo
                              [ dictEnumInt
                              , cbuiltinApp opts ehbnClassEnumFldFromEnum [self,CExpr_Var argFrom]
                              , cbuiltinApp opts ehbnClassEnumFldFromEnum [self,CExpr_Var argTo]
                              ]
                          ]
                )
                undef undef
                nowrap
-}
           -- Enum(enumFromThenTo)
{-
           , mkf ehbnClassEnumFldEnumFromThenTo
                Nothing -- only data constructor patterns
                (take 3 hsnLclSupply)
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \dtiSubs nrOfAlts self [dictEnumInt] [argFrom,argThen,argTo]
                     -> cbuiltinApp opts ehbnMap
                          [ cbuiltinApp opts ehbnClassEnumFldToEnum [self]
                          , cbuiltinApp opts ehbnClassEnumFldEnumFromThenTo
                              [ dictEnumInt
                              , cbuiltinApp opts ehbnClassEnumFldFromEnum [self,CExpr_Var argFrom]
                              , cbuiltinApp opts ehbnClassEnumFldFromEnum [self,CExpr_Var argThen]
                              , cbuiltinApp opts ehbnClassEnumFldFromEnum [self,CExpr_Var argTo]
                              ]
                          ]
                )
                undef undef
                nowrap
-}
           -- Enum(fromEnum)
           , mkf ehbnClassEnumFldFromEnum
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ env _ (altInx,_) _ []
                     -> CExpr_Int altInx
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           -- Enum(toEnum)
           , mkf ehbnClassEnumFldToEnum
                (Just $ \altInx _ _ _ -> CPat_Int hsnWild altInx)
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ _ dti _ _ []
                     -> CExpr_Tup (dtiCTag dti)
                )
                Nothing -- no zero arg
                undef undef
                (\opts dgi nrOfAlts cNm body
                  -> let cNmv = CExpr_Var cNm
                         cNm1 = hsnSuffix cNm "!boundCheck"
                     in  mkCIf opts (Just cNm1)
                           (cbuiltinApp opts ehbnPrimGtInt [cNmv,CExpr_Int (nrOfAlts-1)])
                           (cerror opts $ "too high for toEnum to " ++ show (dgiTyNm dgi))
                           (mkCIf opts (Just cNm1)
                             (cbuiltinApp opts ehbnPrimGtInt [CExpr_Int 0,cNmv])
                             (cerror opts $ "too low for toEnum to " ++ show (dgiTyNm dgi))
                             body
                           )
                )
           -- Enum(succ)
           , mkf ehbnClassEnumFldSucc
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ env dti (altInx,nrOfAlts) _ []
                     -> if altInx < nrOfAlts  - 1
                        then CExpr_Int $ altInx + 1
                        else cerror opts $ "cannot succ last constructor: " ++ show (dtiConNm dti)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           -- Enum(pred)
           , mkf ehbnClassEnumFldPred
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ env dti (altInx,_) _ []
                     -> if altInx > 0
                        then CExpr_Int $ altInx - 1
                        else cerror opts $ "cannot pred first constructor: " ++ show (dtiConNm dti)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           ]
           
      -- Bounded
      ,  mkc ehbnClassBounded []
           [
           -- Bounded(maxBound)
             mkf ehbnClassBoundedFldMaxBound
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \dtiSubs _ _ _ _
                     -> let (dti,subs) = last dtiSubs
                        in  CExpr_Tup (dtiCTag dti) `mkCExprApp` subs
                )
                undef undef
                nowrap
           -- Bounded(minBound)
           , mkf ehbnClassBoundedFldMinBound
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \dtiSubs _ _ _ _
                     -> let (dti,subs) = head dtiSubs
                        in  CExpr_Tup (dtiCTag dti) `mkCExprApp` subs
                )
                undef undef
                nowrap
           ]
           
      -- Show
      ,  mkc ehbnClassShow []
           [
           -- Show(showsPrec)
             mkf ehbnClassShowFldShowsPrec
                Nothing -- only data constructor patterns
                [precDepthNm]
                (Just $ \dti -> [CExpr_Int $ maybe (fixityMaxPrio + 1) (+1) $ dtiMbFixityPrio $ dti])
                0
                (\_ _ dti _ [dNm] vs
                     -> let mk needParen v = mkCExprApp (CExpr_Var $ fn ehbnPrelShowParen) [needParen,v]
                        in  case (vs,dtiMbFixityPrio dti) of
                              ([],_)
                                -> showString $ tag2str (dtiCTag dti)
                              ([v1,v2],Just p)
                                -> mk (cgtint opts (CExpr_Var dNm) p)
                                   $ foldr1 compose ([v1] ++ [ showString $ " " ++ tag2str (dtiCTag dti) ++ " " ] ++ [v2])
                              _ -> mk (cgtint opts (CExpr_Var dNm) fixityMaxPrio)
                                   $ foldr1 compose ([showString $ tag2str (dtiCTag dti) ++ " "] ++ intersperse (showString " ") vs)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           ]
      ]
  where mkc c extraCtxt flds
          = (c', DerivCls c' flds extraCtxt)
          where c' = fn c
        mkf f mbMkPat as mbAsSubs omTl cAllMatch mbCNoArg cLT cGT wrap
          = DerivClsFld f' t mkPat as asSubs omTl cAllMatch cNoArg cLT cGT wrap
          where f' = fn f
                (t,_) = valGamLookupTy f' valGam
                mkPat = maybe (const mkCPatCon) id mbMkPat
                asSubs = maybe (const []) id mbAsSubs
                cNoArg = maybe (\_ _ _ _ _ -> undef) id mbCNoArg
        fn f  = f $ ehcOptBuiltinNames opts
        false = CExpr_Var $ fn ehbnBoolFalse
        true  = CExpr_Var $ fn ehbnBoolTrue
        and l r = (CExpr_Var $ fn ehbnBoolAnd) `mkCExprApp` [l,r]
        compose l r = (CExpr_Var $ fn ehbnPrelCompose) `mkCExprApp` [l,r]
        showString s = (CExpr_Var $ fn ehbnPrelShowString) `mkCExprApp` [cstring opts s]
        eqNm = fn ehbnDataOrderingAltEQ
        ltNm = fn ehbnDataOrderingAltLT
        gtNm = fn ehbnDataOrderingAltGT
        eq = CExpr_Var eqNm
        lt = CExpr_Var ltNm
        gt = CExpr_Var gtNm
        precDepthNm = mkHNm "d"
        undef = cundefined opts
        tag2str = show . hsnQualified . ctagNm
        orderingTag conNm
          = dtiCTag
            $ dgiDtiOfCon conNm
            $ panicJust "mkDerivClsMp.dataGamLookup"
            $ dataGamLookup (fn ehbnDataOrdering) dataGam
        nowrap _ _ _ _ x = x
        nononzeroargs _ _ _ _ _ _ = undef
%%]

