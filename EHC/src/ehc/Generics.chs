%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for Generics, in particular for generic deriving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 hmtyinfer) module {%{EH}Generics}
%%]

%%[(92 hmtyinfer) import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Ty})
%%]

%%[(92 hmtyinfer) import({%{EH}AbstractCore},{%{EH}AbstractCore.Utils})
%%]

%%[(92 hmtyinfer) import({%{EH}Core},{%{EH}Core.Utils})
%%]

%%[(92 hmtyinfer) import({%{EH}Gam.DataGam})
%%]

%%[(92 hmtyinfer) import(EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intermediate representation of value pattern & expression structure used for embedded projection pair
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 hmtyinfer) export(Proj(..))
data Proj
  = 
  -- unit for product, sum resp
    Proj_U1
  | Proj_Void
  
  -- product field wrapper
  | Proj_Rec1
      { projTyL     :: !TyL     -- singleton
      , projN       :: !Int
      }
  | Proj_Par1
      { projTyL     :: !TyL     -- singleton
      , projN       :: !Int
      }
  | Proj_K1
      { projTyL     :: !TyL     -- singleton
      }
  | Proj_Comp1
      { projTyL     :: !TyL     -- singleton
      , projF1Ty    :: !Ty
      , projF2Proj  :: !Proj
      }

  -- sum alternative wrapper
  | Proj_L1						-- left sum injection, wrapping a constructor Proj
      { projProj    :: !Proj
      }
  | Proj_R1						-- right sum injection, wrapping a constructor Proj
      { projProj    :: !Proj
      }

  -- meta info wrapper, both for sum and outer layer of product
  | Proj_M1						-- meta info
      { projProj    :: !Proj
      }
  | Proj_M1_S1					-- meta info, variant of M1, to be used inside product; is it really necessary?
      { projProj    :: !Proj
      }

  -- special constructs
  | Proj_Con                    -- the datatype alternative
      { projCTag    :: !CTag
      , projProj    :: !Proj
      }
  | Proj_Prod					-- place holder for product
      { proj1Proj   :: !Proj
      , proj2Proj   :: !Proj
      }
  | Proj_Sum					-- place holder for sum, components are Proj_L1 and Proj_R1 resp.
      { proj1Proj   :: !Proj
      , proj2Proj   :: !Proj
      }
  | Proj                        -- top constructor, a Proj_Sum
      { projProjL   :: !Proj
      }
  deriving (Show)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract nested info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 hmtyinfer)
-- | get the constructor info
projCon :: Proj -> Proj
projCon (Proj_L1 p) = projCon p
projCon (Proj_R1 p) = projCon p
projCon (Proj_M1 p) = projCon p
projCon p           = p
%%]

%%[(92 hmtyinfer)
-- | get all sum alternatives
projSumAlts :: Proj -> [Proj]
projSumAlts (Proj_Sum l r) = projSumAlts l ++ projSumAlts r
projSumAlts (Proj_L1  p  ) = map Proj_L1 $ projSumAlts p
projSumAlts (Proj_R1  p  ) = map Proj_R1 $ projSumAlts p
projSumAlts (Proj_M1  p  ) = map Proj_M1 $ projSumAlts p
projSumAlts Proj_Void      = []
projSumAlts p              = [p]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin name corresponding to proj
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 hmtyinfer)
-- | builtin name of constructor
projBuiltinNm :: EHCOpts -> Proj -> HsName
projBuiltinNm opts proj
  = case proj of
      Proj_U1           -> v  ehbnGenerDataUnit1AltU1
      Proj_Void         -> v  ehbnUndefined
      Proj_Rec1  _ _    -> v  ehbnGenerDataRec1AltRec1
      Proj_Par1  _ _    -> v  ehbnGenerDataPar1AltPar1
      Proj_K1    _      -> v  ehbnGenerDataKonst1AltK1
      Proj_L1    _      -> v  ehbnGenerDataSumAltLeft
      Proj_R1    _      -> v  ehbnGenerDataSumAltRight
      Proj_M1    _      -> v  ehbnGenerDataMeta1AltM1
      Proj_M1_S1 _      -> v  ehbnGenerDataMeta1AltM1
      Proj_Comp1 _ _ _  -> v  ehbnGenerDataComp1AltComp1
      Proj_Prod  _ _    -> v  ehbnGenerDataProdAltProd
      _                 -> panic ("projBuiltinNm: " ++ show proj)
  where v  f   = ehcOptBuiltin  opts f

-- | builtin var of constructor
projBuiltinVar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Proj -> e
projBuiltinVar opts proj
  = acoreVar $ projBuiltinNm opts proj
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code construction of projections
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(92 hmtyinfer)
-- | pattern names arg to acoreSatSelsCases
nmLForCase nL = zipWith (\n o -> (n,{-n,-}o)) nL [(0::Int) ..]
%%]

%%[(92 hmtyinfer) export(projFrom)
-- | from function, starting with a top level proj
projFrom
  :: (AbstractCore e m b basp bcat mbind t p pr pf a, Eq bcat)
     => EHCOpts
     -> RCEEnv' e m b ba t
     -> Proj        	-- projection descriptor
     -> e       		-- resulting function
projFrom
     opts rceEnv
     (Proj sum)
  = acoreLam [argNm]
    $ acoreSatSelsCases
        rceEnv (Just $ hsnUniqifyEval argNm) (acoreVar argNm)
        [ (tg, nmLForCase nL, Nothing, fst $ mkExp proj nL)
        | proj <- projSumAlts sum
        , let con = projCon proj
              tg  = projCTag con
              nL  = tgNms tg
        ]
  where -- argument name
        argNm  = mkHNm "x"
        
        -- field names
        tgNms tg  = take (ctagArity tg) $ hsnLclSupply
                
        -- make the rhs expr
        mkExp proj nL@(~(n:nL'))
          = case proj of
              Proj_Prod l r     -> (acoreApp (projBuiltinVar opts proj) [l',r'], nrL)
                                where (l',nlL) = mkExp l nL
                                      (r',nrL) = mkExp r nlL
              Proj_K1    _      -> var
              Proj_Rec1  _ _    -> var
              Proj_Par1  _ _    -> var
              Proj_M1    _      -> wrap
              Proj_M1_S1 _      -> wrap
              Proj_L1    _      -> wrap
              Proj_R1    _      -> wrap
              Proj_Void         -> unit
              Proj_U1           -> unit
              Proj_Comp1 _ _ _  -> var
              Proj_Con   _ _    -> skip
              _                 -> panic ("projFrom.mkExp: " ++ show proj)
          where wrap = (acoreApp1 (projBuiltinVar opts proj) x, nL')
                     where (x,nL') = mkExp (projProj proj) nL
                unit = (projBuiltinVar opts proj, nL)
                skip = mkExp (projProj proj) nL
                var  = (acoreApp1 (projBuiltinVar opts proj) (acoreVar n), nL')
                       
        
%%]

%%[(92 hmtyinfer) export(projTo)
-- | from function, starting with a top level proj
projTo
  :: (AbstractCore e m b basp bcat mbind t p pr pf a, Eq bcat)
     => EHCOpts
     -> RCEEnv' e m b ba t
     -> Proj        	-- projection descriptor
     -> e		       	-- resulting function
projTo
     opts rceEnv
     (Proj sum)
  = acoreLam [scrut]
    $ mke (acoreVar scrut)	-- ref to scrut is just dummy
  where (mke,(scrut,_,_)) = mkExp sum [] (hsnLclSupplyWith $ mkHNm "proj")

        -- misc
        dataGam = rceDataGam rceEnv
                
        -- make the full case expr, given the names to be bound + supply of scratch names used for intermediate scrutinizing case expressions
        mkExp proj nL@(~(n:nL')) scrutL@(~(scrut:scrutL'))
          = case proj of
              -- product
              Proj_Prod l r     -> ( \e -> acoreSatSelsCases rceEnv (Just $ hsnUniqifyEval scrut) (acoreVar scrut)
                                             [ (prodTg ehbnGenerDataProdAltProd,nmLForCase [sl,sr],Nothing,l' $ r' e) ]
                                   , (scrut, nr, ssr)
                                   )
                                where (l',(sl,nl,ssl)) = mkExp l nL scrutL'
                                      (r',(sr,nr,ssr)) = mkExp r nl ssl
              Proj_K1    _      -> var  ehbnGenerDataKonst1 ehbnGenerDataKonst1AltK1
              Proj_Rec1  _ _    -> var  ehbnGenerDataRec1   ehbnGenerDataRec1AltRec1
              Proj_Par1  _ _    -> var  ehbnGenerDataPar1   ehbnGenerDataPar1AltPar1
              Proj_Comp1 _ _ _  -> var  ehbnGenerDataComp1  ehbnGenerDataComp1AltComp1
              Proj_U1           -> unit ehbnGenerDataUnit1  ehbnGenerDataUnit1AltU1
              Proj_M1_S1 _      -> wrap ehbnGenerDataMeta1  ehbnGenerDataMeta1AltM1

              -- sum
              Proj_Sum   l r    -> ( \e -> acoreSatSelsCases rceEnv (Just $ hsnUniqifyEval scrut) (acoreVar scrut)
                                             [ (sumTg ehbnGenerDataSumAltLeft ,nmLForCase [sl],Nothing,l' e)
                                             , (sumTg ehbnGenerDataSumAltRight,nmLForCase [sr],Nothing,r' e)
                                             ]
                                   , (scrut, nr, ssr)
                                   )
                                where (l',(sl,nl,ssl)) = mkExp l nL scrutL'
                                      (r',(sr,nr,ssr)) = mkExp r nl ssl
              Proj_M1    _      -> wrap ehbnGenerDataMeta1  ehbnGenerDataMeta1AltM1
              Proj_L1    _      -> skip
              Proj_R1    _      -> skip
              Proj_Void         -> (const $ projBuiltinVar opts proj, (scrut, nL, scrutL'))

              -- glue on boundary from sum to product
              Proj_Con   tg p   -> ( const $ p' $ acoreApp (acoreVar $ ctagNm tg) (map acoreVar nL)
                                   , info
                                   )
                                where (p',info) = mkExp p nL scrutL
                                      nL        = take (ctagArity tg) $ hsnLclSupply

              -- should not happen
              _                 -> panic ("projTo.mkExp: " ++ show proj)

          where wrap ty con      = mkC scrut [sp] np ssp p' ty con
                                 where (p',(sp,np,ssp)) = mkExp (projProj proj) nL scrutL'
                var              = mkC scrut [n] nL' scrutL' id
                unit             = mkC scrut []  nL  scrutL' id
                mkC s nL nL' sL' mke ty con
                                 = ( \e -> acoreSatSelsCases rceEnv (Just $ hsnUniqifyEval s) (acoreVar s)
                                             [ (tgOf ty con,nmLForCase nL,Nothing,mke e) ]
                                   , (s, nL', sL')
                                   )
                skip = mkExp (projProj proj) nL scrutL
                       
                -- CTag for con of dataty combi
                tgOf t c    = panicJust ("projTo.tgOf: " ++ show c') $ dataGamLookupTag (ehcOptBuiltin opts t) c' dataGam
                            where c' = ehcOptBuiltin opts c
                prodTg      = tgOf ehbnGenerDataProd
                sumTg       = tgOf ehbnGenerDataSum
        
%%]
