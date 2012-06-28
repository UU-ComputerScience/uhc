module EH101.Generics
( Proj (..)
, projFrom
, projTo )
where
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts
import EH101.Ty
import EH101.AbstractCore
import EH101.AbstractCore.Utils
import EH101.Core
import EH101.Core.Utils
import EH101.Gam.DataGam
import EH.Util.Utils

{-# LINE 28 "src/ehc/Generics.chs" #-}
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

{-# LINE 92 "src/ehc/Generics.chs" #-}
-- | get the constructor info
projCon :: Proj -> Proj
projCon (Proj_L1 p) = projCon p
projCon (Proj_R1 p) = projCon p
projCon (Proj_M1 p) = projCon p
projCon p           = p

{-# LINE 101 "src/ehc/Generics.chs" #-}
-- | get all sum alternatives
projSumAlts :: Proj -> [Proj]
projSumAlts (Proj_Sum l r) = projSumAlts l ++ projSumAlts r
projSumAlts (Proj_L1  p  ) = map Proj_L1 $ projSumAlts p
projSumAlts (Proj_R1  p  ) = map Proj_R1 $ projSumAlts p
projSumAlts (Proj_M1  p  ) = map Proj_M1 $ projSumAlts p
projSumAlts Proj_Void      = []
projSumAlts p              = [p]

{-# LINE 116 "src/ehc/Generics.chs" #-}
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

{-# LINE 145 "src/ehc/Generics.chs" #-}
-- | pattern names arg to acoreSatSelsCases
nmLForCase nL = zipWith (\n o -> (n,{-n,-}o)) nL [(0::Int) ..]

{-# LINE 150 "src/ehc/Generics.chs" #-}
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
    $ acoreSatSelsCasesTy
        rceEnv (Just (hsnUniqifyEval argNm,acoreTyErr "Generics.projFrom.argNm")) (acoreVar argNm)
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



{-# LINE 203 "src/ehc/Generics.chs" #-}
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
              Proj_Prod l r     -> ( \e -> acoreSatSelsCasesTy rceEnv (Just (hsnUniqifyEval scrut,acoreTyErr "Generics.projTo.Prod.scrut")) (acoreVar scrut)
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
              Proj_Sum   l r    -> ( \e -> acoreSatSelsCasesTy rceEnv (Just (hsnUniqifyEval scrut,acoreTyErr "Generics.projTo.Sum.scrut")) (acoreVar scrut)
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
                                 = ( \e -> acoreSatSelsCasesTy rceEnv (Just (hsnUniqifyEval s,acoreTyErr "Generics.projTo.mkC.s")) (acoreVar s)
                                             [ (tgOf ty con,nmLForCase nL,Nothing,mke e) ]
                                   , (s, nL', sL')
                                   )
                skip = mkExp (projProj proj) nL scrutL

                -- CTag for con of dataty combi
                tgOf t c    = panicJust ("projTo.tgOf: " ++ show c') $ dataGamLookupTag (ehcOptBuiltin opts t) c' dataGam
                            where c' = ehcOptBuiltin opts c
                prodTg      = tgOf ehbnGenerDataProd
                sumTg       = tgOf ehbnGenerDataSum

