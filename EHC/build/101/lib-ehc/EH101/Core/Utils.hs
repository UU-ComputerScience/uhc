module EH101.Core.Utils
( module EH101.AbstractCore.Utils
, RCEEnv
, FieldUpdateL, fuL2ExprL, fuMap
, fvsClosure, fvsTransClosure
, fvLAsArg, mkFvNm, fvLArgRepl, fvVarRepl
, FldOffset (..), foffMkOff, foffLabel
, FieldSplitL, fsL2PatL
, fsLReorder
, fuMkCExpr
, cModMergeByPullingIn
, cModMerge2 )
where
import EH101.Base.Builtin
import EH101.Opts
import EH101.Base.Common
import EH101.Ty
import EH101.Core
import EH101.Gam.Full
import EH101.AbstractCore
import EH101.AbstractCore.Utils
import EH101.Core.Subst
import EH101.VarMp
import EH101.Substitutable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as Map
import EH.Util.Utils
import EH101.Base.Debug
import EH.Util.Pretty
import Control.Monad.State
import Data.Array
import qualified EH.Util.FastSeq as Seq
import EH101.Core.FvS
import EH101.Core.ModAsMap



{-# LINE 44 "src/ehc/Core/Utils.chs" #-}
type RCEEnv = RCEEnv' CExpr CMetaVal CBind CBound Ty

{-# LINE 60 "src/ehc/Core/Utils.chs" #-}
type FieldUpdateL e = AssocL HsName (e,Maybe Int)

fuMap :: (HsName -> e -> (e',Int)) -> FieldUpdateL e -> FieldUpdateL e'
fuMap f = map (\(l,(e,_)) -> let (e',o) = f l e in (l,(e',Just o)))

fuL2ExprL' :: (e -> CExpr) -> FieldUpdateL e -> [CExpr]
fuL2ExprL' f l = [ f e | (_,(e,_)) <- l ]

fuL2ExprL :: FieldUpdateL CExpr -> [CExpr]
fuL2ExprL = fuL2ExprL' cexprTupFld

fuReorder :: EHCOpts -> [HsName] -> FieldUpdateL CExpr -> (CBindL,FieldUpdateL (CExpr -> CExpr))
fuReorder opts nL fuL
  =  let  (fuL',offL,_,_)
            =  foldl
                 (\(fuL,offL,exts,dels) (n,(_,(f,_)))
                     ->  let  mkOff n lbl o
                                =  let smaller l = rowLabCmp l lbl == LT
                                       off = length (filter smaller dels) - length (filter smaller exts)
                                   in  acoreBind1Cat CBindCateg_Plain n (acoreBuiltinAddInt opts o off)
                              no = acoreVar n
                         in   case f of
                                 CExpr_TupIns _ t l o e -> ((l,(\r -> CExpr_TupIns r t l no e,Nothing)) : fuL,(mkOff n l o):offL,l:exts,dels  )
                                 CExpr_TupUpd _ t l o e -> ((l,(\r -> CExpr_TupUpd r t l no e,Nothing)) : fuL,(mkOff n l o):offL,exts  ,dels  )
                                 CExpr_TupDel _ t l o   -> ((l,(\r -> CExpr_TupDel r t l no  ,Nothing)) : fuL,(mkOff n l o):offL,exts  ,l:dels)
                 )
                 ([],[],[],[])
            .  zip nL
            $  fuL
          cmpFU (n1,_ ) (n2,_) = rowLabCmp n1 n2
     in   (offL, sortBy cmpFU fuL')

{-# LINE 94 "src/ehc/Core/Utils.chs" #-}
fuMkCExpr :: EHCOpts -> UID -> FieldUpdateL CExpr -> CExpr -> CExpr
fuMkCExpr opts u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder opts nL fuL
          bL = acoreBind1Cat CBindCateg_Plain n r : oL
     in   acoreLet CBindCateg_Strict bL $ foldl (\r (_,(f,_)) -> f r) (acoreVar n) $ fuL'

{-# LINE 107 "src/ehc/Core/Utils.chs" #-}
fvsClosure :: FvS -> FvS -> FvS -> FvSMp -> FvSMp -> (FvSMp,FvSMp)
fvsClosure newS lamOuterS varOuterS fvmOuter fvmNew
  =  let  fvmNew2  =  Map.filterWithKey (\n _ -> n `Set.member` newS) fvmNew
          fvlam  s =  lamOuterS `Set.intersection` s
          fvvar  s =  varOuterS `Set.intersection` s
          fv     s =  fvvar s `Set.union` s'
                   where s' = Set.unions $ map (\n -> Map.findWithDefault Set.empty n fvmOuter) $ Set.toList $ fvlam $ s
     in   (Map.map fv fvmNew2,Map.map (`Set.intersection` newS) fvmNew2)

fvsTransClosure :: FvSMp -> FvSMp -> FvSMp
fvsTransClosure lamFvSMp varFvSMp
  =  let  varFvSMp2 = Map.mapWithKey
                       (\n s -> s `Set.union` (Set.unions
                                               $ map (\n -> panicJust "fvsTransClosure.1" $ Map.lookup n $ varFvSMp)
                                               $ Set.toList
                                               $ panicJust "fvsTransClosure.2"
                                               $ Map.lookup n lamFvSMp
                       )                      )
                       varFvSMp
          sz = sum . map Set.size . Map.elems
     in   if sz varFvSMp2 > sz varFvSMp
          then fvsTransClosure lamFvSMp varFvSMp2
          else varFvSMp

{-# LINE 133 "src/ehc/Core/Utils.chs" #-}
fvLAsArg :: CVarIntroMp -> FvS -> CVarIntroL
fvLAsArg cvarIntroMp fvS
  =  sortOn (cviLev . snd)
     $ filter (\(_,cvi) -> cviLev cvi > cLevModule)
     $ map (\n -> (n,cviLookup n cvarIntroMp))
     $ Set.toList fvS

mkFvNm :: Int -> HsName -> HsName
mkFvNm i n = hsnUniqifyInt HsNameUniqifier_New i n -- hsnSuffix n ("~" ++ show i)

fvLArgRepl :: Int -> CVarIntroL -> (CVarIntroL,CVarIntroL,CVarReplNmMp)
fvLArgRepl uniq argLevL
  =  let  argNL = zipWith (\u (n,i) -> (mkFvNm u n,i)) [uniq..] argLevL
     in   ( argLevL
          , argNL
          , Map.fromList $ zipWith (\(o,_) (n,cvi) -> (o,(cvrFromCvi cvi) {cvrRepl = n})) argLevL argNL
          )

fvVarRepl :: CVarReplNmMp -> HsName -> CExpr
fvVarRepl nMp n = maybe (acoreVar n) (acoreVar . cvrRepl) $ Map.lookup n nMp

{-# LINE 160 "src/ehc/Core/Utils.chs" #-}
data FldOffset
  = FldKnownOffset      { foffLabel'     :: !HsName, foffOffset   :: !Int      }
  | FldComputeOffset    { foffLabel'     :: !HsName, foffCExpr    :: !CExpr    }
  | FldImplicitOffset

instance Eq FldOffset where
  (FldKnownOffset _ o1) == (FldKnownOffset _ o2) = o1 == o2
  foff1                 == foff2                 = foffLabel foff1 == foffLabel foff2

instance Ord FldOffset where
  (FldKnownOffset _ o1) `compare` (FldKnownOffset _ o2) = o1 `compare` o2
  foff1                 `compare` foff2                 = foffLabel foff1 `rowLabCmp` foffLabel foff2

foffMkOff :: FldOffset -> Int -> (Int,CExpr)
foffMkOff FldImplicitOffset      o = (o,acoreInt o)
foffMkOff (FldKnownOffset   _ o) _ = (o,acoreInt o)
foffMkOff (FldComputeOffset _ e) o = (o,e)

foffLabel :: FldOffset -> HsName
foffLabel FldImplicitOffset = hsnUnknown
foffLabel foff              = foffLabel' foff

{-# LINE 184 "src/ehc/Core/Utils.chs" #-}
type FieldSplitL = AssocL FldOffset RPat

fsL2PatL :: FieldSplitL -> [RPat]
fsL2PatL = assocLElts

{-# LINE 197 "src/ehc/Core/Utils.chs" #-}
fsLReorder :: EHCOpts -> FieldSplitL -> FieldSplitL
fsLReorder opts fsL
  =  let  (fsL',_)
            =  foldr
                 (\(FldComputeOffset l o,p) (fsL,exts)
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  acoreBuiltinAddInt opts o nrSmaller
                         in   ((FldComputeOffset l (mkOff l exts o),p):fsL,l:exts)
                 )
                 ([],[])
            $  fsL
     in   tyRowCanonOrderBy compare fsL'

{-# LINE 219 "src/ehc/Core/Utils.chs" #-}
data PullState cat bind
  = PullState
      { pullstBinds     :: Seq.Seq (CDbBindLetInfo'2 cat bind)  -- pulled in bindings
      , pullstPulledNmS :: !HsNameS                             -- pulled in names
      , pullstToDo      :: ![HsName]                            -- todo
      }

emptyPullState :: PullState cat bind
emptyPullState = PullState Seq.empty Set.empty []

-- | merge by pulling in that which is required only
cModMergeByPullingIn
  ::                                -- function giving bindings for name
     (HsName                        -- name
      -> Maybe
           ( cat                    -- category
           , [bind]                 -- and bindings
           , HsNameS                -- pulled in names (might be > 1 for mutual recursiveness)
     )     )
     -> (expr -> HsNameS)           -- extract free vars
     -> (bind -> [expr])            -- extract relevant exprs for binding
     -> ([(cat,[bind])] -> mod -> mod)
                                    -- update module with pulled bindings
     -> expr                        -- start of pulling in, usually top level name "main"
     -> ( (mod -> mod)              -- conversion of resulting module
        , HsNameS                   -- modules from which something was taken
        )
cModMergeByPullingIn
     pullIn getExprFvS getBindExprs updMod
     rootExpr
  = ( updMod (Seq.toList $ pullstBinds st)
    , Set.map (panicJust "cModMergeByPullingIn" . hsnQualifier) $ pullstPulledNmS st
    )
  where st = execState (pull) (emptyPullState {pullstToDo = Set.toList $ getExprFvS rootExpr})
        pull = do
          s <- get
          case pullstToDo s of
            (nm:nmRest)
              | nm `Set.notMember` pullstPulledNmS s && isJust mbPull
                -> do let pulledNms = pullstPulledNmS s `Set.union` pulled
                          newNms
                            = -- (\x -> tr "cModMergeByPullingIn.pulledNms" (nm >#< show x) x) $
                              (Set.unions $ map (Set.unions . map getExprFvS . getBindExprs) binds)
                              `Set.difference` pulledNms
                      put $
                        s { pullstToDo = Set.toList newNms ++ nmRest
                          , pullstBinds = Seq.singleton (cat,binds) `Seq.union` pullstBinds s
                          , pullstPulledNmS = pulledNms
                          }
                      pull
              | otherwise
                -> do put $
                        s { pullstToDo = nmRest
                          }
                      pull
              where mbPull@(~(Just (cat,binds,pulled))) = pullIn nm
            _ -> return ()

{-# LINE 279 "src/ehc/Core/Utils.chs" #-}
-- | merge by pulling
cModMerge2 :: ([CModule],CModule) -> CModule
cModMerge2 (mimpL,mmain)
  = mkM mmain
  where (mkM,_)   = cModMergeByPullingIn lkupPull cexprFvS cbindExprs
                                         (\bs (CModule_Mod modNm _ _) -> CModule_Mod modNm (acoreLetN bs $ rootExpr) allTags)
                                         rootExpr
        rootExpr  = cmoddbMainExpr modDbMain
        allTags   = concatMap cmoddbTagsMp $ modDbMain : modDbImp
        modDbMain =     cexprModAsDatabase mmain
        modDbImp  = map cexprModAsDatabase mimpL
        modDbMp = Map.unions [ Map.singleton (cmoddbModNm db) db | db <- modDbMain : modDbImp ]
        lkupMod  n = -- (\x -> tr "cModMerge2.lkupMod" (n >#< fmap cmoddbModNm x) x) $
                     maybe (Just modDbMain) (\m -> Map.lookup m modDbMp) $ hsnQualifier n
        lkupPull n = do
           db <- lkupMod n
           (bi,_) <- cmoddbLookup n db
           let (cat,bsarr) = cmoddbBindArr db ! bi
               bs = elems bsarr
           return ( cat, bs
                  , -- (\x -> tr "cModMerge2.lkupPull" (n >#< show x) x) $
                    Set.fromList $ map cbindNm bs
                  )

