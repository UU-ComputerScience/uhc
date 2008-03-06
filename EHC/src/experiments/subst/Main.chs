%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Experiment: substitution variations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Variants:
1 - Basic, greedy subst applied immediately, leading to expansion of types
2 - Via memory simulation
3 - Via IORef sharing

%%[1 module Main
%%]

%%[1 import(System, Data.Char, Data.Maybe, Control.Monad, Control.Monad.State, System.Console.GetOpt, IO)
%%]

%%[1 import(Data.Map as Map, Data.Set as Set, Data.List as List)
%%]

%%[1 import(EH.Util.Pretty)
%%]

%%[3 import(Data.IORef, System.IO.Unsafe)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% main entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Example commandline invocation:
  ... b/15 p

This creates tree b with depth 15, and prints the computed value

%%[1.main
main :: IO ()
main
  =  do  args@((kind:'/':dep):(whichoutput:_):variant:_) <- getArgs
         let (t,c) = case kind of
                        'a' -> (mkTree1 (read dep), treeCompute)
                        'b' -> (mkTree2 (read dep), treeCompute)
                        _   -> error [kind]
%%[[1
         (r,_)   <-  runStateT (c t) emptySt
%%][2
         (q,st)  <-  runStateT (c t) emptySt
         let r =  stVM st |=> q
%%][3
         (r,_)   <-  runStateT (c t) emptySt
%%]]
         when  (whichoutput == 'p')
               (do  putPPLn (pp t)
                    putPPLn (pp r)
               )
         putStrLn (r `seq` "done")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Substitutable.infixr
infixr 6 |=>

%%]

%%[1.Substitutable
class Substitutable v where
  (|=>)         ::  VarMp -> v -> v
  ftv           ::  v -> Set VarId
%%]

%%[3 -(1.Substitutable 1.Substitutable.infixr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Variable identification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
type VarId = Int
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value which we want to compute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Val
data Val                        -- concrete syntax:
  =  Comb   !Val    !Val        -- (v,w)
  |  Const                      -- c
%%[[1
  |  Var    !VarId
%%][3
  |  Var    !VarId  !Ref
%%]]
  |  Err    !PP_Doc
  deriving Show
%%]
valIsErr :: Val -> Bool
valIsErr (Err _) = True
valIsErr _       = False

%%[3.Ref
type     RefContent  = Maybe Val
newtype  Ref         = Ref (IORef RefContent)
%%]

%%[3
instance Show Ref where
  show _ = "Ref"
%%]

%%[1
valErr :: PP x => x -> Val
valErr x = Err (pp x)
%%]

%%[1.Substitutable.Val
instance Substitutable Val where
%%[[Substitutable.Val.sbs
  s |=> v
%%[[1
    =  sbs s v
    where  sbs  s  (Comb v w)    =  Comb (sbs s v) (sbs s w)
           sbs  s  v@(Var i)     =  case i |? s of
                                      Just v'  -> v'
                                      _        -> v
           sbs  s  v             =  v
%%][2
    =  sbs Set.empty s v
    where  sbs  vis  s  (Comb v w)    =  Comb v' w'
             where  v' = sbs vis s v
                    w' = sbs vis s w
           sbs  vis  s  v@(Var i)     =
             case i |? s of
               Just v' | Set.notMember i vis
                  -> sbs (Set.insert i vis) s v'
               _  -> v
           sbs  vis  s  v             =  v
%%][21
    =  fst $ sbs Set.empty s v
    where  sbs  vis  s  (Comb v w)    =  (Comb v' w', s3)
             where  (v',s2) = sbs vis s   v
                    (w',s3) = sbs vis s2  w
           sbs  vis  s  v@(Var i)     = 
             case i |? s of
               Just v' | Set.notMember i vis
                  -> (v'',vmUnit i v'' `vmUnion` s')
                  where (v'',s') = sbs (Set.insert i vis) s v'
               _  -> (v,s)
           sbs  vis  s  v             =  (v,s)
%%]]
%%]]

%%[[1
  ftv (Var i)         =  Set.singleton i
%%]]
  ftv (Comb v w)      =  ftv v `Set.union` ftv w
  ftv _               =  Set.empty
%%]

%%[3 -1.Substitutable.Val
ftv :: Val -> Set VarId
ftv (Var i r)       =  case refRead r of
                         Just v  -> ftv v
                         _       -> Set.singleton i
ftv (Comb v w)      =  ftv v `Set.union` ftv w
ftv _               =  Set.empty
%%]

%%[1
instance PP Val where
%%[[1
  pp (Var i)     = "v" >|< pp i
%%][3
  pp (Var i r)   = case refRead r of
                     Just v  -> pp v
                     _       -> "v" >|< pp i
%%]]
  pp (Comb v w)  = ppParens (v >|< "," >|< w)
  pp (Const)     = pp "c"
  pp (Err s)     = "err:" >#< s
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.VarMp
newtype VarMp = VarMp (Map VarId Val)
%%]

%%[1.emptyVM
emptyVM          ::  VarMp
emptyVM          =   VarMp Map.empty
%%]

%%[1.varmpLookup
(|?)                ::  VarId -> VarMp -> Maybe Val
k |? (VarMp s)      =   Map.lookup k s
%%]

%%[2.varmpLookup -1.varmpLookup
(|?)                ::  VarId -> VarMp -> Maybe Val
k |? (VarMp s)
  = lk Set.empty k s
  where  lk visited k s
           =  case Map.lookup k s of
                r@(Just v)
                  |  Set.notMember k visited
                       ->  case v of
%%[[2
                             Var k'    -> lk (Set.insert k visited) k' s
%%]]
                             _         -> r
                _      ->  Nothing
%%]

%%[1.vmUnit
vmUnit :: VarId -> Val -> VarMp
vmUnit n v = VarMp (Map.singleton n v)

vmUnion :: VarMp -> VarMp -> VarMp
vmUnion (VarMp x) (VarMp y) = VarMp (x `Map.union` y)
%%]

%%[1.Substitutable.VarMp
instance Substitutable VarMp where
%%[[1
  s |=>  (VarMp m)  =  s `vmUnion` VarMp (Map.map (s |=>) m)
%%][2
  s |=>  s2         =  s `vmUnion` s2
%%]]
  ftv    (VarMp m)  =  Map.fold  (\v fv -> fv `Set.union` ftv v)
                                 Set.empty m
%%]

%%[3 -(1.VarMp 1.emptyVM 2.varmpLookup 1.vmUnit 1.Substitutable.VarMp)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Env
newtype Env = Env (Map String Val)
%%]

%%[1
emptyEnv :: Env
emptyEnv = Env Map.empty
%%]

%%[1.Substitutable.Env
instance Substitutable Env where
  s |=>  (Env m)  = Env (Map.map (s |=>) m)
  ftv    (Env m)  = Map.fold (\v fv -> fv `Set.union` ftv v) Set.empty m
%%]

%%[3 -1.Substitutable.Env
%%]

%%[1
envLookup :: String -> Env -> Maybe Val
envLookup n (Env m) = Map.lookup n m

envUnit :: String -> Val -> Env
envUnit n v = Env (Map.singleton n v)

envUnion :: Env -> Env -> Env
envUnion (Env x) (Env y) = Env (x `Map.union` y)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tree over which we compute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Tree
data Tree                                -- concrete syntax:
  =  Constant                            -- C
  |  UseBind     !String                 -- n
  |  DefBind     !String  !Tree  !Tree   -- bind n = x in y
  |  Pair        !Tree    !Tree          -- (x,y)
  |  First       !Tree                   -- fst x
  |  Second      !Tree                   -- snd x
%%[[4
  |  Apply       !Tree    !Tree          -- @@ x y
%%]]
  deriving Show
%%]

%%[1
instance PP Tree where
  pp (Constant        )  = pp "constant"
  pp (UseBind  n      )  = pp n
  pp (DefBind  n x y  )  = "bind" >#< n >#< "=" >#< x >#< "in" >-< y
  pp (Pair     x y    )  = "(" >|< x >-< "," >|< y >|< ")"
  pp (First    x      )  = "fst" >#< x
  pp (Second   x      )  = "snd" >#< x
%%[[4
  pp (Apply    x y    )  = "@" >#< (ppParens x >-< ppParens y)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tree generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
mkv    dep = "v" ++ show dep
mkuse  dep = UseBind (mkv dep)
mkuse1 dep = mkuse (dep-1)
mkuse2 dep = mkuse (dep-2)
mkuse3 dep = mkuse (dep-3)
%%]

Sequence of intro's, Comb's, deconstructed in subsequent uses

%%[1
mkTree1 :: Int -> Tree
mkTree1 maxDepth
  = mk minDepth
  where mk dep
          = DefBind bnam bval buse
          where bnam = mkv dep
                bval | dep == minDepth      =  Constant
                     | dep == (minDepth+1)  =  Pair
                                                 (mkuse1 dep)
                                                 (mkuse1 dep)
                     | otherwise            =  Pair
                                                 (Second (mkuse1 dep))
                                                 (First  (mkuse1 dep))
                buse | dep == maxDepth      =  UseBind bnam
                     | otherwise            =  mk (dep+1)
        minDepth = 1
%%]

Growing types, each further definition constructs a Val double the size of the previous.

%%[1.mkTree2
mkTree2 :: Int -> Tree
mkTree2 maxDepth
  = mk minDepth
  where mk dep
          = DefBind bnam bval buse
          where bnam = mkv dep
                bval | dep == minDepth      =  Constant
                     | dep == (minDepth+1)  =  Pair
                                                 (mkuse1 dep)
                                                 (mkuse1 dep)
                     | dep == (minDepth+2)  =  Pair
                                                 (First (mkuse1 dep))
                                                 (mkuse1 dep)
                     | otherwise            =  Pair
                                                 (Second (mkuse1 dep))
                                                 (Pair
                                                    (mkuse2 dep)
                                                    (mkuse2 dep))
                buse | dep == maxDepth      =  UseBind bnam
                     | otherwise            =  mk (dep+1)
        minDepth = 1
%%]

%%[4 -1.mkTree2
mkTree2 :: Int -> Tree
mkTree2 maxDepth
  = mk minDepth
  where mk dep
          = DefBind bnam bval buse
          where bnam = mkv dep
                bval | dep == minDepth      =  Constant
                     | dep == (minDepth+1)  =  Pair
                                                 (mkuse1 dep)
                                                 (mkuse1 dep)
                     | dep == (minDepth+2)  =  Pair
                                                 (First (mkuse1 dep))
                                                 (mkuse1 dep)
                     | otherwise            =  Pair
                                                 (Second (mkuse1 dep))
                                                 (Pair
                                                    (Apply
                                                       (mkuse1 dep)
                                                       (mkuse3 dep))
                                                    (mkuse2 dep))
                buse | dep == maxDepth      =  UseBind bnam
                     | otherwise            =  mk (dep+1)
        minDepth = 1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compute Val over Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.St
data St  =  St  {  stUniq     :: !VarId
                ,  stEnv      :: !Env
%%[[1
                ,  stVM       :: !VarMp
%%][3
%%]]
                }
%%]

%%[1
emptySt  ::  St
%%[[1
emptySt  =   St 0 emptyEnv emptyVM
%%][3
emptySt  =   St 0 emptyEnv
%%]]
%%]

%%[1.err
err :: PP x => x -> Compute Val
err x = return (valErr x)
%%]

%%[1.Compute
type Compute v = StateT St IO v
%%]

%%[1.treeCompute
treeCompute :: Tree -> Compute Val
treeCompute t =
    case t of
      Constant       ->  return Const
      UseBind n      ->
        do  st <- get
            case envLookup n (stEnv st) of
%%[[1
              Just v -> return (stVM st |=> v)
%%][2
              Just v -> return v
%%]]
              _      -> err $ "not found: " ++ show n
      DefBind n x y  ->
        do  x' <- treeCompute x
            st <- get
            let env = stEnv st
            put (st {stEnv = envUnit n x' `envUnion` env})
            y' <- treeCompute y
            st <- get
            put (st {stEnv = env})
            return y'
      Pair x y       ->
        do  x' <- treeCompute x
            y' <- treeCompute y
%%[[1
            st <- get
            return (Comb (stVM st |=> x') y')
%%][2
            return (Comb x' y')
%%]]
%%[[treeCompute.First
      First x        ->
        do  x'     <- treeCompute x
            [i,j]  <- newVars 2
            valMatch (Comb i j) x'
%%[[1
            st     <- get
            return (stVM st |=> i)
%%][2
            return i
%%]]
%%]]
      Second x       ->
        do  x'     <- treeCompute x
            [i,j]  <- newVars 2
            valMatch (Comb i j) x'
%%[[1
            st     <- get
            return (stVM st |=> j)
%%][2
            return j
%%]]
%%[[4
      Apply x y      ->
        do  x'     <- treeCompute x
            [i,j]  <- newVars 2
            valMatch (Comb i j) x'
            y'     <- treeCompute y
%%[[1
            st     <- get
            valMatch (stVM st |=> i) y'
%%][2
            valMatch i y'
%%]]
%%[[1
            st     <- get
            return (stVM st |=> j)
%%][2
            return j
%%]]
%%]]
%%]

%%[1.newVar
newVar  ::  Compute Val
newVar  =   do  st  <- get
                let i = stUniq st
                put (st {stUniq = i+1})
%%[[1
                return (Var i)
%%][3
                r   <- newRef
                return (Var i r)
%%]]
%%]

%%[1.newVars
newVars     ::  Int -> Compute [Val]
newVars  0  =   return []
newVars  n  =   do  v   <- newVar
                    vs  <- newVars (n-1)
                    return (v : vs)
%%]

%%[3.Ref.IO
newRef  ::  Compute Ref
newRef  =   do  r <- lift $ newIORef Nothing
                return (Ref r)

refRead          ::  Ref -> RefContent
refRead (Ref r)  =   unsafePerformIO $ readIORef r

refWrite            ::  Ref -> RefContent -> Compute ()
refWrite (Ref r) c  =   lift $ writeIORef r c
%%]
refRead          ::  Ref -> Compute RefContent
refRead (Ref r)  =   lift $ readIORef r


%%[1.valMatch
valMatch :: Val -> Val -> Compute Val
valMatch v1 v2
  = do { st <- get ; m st v1 v2 } where
  m  st  x@(Const)    (Const)                     =  return x
%%[[1
  m  st  x@(Var i)    (Var j)    |  i == j        =  return x
%%][3
  m  st  x@(Var i _)  (Var j _)  |  i == j        =  return x
%%]]
%%[[2
  m  st  (Var i)      y          |  isJust mbv    =  m st v y
     where  mbv  = i |? stVM st
            v    = fromJust mbv
  m  st  x            (Var j)    |  isJust mbv    =  m st x v
     where  mbv  = j |? stVM st
            v    = fromJust mbv
%%][3
  m  st  (Var _ r)    y          |  isJust mbv    =  m st v y
     where  mbv  = refRead r
            v    = fromJust mbv
  m  st  x            (Var _ r)  |  isJust mbv    =  m st x v
     where  mbv  = refRead r
            v    = fromJust mbv
%%]]
%%[[1
  m  st  (Var i)      y                           =  bind st i y
  m  st  x            (Var i)                     =  bind st i x
%%][3
  m  st  x@(Var _ _)  y                           =  bind st x y
  m  st  x            y@(Var _ _)                 =  bind st y x
%%]]
  m  st  (Comb p q)   (Comb r s)                  =
    do  pr   <- m st p r
        st2  <- get
%%[[1
        qs   <- m st2 (stVM st2 |=> q) (stVM st2 |=> s)
        st3  <- get
        return (Comb (stVM st3 |=> pr) qs)
%%][2
        qs   <- m st2 q s
        return (Comb pr qs)
%%]]
  m  _   _            _                           =  err "clash"
%%[[1
  bind st i v 
    |  Set.member i (ftv v)                       =  err "infin"
    |  otherwise                                  =
         do  put (st {stVM = vmUnit i v |=> stVM st})
             return v
%%][2
  bind st i v                                     = 
                    do  put (st {stVM = vmUnit i v |=> stVM st})
                        return v
%%][3
  bind st (Var i r) v  |  Set.member i (ftv v)    =  err "infin"
                       |  otherwise               =
                            do  refWrite r (Just v)
                                return v
%%]]
%%]

