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

%%[1.topleveltest
main :: IO ()
main
  =  do  ((kind:'/':dep):(output:_):variant:_)
           <- getArgs
         let  t =  case kind of
                     'a'  -> mkLinearTree (read dep)
                     'b'  -> mkExponentialTree (read dep)
                     _    -> error [kind]
%%[[1
              (r,_) = runState (treeCompute t) emptySt
%%][2
              (q,st) = runState (treeCompute t) emptySt
              r =  stVMp st |@ q
%%][22
              (q,st) = runState (treeCompute t) emptySt
              r =  evl (stVMp st) q `seq` q
%%][3
         (r,_)   <-  runStateT (treeCompute t) emptySt
%%]]
         when  (output == 'p')
               (do  putPPLn (pp t)
                    putPPLn (pp r)
               )
         putStrLn (r `seq` "done")
%%]

%%[22
evl :: VMp -> Val -> ()
evl m x@(Var i)
  | isJust mbv && (evl m (fromJust mbv) `seq` True) = ()
  | otherwise = ()
  where mbv = i |? m
evl m (Pair x y)
  | evl m x `seq` evl m y `seq` True = ()
evl _ x = ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Substitutable.infixr
infixr 6 |@
%%]

%%[1.Substitutable
class Substitutable x where
  (|@)          ::  VMp -> x -> x
%%[[1
  ftv           ::  x -> Set VarId
%%][221
%%]]
%%]

%%[221
ftv :: VMp -> Val -> Set VarId
ftv m (Var i)         =  case i |? m of
                           Just v  -> ftv (vmDelete i m) v
                           _       -> Set.singleton i
ftv m (Pair v w)      =  ftv m v `Set.union` ftv m w
ftv m _               =  Set.empty
%%]

%%[3.ftv
ftv :: Val -> Set VarId
ftv (Var i r)       =  case refRead r of
                         Just v  -> ftv v
                         _       -> Set.singleton i
ftv (Pair v w)      =  ftv v `Set.union` ftv w
ftv _               =  Set.empty
%%]

%%[3 -(1.Substitutable 1.Substitutable.infixr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Variable identification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.VarId
type VarId = Int
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value which we want to compute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Val
data Val                        -- concrete syntax:
  =  Pair   !Val    !Val        -- (v,w)
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
%%[[Substitutable.Val.sbs
instance Substitutable Val where
  s |@ v
%%[[1
    =  sbs s v
    where  sbs  s  (Pair v w)    =  Pair (sbs s v) (sbs s w)
           sbs  s  v@(Var i)     =  case i |? s of
                                      Just v'  -> v'
                                      _        -> v
           sbs  s  v             =  v
%%][2
    =  sbs Set.empty s v
    where  sbs visited s (Pair v w)  =  Pair v' w'
             where  v'  = sbs visited s v
                    w'  = sbs visited s w
           sbs visited s v@(Var i)   =
             case i |? s of
               Just v'
                 |  Set.member i visited
                      -> valErr "infinite"
                 |  otherwise
                      -> sbs (Set.insert i visited) s v'
               _      -> v
           sbs visited s v           =  v
%%][21
    =  fst $ sbs Set.empty s v
    where  sbs  visited  s  (Pair v w)    =  (Pair v' w', s3)
             where  (v',s2)  = sbs visited s   v
                    (w',s3)  = sbs visited s2  w
           sbs  visited  s  v@(Var i)     = 
             case i |? s of
               Just v' | Set.notMember i visited
                  -> (v'',vmUnit i v'' `vmUnion` s')
                  where (v'',s') = sbs (Set.insert i visited) s v'
               _  -> (v,s)
           sbs  visited  s  v             =  (v,s)
%%]]
%%]]

%%[[1
  ftv (Var i)         =  Set.singleton i
  ftv (Pair v w)      =  ftv v `Set.union` ftv w
  ftv _               =  Set.empty
%%][221
%%]]
%%]

%%[3 -1.Substitutable.Val
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
  pp (Pair v w)  = ppParens (v >|< "," >|< w)
  pp (Const)     = pp "c"
  pp (Err s)     = "err:" >#< s
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.VMp
newtype VMp = VMp (Map VarId Val)
%%]

%%[1.VMp.sigs
emptyVM          ::  VMp
(|?)             ::  VarId -> VMp -> Maybe Val    -- lookup
vmUnit           ::  VarId -> Val -> VMp
vmUnion          ::  VMp -> VMp -> VMp
%%]

%%[221
vmDelete :: VarId -> VMp -> VMp
vmDelete i (VMp m) = VMp (Map.delete i m)
%%]

%%[1.emptyVM
emptyVM          =   VMp Map.empty
%%]

%%[1.varmpLookup
k |? (VMp s)      =   Map.lookup k s
%%]

%%[2.varmpLookup -1.varmpLookup
k |? (VMp s)
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
vmUnit n v = VMp (Map.singleton n v)

vmUnion (VMp x) (VMp y) = VMp (x `Map.union` y)
%%]

%%[1.Substitutable.VMp
%%[[Substitutable.VMp.sbs
instance Substitutable VMp where
%%[[1
  s |@   (VMp m)    =  s `vmUnion` VMp (Map.map (s |@) m)
%%][2
  s |@   s2         =  s `vmUnion` s2
%%]]
%%]]
%%[[1
  ftv    (VMp m)    =  Map.fold  (\v fv -> fv `Set.union` ftv v)
                                 Set.empty m
%%][221
%%]]
%%]

%%[3 -(1.VMp.sigs 1.VMp 1.emptyVM 2.varmpLookup 1.vmUnit 1.Substitutable.VMp)
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
  s |@   (Env m)  = Env (Map.map (s |@) m)
%%[[1
  ftv    (Env m)  = Map.fold (\v fv -> fv `Set.union` ftv v) Set.empty m
%%][221
%%]]
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
  |  Tuple       !Tree    !Tree          -- (x,y)
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
  pp (Tuple    x y    )  = "(" >|< x >-< "," >|< y >|< ")"
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

Sequence of intro's, Pair's, deconstructed in subsequent uses

%%[1
mkLinearTree :: Int -> Tree
mkLinearTree maxDepth
  = mk minDepth
  where mk dep
          = DefBind bnam bval buse
          where bnam = mkv dep
                bval | dep == minDepth      =  Constant
                     | dep == (minDepth+1)  =  Tuple
                                                 (mkuse1 dep)
                                                 (mkuse1 dep)
                     | otherwise            =  Tuple
                                                 (Second (mkuse1 dep))
                                                 (First  (mkuse1 dep))
                buse | dep == maxDepth      =  UseBind bnam
                     | otherwise            =  mk (dep+1)
        minDepth = 1
%%]

Growing types, each further definition constructs a Val double the size of the previous.

%%[1.mkExponentialTree
mkExponentialTree :: Int -> Tree
mkExponentialTree maxDepth
  = mk minDepth
  where mk dep
          = DefBind bnam bval buse
          where bnam = mkv dep
                bval | dep == minDepth      =  Constant
                     | dep == (minDepth+1)  =  Tuple
                                                 (mkuse1 dep)
                                                 (mkuse1 dep)
                     | dep == (minDepth+2)  =  Tuple
                                                 (First (mkuse1 dep))
                                                 (mkuse1 dep)
                     | otherwise            =  Tuple
                                                 (Second (mkuse1 dep))
                                                 (Tuple
                                                    (mkuse2 dep)
                                                    (mkuse2 dep))
                buse | dep == maxDepth      =  UseBind bnam
                     | otherwise            =  mk (dep+1)
        minDepth = 1
%%]

%%[4 -1.mkExponentialTree
mkExponentialTree :: Int -> Tree
mkExponentialTree maxDepth
  = mk minDepth
  where mk dep
          = DefBind bnam bval buse
          where bnam = mkv dep
                bval | dep == minDepth      =  Constant
                     | dep == (minDepth+1)  =  Tuple
                                                 (mkuse1 dep)
                                                 (mkuse1 dep)
                     | dep == (minDepth+2)  =  Tuple
                                                 (First (mkuse1 dep))
                                                 (mkuse1 dep)
                     | otherwise            =  Tuple
                                                 (Second (mkuse1 dep))
                                                 (Tuple
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
data St = St  {  stUniq     :: !VarId
              ,  stEnv      :: !Env
%%[[1
              ,  stVMp      :: !VMp
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
type Compute v = State St v
%%]

%%[3.Compute -1.Compute
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
              Just v -> return (stVMp st |@ v)
%%][2
              Just v -> return v
%%]]
              _      -> err $ "not found: " ++ show n
      DefBind n x y  ->
        do  v   <- treeCompute x
            st  <- get
            let env = stEnv st
            put (st {stEnv = envUnit n v `envUnion` env})
            w   <- treeCompute y
            st  <- get
            put (st {stEnv = env})
            return w
      Tuple x y       ->
        do  v   <- treeCompute x
            w   <- treeCompute y
%%[[1
            st  <- get
            return (Pair (stVMp st |@ v) w)
%%][2
            return (Pair v w)
%%]]
%%[[treeCompute.First
      First x        ->
        do  vw     <- treeCompute x
            [v,w]  <- newVars 2
            valUnify (Pair v w) vw
%%[[1
            st     <- get
            return (stVMp st |@ v)
%%][2
            return v
%%]]
%%]]
      Second x       ->
        do  vw     <- treeCompute x
            [v,w]  <- newVars 2
            valUnify (Pair v w) vw
%%[[1
            st     <- get
            return (stVMp st |@ w)
%%][2
            return w
%%]]
%%[[4
      Apply x y      ->
        do  x'     <- treeCompute x
            [v,w]  <- newVars 2
            valUnify (Pair v w) x'
            y'     <- treeCompute y
%%[[1
            st     <- get
            valUnify (stVMp st |@ v) y'
%%][2
            valUnify v y'
%%]]
%%[[1
            st     <- get
            return (stVMp st |@ w)
%%][2
            return w
%%]]
%%]]
%%]

%%[1.newVar
newVar      ::  Compute Val
newVar  =   do  st  <- get
                let fresh = stUniq st
                put (st {stUniq = fresh + 1})
%%[[1
                return (Var fresh)
%%][3
                r   <- newRef
                return (Var fresh r)
%%]]
%%]

%%[1.newVars
newVars     ::  Int -> Compute [Val]
newVars  0  =   return []
newVars  n  =   do  v   <- newVar
                    vs  <- newVars (n-1)
                    return (v : vs)
%%]

%%[3.Ref.IO.sigs
newRef     ::  Compute Ref
refRead    ::  Ref -> RefContent
refWrite   ::  Ref -> RefContent -> Compute ()
%%]

%%[3.newRef
newRef  =   do  r <- lift $ newIORef Nothing
                return (Ref r)
%%]

%%[3.Ref.IO
refRead   (Ref r)    =   unsafePerformIO $ readIORef r

refWrite  (Ref r) c  =   lift $ writeIORef r c
%%]

%%[1.valUnify
valUnify :: Val -> Val -> Compute Val
valUnify v1 v2
  = do { st <- get ; uni st v1 v2 } where
  uni  st  x@(Const)    (Const)                     =  return x
%%[[1
  uni  st  x@(Var i)    (Var j)    |  i == j        =  return x
%%][3
  uni  st  x@(Var i _)  (Var j _)  |  i == j        =  return x
%%]]
%%[[2
  uni  st  (Var i)      y          |  isJust mbv    =  uni st v y
       where  mbv  = i |? stVMp st
              v    = fromJust mbv
  uni  st  x            (Var j)    |  isJust mbv    =  uni st x v
       where  mbv  = j |? stVMp st
              v    = fromJust mbv
%%][3
  uni  st  (Var _ r)    y          |  isJust mbv    =  uni st v y
       where  mbv  = refRead r
              v    = fromJust mbv
  uni  st  x            (Var _ r)  |  isJust mbv    =  uni st x v
       where  mbv  = refRead r
              v    = fromJust mbv
%%]]
%%[[1
  uni  st  (Var i)      y                           =  bindv st i y
  uni  st  x            (Var i)                     =  bindv st i x
%%][3
  uni  st  x@(Var _ _)  y                           =  bindv st x y
  uni  st  x            y@(Var _ _)                 =  bindv st y x
%%]]
  uni  st  (Pair p q)   (Pair r s)                  =
      do  pr   <- uni st p r
          st2  <- get
%%[[1
          qs   <- uni st2 (stVMp st2 |@ q) (stVMp st2 |@ s)
          st3  <- get
          return (Pair (stVMp st3 |@ pr) qs)
%%][2
          qs   <- uni st2 q s
          return (Pair pr qs)
%%]]
  uni  _   _            _                           =  err "no match"
%%[[1
  bindv st i v 
      |  Set.member i (ftv v)                       =  err "infinite"
      |  otherwise                                  =
           do  put (st {stVMp = vmUnit i v |@ stVMp st})
               return v
%%][2
  bindv st i v                                      = 
    do  put (st {stVMp = vmUnit i v |@ stVMp st})
        return v
%%][221
  bindv st i v 
      |  Set.member i (ftv (stVMp st) v)            =  err "infinite"
      |  otherwise                                  =
           do  put (st {stVMp = vmUnit i v |@ stVMp st})
               return v
%%][3
  bindv st (Var i r) v  |  Set.member i (ftv v)     =  err "infinite"
                        |  otherwise                =
                             do  refWrite r (Just v)
                                 return v
%%]]
%%]

