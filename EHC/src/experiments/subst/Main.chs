%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Experiment: substitution variations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main
%%]

%%[1 import(System, Data.Char, Control.Monad, Control.Monad.State, System.Console.GetOpt, IO)
%%]

%%[1 import(Data.Map as Map, Data.Set as Set, Data.List as List)
%%]

%%[1 import(EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% main entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  do  args@((kind:'/':dep):_) <- getArgs
         let t = case kind of
                        'b' -> mkTree2 (read dep)
                        _   -> error [kind]
         -- putPPLn (pp t)
         let (r,_) = runState (treeCompute t) emptySt
         -- putPPLn (pp r)
         putStrLn (r `seq` "done")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substituter class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
%%]
class Substituter s k v where
  (|?)        :: k -> s -> Maybe v
  emptySubst  :: s

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Substitutable
infixr 6 |=>
%%]

%%[4
infixr 6 |==>
%%]

%%[1.Substitutable
class Substitutable v where
  (|=>)         ::  VarMp -> v -> v
  ftv           ::  v -> Set VarId
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

%%[1
data Val
  = Var    !VarId
  | Comb   !Val !Val
  | Const
  | Err    !PP_Doc
  deriving Show

%%]
valIsErr :: Val -> Bool
valIsErr (Err _) = True
valIsErr _       = False

%%[1
err :: PP x => x -> Val
err x = Err (pp x)
%%]

%%[1
instance Substitutable Val where
  s |=> v@(Var i)     =  case i |? s of
                           Just v'  -> v'
                           _        -> v
  s |=> (Comb v w)    =  Comb (s |=> v) (s |=> w)
  s |=> v             =  v
  
  ftv (Var i)         =  Set.singleton i
  ftv (Comb v w)      =  ftv v `Set.union` ftv w
  ftv _               =  Set.empty
%%]

%%[1
instance PP Val where
  pp (Var i)     = "v" >|< pp i
  pp (Comb v w)  = ppParens (v >#< w)
  pp (Const)     = pp "c"
  pp (Err s)     = "err:" >#< s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
newtype VarMp = VarMp (Map VarId Val)

emptyVarMp          ::  VarMp
emptyVarMp          =   VarMp Map.empty
%%]

%%[1
(|?)                ::  VarId -> VarMp -> Maybe Val
k |? (VarMp s)      =   Map.lookup k s

varmpSingleton :: VarId -> Val -> VarMp
varmpSingleton n v = VarMp (Map.singleton n v)

varmpUnion :: VarMp -> VarMp -> VarMp
varmpUnion (VarMp x) (VarMp y) = VarMp (x `Map.union` y)
%%]
instance Substituter VarMp VarId Val where
  k |? (VarMp s)      = Map.lookup k s
  emptySubst          = VarMp Map.empty

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
newtype Env = Env (Map String Val)

emptyEnv :: Env
emptyEnv = Env Map.empty
%%]

%%[1
instance Substitutable Env where
  s |=>  (Env m)  = Env (Map.map (s |=>) m)
  ftv    (Env m)  = Map.fold (\v fv -> fv `Set.union` ftv v) Set.empty m
%%]

%%[1
envLookup :: String -> Env -> Maybe Val
envLookup n (Env m) = Map.lookup n m

envSingleton :: String -> Val -> Env
envSingleton n v = Env (Map.singleton n v)

envUnion :: Env -> Env -> Env
envUnion (Env x) (Env y) = Env (x `Map.union` y)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tree over which we compute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
data Tree
  = Constant
  | UseBind     !String
  | DefBind     !String !Tree !Tree
  | Pair        !Tree !Tree
  | App         !Tree !Tree
  | Fst         !Tree
  | Snd         !Tree
  deriving Show
%%]

%%[1
instance PP Tree where
  pp (Constant        )  = pp "constant"
  pp (UseBind  n      )  = pp n
  pp (DefBind  n x y  )  = "def" >#< n >#< "=" >#< x >#< "in" >-< y
  pp (Pair     x y    )  = ppParens x >-< ppParens y
  pp (App      x y    )  = "@" >#< (ppParens x >-< ppParens y)
  pp (Fst      x      )  = "fst" >#< x
  pp (Snd      x      )  = "snd" >#< x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tree generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Growing types, each further definition constructs a Val double the size of the previous.

%%[1
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
                                                 (Fst (mkuse1 dep))
                                                 (mkuse1 dep)
                     | otherwise            =  Pair
                                                 (Snd (mkuse1 dep))
                                                 (Pair
                                                    (App
                                                       (mkuse1 dep)
                                                       (mkuse3 dep))
                                                    (mkuse2 dep))
                buse | dep == maxDepth      =  UseBind bnam
                     | otherwise            =  mk (dep+1)
        minDepth = 1
        mkv    dep = "v" ++ show dep
        mkuse  dep = UseBind (mkv dep)
        mkuse1 dep = mkuse (dep-1)
        mkuse2 dep = mkuse (dep-2)
        mkuse3 dep = mkuse (dep-3)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compute Val over Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
data St
  = St
      {  stUniq     :: !VarId
      ,  stVarMp    :: !VarMp
      ,  stEnv      :: !Env
      }

emptySt  ::  St
emptySt  =   St 0 emptyVarMp emptyEnv
%%]

%%[1
type Compute = State St Val
%%]

%%[1
treeCompute :: Tree -> Compute
treeCompute t
  = case t of
      Constant       ->  return Const
      UseBind n      ->  do  st <- get
                             case envLookup n (stEnv st) of
                               Just v -> return (stVarMp st |=> v)
                               _      -> return (err $ "not found: " ++ show n)
      DefBind n x y  ->  do  x' <- treeCompute x
                             st <- get
                             let env = stEnv st
                             put (st {stEnv = envSingleton n x' `envUnion` env})
                             y' <- treeCompute y
                             st <- get
                             put (st {stEnv = env})
                             return y'
      Pair x y       ->  do  x' <- treeCompute x
                             y' <- treeCompute y
                             return (Comb x' y')
      Fst x          ->  do  x' <- treeCompute x
                             i <- newVarId
                             j <- newVarId
                             valMatch (Comb (Var i) (Var j)) x'
                             st <- get
                             return (stVarMp st |=> Var i)
      Snd x          ->  do  x' <- treeCompute x
                             i <- newVarId
                             j <- newVarId
                             valMatch (Comb (Var i) (Var j)) x'
                             st <- get
                             return (stVarMp st |=> Var j)
      App x y        ->  do  x' <- treeCompute x
                             i <- newVarId
                             j <- newVarId
                             valMatch (Comb (Var i) (Var j)) x'
                             y' <- treeCompute y
                             st <- get
                             valMatch (stVarMp st |=> Var i) y'
                             st <- get
                             return (stVarMp st |=> Var j)
%%]

%%[1
newVarId  ::  State St VarId
newVarId  =   do  st <- get
                  let i = stUniq st
                  put (st {stUniq = i+1})
                  return i
%%]

%%[1
valMatch :: Val -> Val -> Compute
valMatch v w
  = m v w
  where  m  (Const)     (Const)                  =  return Const
         m  x@(Var i)   y@(Var j)  |  i == j     =  return x
                                   |  otherwise  =  bind i y
         m  (Var i)     y                        =  bind i y
         m  x           (Var j)                  =  bind j x
         m  (Comb p q)  (Comb r s)               =  do  pr <- m p r
                                                        st <- get
                                                        qs <- m (stVarMp st |=> q) (stVarMp st |=> s)
                                                        st <- get
                                                        return (Comb (stVarMp st |=> pr) qs)
         m  _           _                        =  return (err "clash")
         bind i v  |  i `Set.member` ftv v       =  return (err "occur check")
                   |  otherwise                  =  do  st <- get
                                                        put (st {stVarMp = varmpSingleton i v `varmpUnion` stVarMp st})
                                                        return v
%%]

