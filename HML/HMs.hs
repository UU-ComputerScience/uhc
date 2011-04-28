module HMs where

import HM hiding (typeCheck, unify)
import Control.Arrow(first)

import PrettyUtil (Name, fresh, Pretty, pp)

import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.List(foldl', sortBy, nub)
import Data.Function(on)

import Control.Monad.State  
import Control.Monad(liftM2)

data Session 
  =  Session { env :: Gamma
             , frs :: Int
             , sub :: Env
             } deriving Show
                 
type Infer a = StateT Session IO a
type Unify a = StateT Session IO a

setSubs :: Env -> Infer ()
setSubs env = get >>= \e -> put (e{sub= env})

clear :: Infer ()
clear = setSubs []

infer :: Expr -> Infer TyExpr
infer exp
  = case exp of
      (Expr_IConst         _) -> do setSubs []
                                    return $ TyExpr_Con "Int"
      (Expr_Var           nm) -> do setSubs []
                                    session <- get
                                    return $ fromMaybe (error $ "Variable '" ++ nm ++ "' not found") (lookup nm (env session))
      (Expr_Con           nm) -> do setSubs []
                                    return $ TyExpr_Con nm
      (Expr_Paren         ty) -> infer ty
      (Expr_Lam     arg body) -> do session <- get
                                    let (a, frs') = first TyExpr_Var (fresh $ frs session)
                                    let env'      = (arg, a):(env session)
                                    put (session{env = env', frs = frs'})
                                    ty <- infer body
                                    session' <- get
                                    return $ (appAll (sub session') a) `mkArrow` ty
      (Expr_App     func arg) -> do setSubs []
                                    session <- get
                                    let (b, frs') = first TyExpr_Var (fresh $ frs session)
                                    put (session{frs = frs'})
                                    
                                    func_ty <- infer func
                                    func_s  <- get
                                    let func_sub = sub func_s
                                    
                                    arg_ty  <- infer arg
                                    arg_s   <- get
                                    let arg_sub  = sub arg_s
                                    
                                    unify func_ty (arg_ty `mkArrow` b)
                                    
                                    session' <- get
                                    let newsession = session'{sub = func_sub ++ arg_sub ++ sub session'}
                                    put newsession
                                    liftIO $ print newsession
                                    return $ appAll (sub newsession) b                            
      (Expr_Let nm bind body) -> do session <- get
                                    ty <- infer bind
                                    let env' = (nm, ty):(env session)
                                    put (session{env = env'})
                                    infer body
                                    
unify :: TyExpr -> TyExpr -> Unify ()
unify (TyExpr_Paren    exp1) exp2                   = unify exp1 exp2
unify exp1                   (TyExpr_Paren    exp2) = unify exp1 exp2
unify (TyExpr_Con      c1nm) (TyExpr_Con      c2nm) | c1nm == c2nm = setSubs []
                                                    | otherwise    = error $ "Constructor mismatched: " ++ c1nm ++ " ==> " ++ c2nm
unify (TyExpr_App fun1 arg1) (TyExpr_App fun2 arg2) = do unify fun1 fun2
                                                         env1 <- get
                                                         let sub1 = sub env1
                                                         
                                                         unify (appAll sub1 arg1) (appAll sub1 arg2)
                                                         env2 <- get
                                                         let sub2 = sub env2
                                                         
                                                         setSubs $ sub1 ++ sub2                    
unify (TyExpr_Var      v1nm) e2@(TyExpr_Var   v2nm) | v1nm == v2nm = setSubs []
unify (TyExpr_Var      v1nm) e2                     = setSubs [(v1nm, e2)]                                             
unify e1                     (TyExpr_Var      v2nm) = setSubs [(v2nm, e1)]                                             
unify _                       _                     = error "mismatch in unify."                               

typeCheck :: Expr -> IO TyExpr
typeCheck exp = evalStateT (infer exp) session
  where session = Session [] 0 []