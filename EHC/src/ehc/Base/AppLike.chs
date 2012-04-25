%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.AppLike} import(UU.Scanner.Position,EH.Util.Utils,{%{EH}Base.Common},{%{EH}Base.HsName},{%{EH}Base.Builtin})
%%]

%%[1 import(Control.Applicative((<|>)), Data.Maybe)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% App like structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AppLike export(AppLike(..))
%%[[AppLikeCore1
class AppLike a {- ann bnd | a -> ann bnd -} where
  -- basic semantics
  app1App           ::  a -> a -> a							-- single application
  appTop            ::  a -> a                              -- top of multiple apps
  appVar            ::  (Position n,HSNM n) => n -> a       -- variable
  appCon            ::  (Position n,HSNM n) => n -> a       -- constructor
  appPar            ::  a -> a                              -- parenthesis
%%]]
  -- and the defaults
  app1App           =   appRngApp1   emptyRange
  appTop            =   appRngTop    emptyRange
  appVar            =   appRngVar    emptyRange
  appCon            =   appRngCon    emptyRange
  appPar            =   appRngPar    emptyRange

  -- variation with Range
  appRngApp1        ::  Range -> a -> a -> a
  appRngTop         ::  Range -> a -> a
  appRngVar         ::  (Position n,HSNM n) => Range -> n -> a
  appRngCon         ::  (Position n,HSNM n) => Range -> n -> a
  appRngPar         ::  Range -> a -> a
  
  -- and the defaults
  appRngApp1   _    =   app1App
  appRngTop    _    =   appTop
  appRngVar    _    =   appVar
  appRngCon    _    =   appCon
  appRngPar    _    =   appPar
  
  -- inspection/deconstruction
  appMbAnn1         :: a -> Maybe (a->a,a)
  appMbTop1         :: a -> Maybe (a->a,a)
  appMbCon          :: a -> Maybe (HsName)
  appMbApp1         :: a -> Maybe (a,a)
  appUnApp          :: a ->       (a,[a])
  appMbConApp       :: a -> Maybe (HsName,[a])
  appUnArr          :: a ->       ([a],a)
  
  -- and the defaults
  appMbAnn1         = const Nothing
  appMbTop1         = appMbAnn1
  appMbCon          = const Nothing
  appMbApp1         = const Nothing
  appUnApp     x    = un [] (snd $ appUnTop x)
                    where un as x = case appMbApp1 x of
                                      Just (f,a) -> un (a:as) (snd $ appUnAnn f)
                                      _          -> (x,as)
  appMbConApp  x    = do { (f,as) <- appMbApp x
                         ; c <- appMbCon f
                         ; return (c,as)
                         }
  appUnArr  x       = case appMbApp x of
                        Just (fx,asx) -> case appMbCon fx of
                                           Just con | hsnIsArrow con -> (arg:as,r)
                                                                     where [arg,res] = asx
                                                                           (as,r) = appUnArr res
                                           _                         -> dflt
                        _             -> dflt
                    where dflt = ([],x)

  -- specialised constructing
  -- | Make application wrapped in top, except for singleton
  appTopApp         ::  [a] -> a

  -- and the defaults
  appTopApp         =   appRngTopApp emptyRange

  -- variation with Range
  appRngTopApp      ::  Range -> [a] -> a

  -- and the defaults
  appRngTopApp  r [a] = a
  appRngTopApp  r as  = appRngTop r (foldl1 (appRngApp1 r) as)

%%[[AppLikeCore2
  mkProdApp         ::  [a] -> a
%%]]
  mk1ConApp         ::  (Position n,HSNM n) => n -> a -> a
  -- constructin with Range
  mk1RngApp         ::  Range -> a -> a -> a
  mkRngProd         ::  Range -> [a] -> a
  
  -- defaults semantics
  -- defaults
  mk1ConApp  c a    =   appConApp c [a]
  mkProdApp  as     =   appConApp (hsnProd (length as)) as
  -- defaults with Range
  mkRngProd _   as  =   mkProdApp as            -- to be done
  mk1RngApp rng a r =   appRngTopApp rng [a,r]

  -- default inspection
                         
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived constructing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(appTopApp1)
-- | Make single application, with top
appTopApp1 :: AppLike a {- ann bnd -} => a -> a -> a
appTopApp1 a r = appTopApp [a,r]
{-# INLINE appTopApp1 #-}
%%]

%%[1 export(appRngProdOpt)
-- | Make product, except for singleton
appRngProdOpt :: AppLike a {- ann bnd -} => Range -> [a] -> a
appRngProdOpt r [a] = a
appRngProdOpt r as  = mkRngProd r as
%%]

%%[1 export(appRngParApp)
-- | Make parenthesized app, except for singleton
appRngParApp :: AppLike a {- ann bnd -} => Range -> [a] -> a
appRngParApp r [a] = a
appRngParApp r as  = appRngPar r (appRngTopApp r as)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived constructing: arrow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(appConApp)
-- | Make constructor applied to arguments
appConApp          ::  (AppLike a {- ann bnd -}, Position n, HSNM n) => n -> [a] -> a
appConApp   c as   =   appTopApp (appCon c : as)
{-# INLINE appConApp #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived constructing: arrow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(app1Arr,appArr)
-- | Make (type) rep for single arrow (i.e. abstraction)
app1Arr :: AppLike a {- ann bnd -} => a -> a -> a
app1Arr a r = appConApp hsnArrow [a,r] -- appTopApp [appCon hsnArrow,a,r]
{-# INLINE app1Arr #-}

-- | Multiple app1Arr
appArr :: AppLike a {- ann bnd -} => [a] -> a -> a
appArr = flip (foldr app1Arr)
{-# INLINE appArr #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived constructing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
-- | Given a single level unwrap, deepnested unwrap top like stuff, also giving reconstruction
appMb2Un :: (a -> Maybe (a->a,a)) -> a -> (a->a,a)
appMb2Un un a
  = case un a of
      Just (mk1,a') -> (mk1 . mk, a'')
                    where (mk,a'') = appMb2Un un a'
      _ -> (id,a)
%%]

%%[1 export(appUnAnn,appUnTop)
-- | Unwrap ann like stuff, also giving reconstruction
appUnAnn :: AppLike a {- ann bnd -} => a -> (a->a,a)
appUnAnn = appMb2Un appMbAnn1

-- | Unwrap top like stuff, also giving reconstruction
appUnTop :: AppLike a {- ann bnd -} => a -> (a->a,a)
appUnTop = appMb2Un appMbTop1
%%]

%%[1 export(appMbApp,appMbArr)
-- | Wrap app unpacking into Maybe
appMbApp :: AppLike a {- ann bnd -} => a -> Maybe (a,[a])
appMbApp x
  = case appUnApp x of
      u@(_,(_:_)) -> Just u
      _           -> Nothing

-- | Wrap arr unpacking into Maybe
appMbArr :: AppLike a {- ann bnd -} => a -> Maybe ([a],a)
appMbArr x 
  = case appUnArr x of
	  a@((_:_),_) -> Just a
	  _           -> Nothing
%%]


