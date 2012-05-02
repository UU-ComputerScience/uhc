%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.TermLike} import(UU.Scanner.Position,EH.Util.Utils,{%{EH}Base.Common},{%{EH}Base.HsName},{%{EH}Base.Builtin})
%%]

%%[1 import(Control.Applicative((<|>)), Control.Monad, Data.Maybe)
%%]

%%[doesWhat doclatex
Term like behavior encapsulated in various class interfaces.
\begin{itemize}
\item AppLike: application, constructor, etc
\item RecLike: (extensible) record
\end{itemize}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AppLike: App like structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AppLike export(AppLike(..))
%%[[AppLikeCore1
-- | Application like terms.
--   Note: defaults for Range and non-Range variants are defined using eachother. Only one needs definition.
class AppLike a {- ann bnd | a -> ann bnd -} where
  ----------
  -- AppLike
  ----------
  
  -- basic semantics
  app1App           ::  a -> a -> a                         -- single application
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
  
  -- fallback, default value
  -- appDflt			:: a
  
  -- inspection/deconstruction
  appMbBind1        :: a -> Maybe (a,a->a)
  appMbAnn1         :: a -> Maybe (a,a->a)
  appMbTop1         :: a -> Maybe (a,a->a)
  appMbCon          :: a -> Maybe (HsName)
  appMbApp1         :: a -> Maybe (a,a)
  
  -- and the defaults
  appMbBind1        = const Nothing
  appMbAnn1         = const Nothing
  appMbTop1         = appMbAnn1
  appMbCon          = const Nothing
  appMbApp1         = const Nothing

  -- specialised constructing
  -- | Make application wrapped in top, except for singleton
  appTopApp         ::  [a] -> a
  appProdApp        ::  [a] -> a

  -- and the defaults
  appTopApp         =   appRngTopApp emptyRange
  appProdApp    as  =   appConApp (hsnProd (length as)) as

  -- variation with Range
  appRngTopApp      ::  Range -> [a] -> a
  appRngProdApp     ::  Range -> [a] -> a

  -- and the defaults
  appRngTopApp  r [a] = a
  appRngTopApp  r as  = appRngTop r (foldl1 (appRngApp1 r) as)

  appRngProdApp _ as  = appProdApp as            -- to be done
  
  -- misc: debugging (intended to return a more appropriate value of type 'a')
  appDbg			:: 	String -> a
  appDbg m			=	panic $ "TermLike.appDbg: " ++ m

  -- misc: evaluatedness (i.e. yes/no lazy/thunk)
  -- yes evaluated (no thunk, not lazy)
  appEvl			:: 	a -> a
  appEvl			=	id
  -- not evaluated (yes thunk, yes lazy)
  appNonEvl			:: 	a -> a
  appNonEvl			=	id
  
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BndLike: binder/binding/bound like (20120502 AD: under design...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(BndLike(..))
class {- AppLike a => -} BndLike a bndnm where
  bndBndIn			:: bndnm -> MetaLev -> a -> a -> a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RecLike: record/product like
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(RecLike(..))
class AppLike a => RecLike a
%%[[7
  where
  ----------
  -- RecLike
  ----------
  
  -- constructing
  recRow 			:: a -> AssocL HsName a -> a
  
  -- default values
  recRowEmp			:: a

  -- and the defaults
  recRowEmp			= appCon hsnRowEmpty

  -- inspection/deconstruction
  recMbRecRow 		:: a -> Maybe a
  recUnRowExts 		:: a -> (a,AssocL HsName a)

%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(rowCanonOrderBy,rowCanonOrder)
-- | Order on labels, given a comparison function
rowCanonOrderBy :: (o -> o -> Ordering) -> AssocL o a -> AssocL o a
rowCanonOrderBy cmp = sortByOn cmp fst

-- | Order on labels
rowCanonOrder :: AssocL HsName a -> AssocL HsName a
rowCanonOrder = rowCanonOrderBy rowLabCmp
{-# INLINE rowCanonOrder #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AppLike: Derived constructing
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
appRngProdOpt r as  = appRngProdApp r as
%%]

%%[1 export(appRngParApp)
-- | Make parenthesized app, except for singleton
appRngParApp :: AppLike a {- ann bnd -} => Range -> [a] -> a
appRngParApp r [a] = a
appRngParApp r as  = appRngPar r (appRngTopApp r as)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AppLike: Derived constructing: arrow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(appConApp,appCon1App)
-- | Make constructor applied to arguments
appConApp :: (AppLike a {- ann bnd -}, Position n, HSNM n) => n -> [a] -> a
appConApp c as = appTopApp (appCon c : as)
{-# INLINE appConApp #-}

-- | See 'appCon1App', just for 1 arg
appCon1App :: (AppLike a {- ann bnd -}, Position n, HSNM n) => n -> a -> a
appCon1App c a = appConApp c [a]
{-# INLINE appCon1App #-}
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
%%% AppLike: Derived constructing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
-- | Given a single level unwrap, deepnested unwrap top like stuff, also giving reconstruction
appMb2Un :: (a -> Maybe (a,a->a)) -> a -> (a,a->a)
appMb2Un un a
  = case un a of
      Just (a',mk1) -> (a'', mk1 . mk)
                    where (a'',mk) = appMb2Un un a'
      _ -> (a,id)
%%]

%%[1 export(appUnAnn,appUnTop,appUnBind)
-- | Unwrap binding like stuff (i.e. quantifiers), also giving reconstruction
appUnBind :: AppLike a {- ann bnd -} => a -> (a,a->a)
appUnBind = appMb2Un appMbBind1
{-# INLINE appUnBind #-}

-- | Unwrap ann like stuff, also giving reconstruction
appUnAnn :: AppLike a {- ann bnd -} => a -> (a,a->a)
appUnAnn = appMb2Un appMbAnn1
{-# INLINE appUnAnn #-}

-- | Unwrap top like stuff, also giving reconstruction
appUnTop :: AppLike a {- ann bnd -} => a -> (a,a->a)
appUnTop = appMb2Un appMbTop1
{-# INLINE appUnTop #-}
%%]

%%[1 export(appUnApp)
-- | Unpack app into function and args
appUnApp :: AppLike a {- ann bnd -} => a -> (a,[a])
appUnApp x
  = un [] (fst $ appUnTop x)
  where un as x = case appMbApp1 x' of
                    Just (f,a) -> un (a:as) f
                    _          -> (x',as)
                where x' = -- fst $ appMb2Un (\a -> appMbAnn1 a <|> appMbBind1 a) x
                           fst $ appUnBind $ fst $ appUnAnn x
%%]

%%[1 export(appUnAppArgs)
-- | Unpack app into function and args, args only
appUnAppArgs :: AppLike a {- ann bnd -} => a -> [a]
appUnAppArgs = snd . appUnApp
{-# INLINE appUnAppArgs #-}
%%]

%%[1 export(appMbApp,appMbConApp)
-- | Wrap app unpacking into Maybe
appMbApp :: AppLike a {- ann bnd -} => a -> Maybe (a,[a])
appMbApp x
  = case appUnApp x of
      u@(_,(_:_)) -> Just u
      _           -> Nothing

-- | Wrap app into constructor name applied to args
appMbConApp :: AppLike a {- ann bnd -} => a -> Maybe (HsName,[a])
appMbConApp x
  = do let (f,as) = appUnApp x
       c <- appMbCon $ fst $ appUnAnn f
       return (c,as)
       
%%]

%%[1 export(appMb1Arr',appMb1Arr,appMbArr,appUnArr',appUnArr,appUn1Arr)
-- | Wrap 1 arr unpacking into Maybe, together with reconstruction function for toplevel unwrapping
appMb1Arr' :: AppLike a {- ann bnd -} => a -> Maybe ((a,a),a->a)
appMb1Arr' x
  = do let (x',mktop) = appUnBind $ fst $ appUnAnn x
       (arr,[a,r]) <- appMbConApp x'
       if hsnIsArrow arr then return ((a,r),mktop) else Nothing

appMb1Arr :: AppLike a {- ann bnd -} => a -> Maybe (a,a)
appMb1Arr = fmap fst . appMb1Arr'
{-# INLINE appMb1Arr #-}

-- | Wrap arr unpacking into Maybe
appMbArr :: AppLike a {- ann bnd -} => a -> Maybe ([a],a)
appMbArr x 
  = case appUnArr x of
      a@((_:_),_) -> Just a
      _           -> Nothing

-- | Arr unpacking, together with reconstruction function for toplevel unwrapping
appUnArr' :: AppLike a {- ann bnd -} => a -> (([a],a),a->a)
appUnArr' x
  = case appMb1Arr' x of
      Just ((a,r),mk) -> ((a:as,r'),mk)
                      where ((as,r'),_) = appUnArr' r
      _               -> (([],x),id)

-- | Arr unpacking into args + res
appUnArr :: AppLike a {- ann bnd -} => a -> ([a],a)
appUnArr = fst . appUnArr'
{-# INLINE appUnArr #-}

-- | Arr unpacking into arg + res, when failing to unpack arg holds a default
appUn1Arr :: AppLike a {- ann bnd -} => a -> (a,a)
appUn1Arr x = maybe (panic "appUn1Arr.arg",x) id $ appMb1Arr x
{-# INLINE appUn1Arr #-}
%%]

%%[1 export(appUnArrArgs,appUnArrRes,appUnArrArg)
-- | Arr unpacking, args only
appUnArrArgs :: AppLike a {- ann bnd -} => a -> [a]
appUnArrArgs = fst . appUnArr
{-# INLINE appUnArrArgs #-}

-- | Arr unpacking, res only
appUnArrRes :: AppLike a {- ann bnd -} => a -> a
appUnArrRes = snd . appUnArr
{-# INLINE appUnArrRes #-}

-- | Arr unpacking, arg only
appUnArrArg :: AppLike a {- ann bnd -} => a -> a
appUnArrArg = fst . appUn1Arr
{-# INLINE appUnArrArg #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AppLike: Derived constructing: misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(appArrInverse)
-- |  inverse type, i.e. a->b gives b->a, a->b->c gives c->(a,b)
appArrInverse :: AppLike a {- ann bnd -} => a -> a
appArrInverse x
  = case appUnArr' x of
      ((   [a]  ,r),mk) -> mk $ [r] `appArr` a
      ((as@(_:_),r),mk) -> mk $ [r] `appArr` appProdApp as
      _                 -> x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RecLike: Derived inspecting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(recUnRecRow)
-- | If a row based record, return the row
recUnRecRow :: RecLike a => a -> a
recUnRecRow = maybe (panic "recUnRecRow") id . recMbRecRow
{-# INLINE recUnRecRow #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RecLike: Derived constructing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(recRec,recSum,recRecExt,recRecEmp, recSumEmp)
-- | Construct record from labels + terms
recRec :: RecLike a => AssocL HsName a -> a
recRec al = hsnRec `appConApp` [recRowEmp `recRow` al]
{-# INLINE recRec #-}

-- | Construct record from labels + terms
recSum :: RecLike a => AssocL HsName a -> a
recSum al = hsnSum `appConApp` [recRowEmp `recRow` al]
{-# INLINE recSum #-}

-- | Construct record from record to be extended + labels + terms
recRecExt :: RecLike a => a -> AssocL HsName a -> a
recRecExt recd al
  = hsnRec `appConApp` [row `recRow` (exts ++ al)]
  where (row,exts) = recUnRowExts (recUnRecRow recd)

-- | Empty record
recRecEmp :: RecLike a => a
recRecEmp = recRec []
{-# INLINE recRecEmp #-}

-- | Empty sum
recSumEmp :: RecLike a => a
recSumEmp = recSum []
{-# INLINE recSumEmp #-}
%%]
