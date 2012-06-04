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
class AppLike a boundmeta {- ann bnd | a -> ann bnd -}
  | a -> boundmeta
  -- , boundmeta -> a
  where
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
  
  -- inspection/deconstruction
  appMbBind1        :: a -> Maybe (a,a->a)					-- strip binding, if any, also giving reconstruction
  appMbAnn1         :: a -> Maybe (a,a->a)					-- strip annotation, if any, also giving reconstruction
  appMbTop1         :: a -> Maybe (a,a->a)					-- strip top of app, if any, also giving reconstruction
  appMbCanon1       :: a -> Maybe (a,a->a)					-- minimal canonicalization (e.g. strip empty implicits), if any, also giving reconstruction
  appMbCon          :: a -> Maybe (HsName)					-- is con?
  appMbApp1         :: a -> Maybe (a,a)						-- is app?
  appMbDbg          :: a -> Maybe String					-- is dbg?
  
  -- and the defaults
  appMbBind1        = const Nothing
  appMbAnn1         = const Nothing
  appMbTop1         = appMbAnn1
  appMbCanon1       = const Nothing
  appMbCon          = const Nothing
  appMbApp1         = const Nothing
  appMbDbg          = const Nothing

  -- specialised constructing
  -- | Make application wrapped in top, except for singleton
  appTopApp         ::  [a] -> a
  appProdApp        ::  [a] -> a
  app1MetaArr       :: 	(Maybe HsName,boundmeta) -> a -> a -> a

  -- and the defaults
  appTopApp         =   appRngTopApp emptyRange
  appProdApp    as  =   appConApp (hsnProd (length as)) as
  app1MetaArr _ a r =   appConApp hsnArrow [a,r]

  -- variation with Range
  appRngTopApp      ::  Range -> [a] -> a
  appRngProdApp     ::  Range -> [a] -> a

  -- and the defaults
  appRngTopApp  r [a] = a
  appRngTopApp  r as  = appRngTop r (foldl1 (appRngApp1 r) as)

  appRngProdApp _ as  = appProdApp as            -- to be done
  
  -- specialised deconstructing
  -- | Wrap 1 arr unpacking into Maybe, together with reconstruction function for toplevel unwrapping
  appMb1ArrMk 		:: a -> Maybe (((HsName,boundmeta),a,a),a->a)

  -- and the defaults
  appMb1ArrMk x
    = do let (x',mktop) = appUnBind $ fst $ appUnAnn x
         (arr,[a,r]) <- appMbConApp x'
         if hsnIsArrow arr then return (((mkHNm "??TermLike.appMb1ArrMk",appDfltBoundmeta a),a,r),mktop) else Nothing
  
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
  
  -- fallback, default value
  -- appDflt			:: a
  appDfltBoundmeta  :: a -> boundmeta	-- the 'a' is only required because a fundep boundmeta -> a would be too restrictive
  appDfltBoundmeta _ = panic "TermLike.appDfltBoundmeta not implemented"
  
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion between two AppLikes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(appToApp)
appToApp :: (AppLike a aboundmeta, AppLike b bboundmeta) => a -> Maybe b
appToApp x
  =   c appMbCon appCon x
  <|> c appMbDbg appDbg x
  where c mbUn mk = fmap mk . mbUn
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BndLike: binder/binding/bound like (20120502 AD: under design...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(BndLike(..))
class {- AppLike a boundmeta => -} BndLike a bndnm where
  bndBndIn			:: bndnm -> MetaLev -> a -> a -> a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RecLike: record/product like
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(RecLike(..))
class AppLike a boundmeta => RecLike a boundmeta
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
appTopApp1 :: AppLike a boundmeta {- ann -} => a -> a -> a
appTopApp1 a r = appTopApp [a,r]
{-# INLINE appTopApp1 #-}
%%]

%%[1 export(appRngProdOpt)
-- | Make product, except for singleton
appRngProdOpt :: AppLike a boundmeta {- ann -} => Range -> [a] -> a
appRngProdOpt r [a] = a
appRngProdOpt r as  = appRngProdApp r as
%%]

%%[1 export(appRngParApp)
-- | Make parenthesized app, except for singleton
appRngParApp :: AppLike a boundmeta {- ann -} => Range -> [a] -> a
appRngParApp r [a] = a
appRngParApp r as  = appRngPar r (appRngTopApp r as)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AppLike: Derived constructing: arrow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(appConApp,appCon1App)
-- | Make constructor applied to arguments
appConApp :: (AppLike a boundmeta {- ann -}, Position n, HSNM n) => n -> [a] -> a
appConApp c as = appTopApp (appCon c : as)
{-# INLINE appConApp #-}

-- | See 'appCon1App', just for 1 arg
appCon1App :: (AppLike a boundmeta {- ann -}, Position n, HSNM n) => n -> a -> a
appCon1App c a = appConApp c [a]
{-# INLINE appCon1App #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived constructing: arrow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(app1Arr,appArr)
-- | Make (type) rep for single arrow (i.e. abstraction)
app1Arr :: AppLike a boundmeta {- ann -} => a -> a -> a
app1Arr x y = app1MetaArr (Nothing,appDfltBoundmeta x) x y
{-# INLINE app1Arr #-}

-- | Multiple app1Arr
appArr :: AppLike a boundmeta {- ann -} => [a] -> a -> a
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

%%[1 export(appUnAnn,appUnTop,appUnBind,appUnAnnCanon)
-- | Unwrap binding like stuff (i.e. quantifiers), also giving reconstruction
appUnBind :: AppLike a boundmeta {- ann -} => a -> (a,a->a)
appUnBind = appMb2Un appMbBind1
{-# INLINE appUnBind #-}

-- | Unwrap ann like stuff, also giving reconstruction
appUnAnn :: AppLike a boundmeta {- ann -} => a -> (a,a->a)
appUnAnn = appMb2Un appMbAnn1
{-# INLINE appUnAnn #-}

-- | Unwrap ann+canonic like stuff, also giving reconstruction
appUnAnnCanon :: AppLike a boundmeta {- ann -} => a -> (a,a->a)
appUnAnnCanon = appMb2Un (\a -> appMbAnn1 a <|> appMbCanon1 a)

-- | Unwrap top like stuff, also giving reconstruction
appUnTop :: AppLike a boundmeta {- ann -} => a -> (a,a->a)
appUnTop = appMb2Un appMbTop1
{-# INLINE appUnTop #-}
%%]

%%[1 export(appUnApp)
-- | Unpack app into function and args
appUnApp :: AppLike a boundmeta {- ann -} => a -> (a,[a])
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
appUnAppArgs :: AppLike a boundmeta {- ann -} => a -> [a]
appUnAppArgs = snd . appUnApp
{-# INLINE appUnAppArgs #-}
%%]

%%[1 export(appMbApp,appMbConApp)
-- | Wrap app unpacking into Maybe
appMbApp :: AppLike a boundmeta {- ann -} => a -> Maybe (a,[a])
appMbApp x
  = case appUnApp x of
      u@(_,(_:_)) -> Just u
      _           -> Nothing

-- | Wrap app into constructor name applied to args
appMbConApp :: AppLike a boundmeta {- ann -} => a -> Maybe (HsName,[a])
appMbConApp x
  = do let (f,as) = appUnApp x
       c <- appMbCon $ fst $ appUnAnn f
       return (c,as)
       
%%]

%%[1 export(appMb1MetaArr,appMb1Arr,appMbArr,appUnMetaArrMk,appUnArrMk,appUnMetaArr,appUnArr,appUn1Arr)
appMb1MetaArr :: AppLike a boundmeta {- ann -} => a -> Maybe ((HsName,boundmeta),a,a)
appMb1MetaArr = fmap fst . appMb1ArrMk
{-# INLINE appMb1MetaArr #-}

appMb1Arr :: AppLike a boundmeta {- ann -} => a -> Maybe (a,a)
appMb1Arr = fmap (\(_,x,y) -> (x,y)) . appMb1MetaArr
{-# INLINE appMb1Arr #-}

-- | Wrap arr unpacking into Maybe
appMbArr :: AppLike a boundmeta {- ann -} => a -> Maybe ([a],a)
appMbArr x 
  = case appUnArr x of
      a@((_:_),_) -> Just a
      _           -> Nothing

-- | Arr unpacking, together with reconstruction function for toplevel unwrapping
appUnMetaArrMk :: AppLike a boundmeta {- ann -} => a -> (([((HsName,boundmeta),a)],a),a->a)
appUnMetaArrMk x
  = case appMb1ArrMk x of
      Just ((m,a,r),mk) -> (((m,a):as,r'),mk)
                        where ((as,r'),_) = appUnMetaArrMk r
      _                 -> (([],x),id)

-- | Arr unpacking, together with reconstruction function for toplevel unwrapping
appUnArrMk :: AppLike a boundmeta {- ann -} => a -> (([a],a),a->a)
appUnArrMk x
  = ((map snd as,r),mk)
  where ((as,r),mk) = appUnMetaArrMk x
{-
  = case appMb1ArrMk x of
      Just ((_,a,r),mk) -> ((a:as,r'),mk)
                        where ((as,r'),_) = appUnArrMk r
      _                 -> (([],x),id)
-}

-- | Arr unpacking into args + res
appUnMetaArr :: AppLike a boundmeta {- ann -} => a -> ([((HsName,boundmeta),a)],a)
appUnMetaArr = fst . appUnMetaArrMk
{-# INLINE appUnMetaArr #-}

-- | Arr unpacking into args + res
appUnArr :: AppLike a boundmeta {- ann -} => a -> ([a],a)
appUnArr = fst . appUnArrMk
{-# INLINE appUnArr #-}

-- | Arr unpacking into arg + res, when failing to unpack arg holds a default
appUn1Arr :: AppLike a boundmeta {- ann -} => a -> (a,a)
appUn1Arr x = maybe (panic "appUn1Arr.arg",x) id $ appMb1Arr x
{-# INLINE appUn1Arr #-}
%%]

%%[1 export(appUnArrArgs,appUnArrRes,appUnArrArg)
-- | Arr unpacking, args only
appUnArrArgs :: AppLike a boundmeta {- ann -} => a -> [a]
appUnArrArgs = fst . appUnArr
{-# INLINE appUnArrArgs #-}

-- | Arr unpacking, res only
appUnArrRes :: AppLike a boundmeta {- ann -} => a -> a
appUnArrRes = snd . appUnArr
{-# INLINE appUnArrRes #-}

-- | Arr unpacking, arg only
appUnArrArg :: AppLike a boundmeta {- ann -} => a -> a
appUnArrArg = fst . appUn1Arr
{-# INLINE appUnArrArg #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AppLike: Derived constructing: misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(appArrInverse)
-- |  inverse type, i.e. a->b gives b->a, a->b->c gives c->(a,b)
appArrInverse :: AppLike a boundmeta {- ann -} => a -> a
appArrInverse x
  = case appUnArrMk x of
      ((   [a]  ,r),mk) -> mk $ [r] `appArr` a
      ((as@(_:_),r),mk) -> mk $ [r] `appArr` appProdApp as
      _                 -> x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RecLike: Derived inspecting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(recUnRecRow)
-- | If a row based record, return the row
recUnRecRow :: RecLike a boundmeta => a -> a
recUnRecRow = maybe (panic "recUnRecRow") id . recMbRecRow
{-# INLINE recUnRecRow #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RecLike: Derived constructing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(recRec,recSum,recRecExt,recRecEmp, recSumEmp)
-- | Construct record from labels + terms
recRec :: RecLike a boundmeta => AssocL HsName a -> a
recRec al = hsnRec `appConApp` [recRowEmp `recRow` al]
{-# INLINE recRec #-}

-- | Construct record from labels + terms
recSum :: RecLike a boundmeta => AssocL HsName a -> a
recSum al = hsnSum `appConApp` [recRowEmp `recRow` al]
{-# INLINE recSum #-}

-- | Construct record from record to be extended + labels + terms
recRecExt :: RecLike a boundmeta => a -> AssocL HsName a -> a
recRecExt recd al
  = hsnRec `appConApp` [row `recRow` (exts ++ al)]
  where (row,exts) = recUnRowExts (recUnRecRow recd)

-- | Empty record
recRecEmp :: RecLike a boundmeta => a
recRecEmp = recRec []
{-# INLINE recRecEmp #-}

-- | Empty sum
recSumEmp :: RecLike a boundmeta => a
recSumEmp = recSum []
{-# INLINE recSumEmp #-}
%%]
