%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.AppLike} import(UU.Scanner.Position,EH.Util.Utils,{%{EH}Base.Common},{%{EH}Base.HsName},{%{EH}Base.Builtin})
%%]

%%[1 import(Control.Applicative((<|>)))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% App like structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AppLike export(AppLike(..))
%%[[AppLikeCore1
class AppLike a where
  -- basic semantics
  semApp            ::  a -> a -> a
  semAppTop         ::  a -> a
  semVar            ::  (Position n,HSNM n) => n -> a
  semCon            ::  (Position n,HSNM n) => n -> a
  semParens         ::  a -> a
%%]]
  -- basic semantics with Range
  semRngApp         ::  Range -> a -> a -> a
  semRngAppTop      ::  Range -> a -> a
  semRngVar         ::  (Position n,HSNM n) => Range -> n -> a
  semRngCon         ::  (Position n,HSNM n) => Range -> n -> a
  semRngParens      ::  Range -> a -> a
  -- constructing
%%[[AppLikeCore2
  mkApp             ::  [a] -> a
  mkConApp          ::  (Position n,HSNM n) => n -> [a] -> a
  mkProdApp         ::  [a] -> a
  mk1Arrow          ::  a -> a -> a
  mkArrow           ::  [a] -> a -> a
%%]]
  mk1App            ::  a -> a -> a
  mk1ConApp         ::  (Position n,HSNM n) => n -> a -> a
  -- constructin with Range
  mk1RngApp         ::  Range -> a -> a -> a
  mkRngApp          ::  Range -> [a] -> a
  mkRngProd         ::  Range -> [a] -> a
  
  -- inspection/deconstruction
  unTop				:: a -> a
  isCon             :: a -> Maybe (HsName)
  isApp1            :: a -> Maybe (a,a)
  isApp             :: a -> Maybe (a,[a])
  isConApp          :: a -> Maybe (HsName,[a])
  isArrow           :: a -> Maybe ([a],a)
  unArrow           :: a -> ([a],a)

  -- defaults semantics
  semApp            =   semRngApp    emptyRange
  semAppTop         =   semRngAppTop emptyRange
  semVar            =   semRngVar    emptyRange
  semCon            =   semRngCon    emptyRange
  semParens         =   semRngParens emptyRange
  semRngApp    _    =   semApp
  semRngAppTop _    =   semAppTop
  semRngVar    _    =   semVar
  semRngCon    _    =   semCon
  semRngParens _    =   semParens
  -- defaults
  mkApp             =   mkRngApp emptyRange
  mk1App     a r    =   mkApp [a,r]
  mkConApp   c as   =   mkApp (semCon c : as)
  mk1ConApp  c a    =   mkConApp c [a]
  mkProdApp  as     =   mkConApp (hsnProd (length as)) as
  mk1Arrow   a r    =   mkApp [semCon hsnArrow,a,r]
  mkArrow           =   flip (foldr mk1Arrow)
  -- defaults with Range
  mkRngProd rng     =   mkProdApp               -- to be done
  mk1RngApp rng a r =   mkRngApp rng [a,r]
  mkRngApp  rng as  =   case as of
                          [a] -> a
                          _   -> semRngAppTop rng (foldl1 (semRngApp rng) as)

  -- default inspection
  unTop             = id
  isCon             = const Nothing
  isApp1            = const Nothing
  isApp     x       = do { (f1,a) <- isApp1 $ unTop x
                         ; (do {(f2,as) <- isApp f1; return (f2,as++[a])}) <|> (return (f1,[a]))
                         }
  isConApp  x       = do { (f,as) <- isApp x
                         ; c <- isCon f
                         ; return (c,as)
                         }
  unArrow   x       = case isApp x of
                        Just (fx,asx) -> case isCon fx of
                                           Just con | hsnIsArrow con -> (arg:as,r)
                                                                     where [arg,res] = asx
                                                                           (as,r) = unArrow res
                                           _                         -> dflt
                        _             -> dflt
                    where dflt = ([],x)
  isArrow   x       = case unArrow x of
                        a@((_:_),_) -> Just a
                        _           -> Nothing
                         
%%]

%%[1 export(mkRngProdOpt)
mkRngProdOpt :: AppLike e => Range -> [e] -> e
mkRngProdOpt r [e] = e
mkRngProdOpt r es  = mkRngProd r es
%%]

%%[1 export(mkRngParApp)
mkRngParApp :: AppLike e => Range -> [e] -> e
mkRngParApp r [a] = a
mkRngParApp r as  = semRngParens r (mkRngApp r as)
%%]


