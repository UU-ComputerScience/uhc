{-% -----------------------------------------------------------------------------
% $Id: HsSyn.hs,v 1.1 2009/07/18 18:33:52 fdomin Exp $
%
%  Un conjunto de datatypes que describen la gramática que se parsea.
%
% -----------------------------------------------------------------------------}

%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.HsSyn}
%%]

%%[(8 codegen)

import Char(isDigit)
import List(union,(\\),nub)

-- Programa
data Prog = Prog [Def]

-- Definiciones del programa
data Def = Defvalue Variable Term
           | Defdata Data

getDefName :: Def -> Variable
getDefName (Defvalue v _) = v
getDefName (Defdata _) = error$ "getDefName: unexpected Defdata"

getDefTerm :: Def -> Term
getDefTerm (Defvalue _ t) = t
getDefTerm (Defdata _) = error$ "getDefTerm: unexpected Defdata"


-- Información de tipos
data Tipo = Typecons Constructor [Tipo]
            | Typefunc [Tipo]
            | Typetuple [Tipo]
            | Typelist Tipo
            | Typevar Variable

-- Representación de variables y literales

data Variable = Vuserdef String -- Nombre dado por el usuario
                | Vgen String Int -- Identifier of a generated var. The string is used as a prefix.
               deriving Ord

instance Show Variable where
    show (Vuserdef name) = name
    show (Vgen p num) = p ++ show num

instance Eq Variable where
  Vuserdef v == Vuserdef v' = v==v'
  Vgen p i == Vgen p' i' = p==p' && i==i'
  v1 == v2 = show v1 == show v2


str2var [] = Vuserdef ""
str2var s | null ds = Vuserdef s
          | otherwise = Vgen (reverse p) ((read (reverse ds))::Int)
  where (ds,p) = break (not . isDigit) (reverse s)

varPrefix :: Variable -> String
varPrefix (Vgen p _) = p
varPrefix (Vuserdef s) = reverse . dropWhile isDigit . dropWhile (=='_'). reverse$ s

data Literal = Lstring String
               | Lint String
               | Lchar Char
               | Lrat String
    deriving (Eq)

-- Representación de identificadores de constructores
type Constructor = String

-- Representación de terminos del lenguaje.
data Term = Tvar Variable   -- Variables
            | Tlit Literal  -- Literals
            | Ttuple Bool [Term] -- Tuples. The boolean argument tells if the tuple must be flatten
                                 -- when nested with others under an hylo application. Use False as
                                 -- default value.
            | Tlamb Boundvar Term -- Lambda expressions
            | Tlet Variable Term Term -- Let expressions
            | Tcase Term [Pattern] [Term] -- Case expressions
            | Tfapp Variable [Term] -- Function application (saturated)
            | Tcapp Constructor [Term] -- Constructor application
            | Tapp Term Term -- General term application
            | Tbottom  -- Undefined computation

            | Tif Term Term Term -- If expressions, only used for pretty printing
            | Tpar Term -- Parenthesized expressions, to better handle associativity of infix operators
            | Thyloapp Variable Int [Term] (Maybe [Int]) Term -- Hylo application, only used for inlining
                                                              -- Thyloapp name recargsCount non-recargs recarg
                                                              -- recarg may be a tuple 
    deriving (Eq)


-- Calculates arguments of a Thyloapp

thyloArgs :: Int -> [Term] -> Maybe [Int] -> Term -> [Term]
thyloArgs 1 ts pos t = insertElems (zip ts$ maybe [0..] id pos) [t]
thyloArgs 0 ts pos t = insertElems (zip ts [0..]) []
thyloArgs i ts pos t = insertElems (zip ts$ maybe [0..] id pos) (flatten t)
 where flatten (Ttuple True ts) = concat (map flatten ts)
       flatten t = [t]


insertElems :: [(a,Int)] -> [a] -> [a]
insertElems = insert 0
  where insert i xs [] = map fst xs 
        insert i [] as = as
        insert i xs@((x,ix):xss) as@(a:ass)
                 | ix<=i = x : insert (i+1) xss as
                 | otherwise = a : insert (i+1) xs ass

applyFst f (a,b) = (f a,b)


bv2pat (Bvar v)=Pvar v
bv2pat (Bvtuple _ vs)=Ptuple (map bv2pat vs)

tapp (Tvar v) t = Tfapp v [t]
tapp (Tfapp v []) t = Tfapp v [t]
tapp t1 t2 = Tapp t1 t2

ttuple [a] = a
ttuple as = Ttuple False as

ptuple [a] = a
ptuple as = Ptuple as

bvtuple [a] = a
bvtuple as = Bvtuple False as


-- Representación de variables de lambda expresiones.
data Boundvar = Bvar Variable -- Variables
              | Bvtuple Bool [Boundvar] -- Bound variable tuples. Uses the boolean value like in Ttuple,
                                        -- but when bounding input variables of hylomorphisms.
    deriving (Eq)

-- Representación de patrones
data Pattern = Pvar Variable  -- Variables
               | Ptuple [Pattern] -- Tuplas de patrones
               | Pcons Constructor [Pattern] -- Aplicación de constructores
               | Plit Literal -- Literales
               | Pas Variable Pattern -- @-Pattern
    deriving (Eq)

pany = Pvar (Vuserdef "_")


-- Representación de functores
-- Fue discutida, pero aún podría sufrir cambios.
newtype Func = Fsum [Fprod]
          deriving (Show,Eq)
type Fprod = (Int,Int)

-- Data (identificador,argumentos) (Lista de constructores con sus argumentos)
-- Representación improvisada para poder parsear definiciones de datatypes.
-- Podría sufrir cambios.
data Data = Data (Constructor,[Variable]) [(Constructor,[ArgumentDeclaration])]
            deriving (Show)

data ArgumentDeclaration = Acons Constructor [Variable]
                         | Avar Variable
            deriving (Show)


-- Variables libres de una expresión, sin repeticiones.

class Vars a where
  vars :: a -> [Variable]

instance Vars Variable where
  vars a = [a]

instance Vars Boundvar where
  vars (Bvar v) = [v]
  vars (Bvtuple _ vs) = vars vs

instance Vars Pattern where
  vars p@(Pvar v) | p/=pany = [v]
                  | otherwise = []
  vars (Ptuple ps) = concat (map vars ps)
  vars (Pcons _ ps) = concat (map vars ps)
  vars (Plit _) = []
  vars (Pas v p) = v:vars p
 -- vars p = error "vars Pattern: not defined" -- ++ (not_Defined_For_Applied_Pattern p)

instance Vars a => Vars [a] where
  vars = concat.map vars

instance (Vars a, Vars b) => Vars (Either a b) where
  vars = either vars vars

instance Vars Term where
  vars (Tvar v) = [v]
  vars (Ttuple _ ps) = foldr union [] (map vars ps)
  vars (Tcapp _ ps) = foldr union [] (map vars ps)
  vars (Tlit _) = []
  vars (Tfapp fn ps) = foldr union [fn] (map vars ps)
  vars (Tapp t1 t2) = union (vars t1) (vars t2)
  vars (Tlamb bv t) = vars t \\ vars bv
  vars (Tlet v t0 t1) = union (vars t0) (vars t1 \\ [v])
  vars (Tpar t) = vars t
  vars (Tif t0 t1 t2) = union (vars t0) (union (vars t1) (vars t2))
  vars (Tcase t ps ts) = foldr union (vars t) (zipWith (\pi ti ->vars ti \\ vars pi) ps ts)
  vars Tbottom = []
  vars (Thyloapp v i ts _ t) = nub (v:(vars ts++vars t))


-- ======================================================================
-- Obtener las variables ligadas de una expresión.
-- ======================================================================


class VarsB a where
  varsB :: a -> [Variable]

instance (VarsB a) => (VarsB [a]) where
 varsB x = concat$ map varsB x

instance VarsB Term where
  varsB (Tvar _) = []
  varsB (Ttuple _ ps) = concat (map varsB ps)
  varsB (Tcapp _ ps) = concat (map varsB ps)
  varsB (Tlit _) = []
  varsB (Tfapp _ ps) = concat (map varsB ps)
  varsB (Tapp t1 t2) = varsB t1 ++ varsB t2
  varsB (Tlamb bv t) = varsB t ++ vars bv
  varsB (Tlet v t0 t1) = v : (varsB t0 ++ varsB t1)
  varsB (Tif t0 t1 t2) = varsB t0 ++ varsB t1 ++ varsB t2
  varsB (Tpar t) = varsB t
  varsB (Tcase t ps ts) = varsB t ++ concat (zipWith (\pi ti ->varsB ti ++ vars pi) ps ts)
  varsB Tbottom = []
  varsB (Thyloapp _ i ts _ t) = varsB ts++varsB t

instance (VarsB a, VarsB b) => VarsB (Either a b) where
  varsB = either varsB varsB


-- =======================================
-- Alpha convertion
-- =======================================

class Substitutable a where
  -- The [Variable] is the list of variables in scope (i.e. which can be replaced).
  substitute :: [Variable] -> [(Variable, Variable)] -> a -> a

instance Substitutable Variable where
  substitute sc lvars v = if elem v sc then maybe v id $ lookup v lvars else v

instance Substitutable Term where
  substitute sc ss t@(Tvar v) = if elem v sc then maybe t Tvar $ lookup v ss else t
  substitute sc ss (Tlamb bv t) = Tlamb (substitute sc ss bv) (substitute (sc++vars bv) ss t)
  substitute sc ss (Tlet v t0 t1) = case lookup v ss of
                                      Just valor -> Tlet valor (substitute (v:sc) ss t0) (substitute (v:sc) ss t1)
                                      Nothing ->  Tlet v (substitute sc ss t0) (substitute sc ss t1)
  substitute sc ss (Tcase t0 ps ts) = Tcase (substitute sc ss t0) (map (substitute sc ss) ps) (zipWith substitute' ps ts)
    where substitute' p t = substitute (sc++vars p) ss t
  substitute sc ss t@(Tlit _) = t
  substitute sc ss (Ttuple b ts) = Ttuple b$ map (substitute sc ss) ts
  substitute sc ss (Tfapp v ts) = case lookup v ss of
             Just valor -> foldl Tapp (Tvar valor) (map (substitute sc ss) ts)
             Nothing -> Tfapp v (map (substitute sc ss) ts)
  substitute sc ss (Tcapp cons ts) = Tcapp cons (map (substitute sc ss) ts)
  substitute sc ss (Tapp t0 t1) = Tapp (substitute sc ss t0) (substitute sc ss t1)
  substitute sc ss (Thyloapp v i ts pos t) = case lookup v ss of
             Just valor -> foldl Tapp (Tvar valor) (map (substitute sc ss) (thyloArgs i ts pos t))
             Nothing -> Thyloapp v i (map (substitute sc ss) ts) pos (substitute sc ss t)
  substitute sc ss t = error$ "substitute: unexpected term"

instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
  substitute sc lvars = either (Left . substitute sc lvars) (Right . substitute sc lvars)

instance (Substitutable a) => (Substitutable [a]) where
 substitute sc lvars x = map (substitute sc lvars) x


instance Substitutable Pattern where
  substitute sc lvars t@(Pvar v) = maybe t Pvar $ lookup v lvars
  substitute sc lvars (Ptuple ps) = Ptuple (map (substitute sc lvars) ps)
  substitute sc lvars (Pcons c ps) = Pcons c (map (substitute sc lvars) ps)
  substitute sc lvars t@(Plit l) = t
  substitute sc lvars (Pas v p) = Pas (maybe v id$ lookup v lvars) (substitute sc lvars p)

instance Substitutable Boundvar where
  substitute sc lvars b@(Bvar v) = maybe b Bvar $ lookup v lvars
  substitute sc lvars (Bvtuple b vs) = Bvtuple b$ substitute sc lvars vs


%%]