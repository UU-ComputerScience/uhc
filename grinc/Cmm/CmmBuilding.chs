% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[8 module Cmm.CmmBuilding import("EHCommon(HsName)", Data.Char, Data.List(nubBy), GrinCode, Cmm.CmmCode) export(module Cmm.CmmCode)
%%]

%%%%%%%%%%%%%%%%%%%%%%
%% helper functions %%
%%%%%%%%%%%%%%%%%%%%%%

%%[8.lifting export(Toplevel(..),CmmToplevelBuilder,Section(..),CmmSectionBuilder,Body(..),CmmBodyBuilder)
-- toplevel
class Toplevel a where
 toplevel :: a -> CmmToplevel

instance Toplevel CmmProcedure where
  toplevel = CmmToplevel_Procedure 

instance Toplevel CmmDeclaration where
  toplevel = CmmToplevel_Declaration

type CmmToplevelBuilder = CmmToplevels -> CmmToplevels

-- section
class Section a where
 section :: a -> CmmSectionElement

instance Section CmmProcedure where
  section = CmmSectionElement_Procedure

instance Section CmmDeclaration where
  section = CmmSectionElement_Declaration

instance Section CmmData where
  section = CmmSectionElement_Data

type CmmSectionBuilder = CmmSection -> CmmSection
-- Body
class Body a where
  body :: a -> CmmBodyElement

instance Body CmmStatement where
  body = CmmBodyElement_Statement

instance Body CmmDeclaration where
  body = CmmBodyElement_Declaration

type CmmBodyBuilder = CmmBody -> CmmBody

%%]

%%[8.types export(bits, bits8, bits16, bits32, bits64, ptrType, valType)
bits :: Int -> CmmType
bits n = CmmBits n

-- common types
bits8  = bits 8
bits16 = bits 16
bits32 = bits 32
bits64 = bits 64

-- type names
ptrType = CmmName "@ptr" -- type for locations
valType = CmmName "@val" -- type for node elements

%%]

%%[8.expressions export(int,char,cmmVar,prim,memRef,fetch,"(<%>)","(<*>)","(</>)","(<+>)","(<->)")
-- simple expressions
int   v = CmmExpression_Int v Nothing
char  v = CmmExpression_Char v Nothing
cmmVar  = CmmExpression_Var
prim    = CmmExpression_Primitive
memRef  = CmmExpression_MemRef

-- mem refs
fetch :: CmmExpression -> Int -> CmmExpression
fetch l i = memRef valType (if i == 0 then l else l <+> int i)

-- basic calculations
infixl 7 <%>, <*>, </>
infixl 6 <+>, <->

a <%> b = prim "mod" [CmmActual_Actual "" a, CmmActual_Actual "" b]
a <*> b = prim "mul" [CmmActual_Actual "" a, CmmActual_Actual "" b]
a </> b = prim "div" [CmmActual_Actual "" a, CmmActual_Actual "" b]
a <+> b = prim "add" [CmmActual_Actual "" a, CmmActual_Actual "" b]
a <-> b = prim "sub" [CmmActual_Actual "" a, CmmActual_Actual "" b]
%%]

Is this so: I might want to lend the monadic notation by defining >> = ~> and return = |~

%%[8.sequences export(lift,"(~>)",build,emptyBuilder)

lift :: (a -> b) -> a -> [b] -> [b]
lift f = (:) . f

infixr 2 ~> 

(~>) a b = a . b 

emptyBuilder = id

build a = a []
%%]

%%[8.statements export(cmmNop,cmmIfThenElse,cmmIfThen,cmmSwitch,cmmAssign,cmmCall,cmmTailcall,cmmReturn,cmmLabel,cmmGoto,ite,it)

-- statements
cmmNop                = lift body $ CmmStatement_Nop
cmmIfThenElse :: CmmExpression -> CmmBody -> CmmBody -> CmmBodyBuilder
cmmIfThenElse i t e   = lift body $ CmmStatement_IfElse i t e
cmmIfThen     i t     = lift body $ CmmStatement_If i t
cmmSwitch     e a     = lift body $ CmmStatement_Switch e a
cmmAssign     l e     = lift body $ CmmStatement_Assign l e
cmmCall       c f a r fl = lift body $ CmmStatement_Call c f a r fl
cmmTailcall   c f a   = lift body $ CmmStatement_TailCall c f a
cmmReturn     c a     = lift body $ CmmStatement_Return c a
cmmLabel      l       = lift body $ CmmStatement_Label l
cmmGoto       l       = lift body $ CmmStatement_GoTo l

ite :: CmmExpression -> CmmBodyBuilder -> CmmBodyBuilder -> CmmBodyBuilder
ite i t e = cmmIfThenElse i (build t) (build e)
it  i t   = cmmIfThen     i (build t)
%%]

%%[8.declarations export(typedefs,imports,exports,constant,varDecl,globalVarDecl,target)
-- declarations

typedefs t n     = lift toplevel $ CmmDeclaration_Typedef t n
imports  n       = lift toplevel $ CmmDeclaration_Import n
exports  n       = lift toplevel $ CmmDeclaration_Export n
constant t n v   = lift toplevel $ CmmDeclaration_Constant t n v

liftedVarDecl  f i k t n = lift f $ CmmDeclaration_Var i k t n
globalVarDecl = liftedVarDecl toplevel
varDecl       = liftedVarDecl body

target   :: Int -> CmmByteOrder -> Int -> Int -> CmmToplevelBuilder
target ms bo ps ws = lift toplevel (CmmDeclaration_Target $ TargetSpec ms bo ps ws)
%%]

%%[8.procedures export(procedure,formal,formal',ptrArg,fltArg,valArg,buildProcedure)
procedure :: String -> CmmName -> CmmFormals -> CmmBodyBuilder -> CmmToplevelBuilder
procedure c n a b = lift toplevel $ CmmProcedure_Procedure c n a (build b)

-- formals are fixed, only apply function will differ
-- note: the 'invariant' keyword of a formal might be used (e.g. False might be True).
formal'     = formal . cmmName'
formal name = ("address", False, ptrType, name)

ptrArg e        = CmmActual_Actual "address" e
fltArg e        = CmmActual_Actual "float" e
valArg e        = CmmActual_Actual "" e

-- builder
buildProcedure :: HsName -> [HsName] -> CmmBodyBuilder -> CmmToplevelBuilder
buildProcedure name args bodyElems
  = procedure "" (cmmName' name) (map formal' args) bodyElems
%%]

%%%%%%%%%%%
%% Names %%
%%%%%%%%%%%

%Naming convention:
% 
% GRIN names are converted to C-- in the following way:
%   - prepend a $ (dollar)
%   - escape a @ (at) with @@ (at at)
%   - escape all but [a-zA-Z] (letter), [0-9] (digit), _ (underscore),
%     $ (dollar), @ (at) in a name with a @ <ascii code> @ 
% Compiler introduced names start with an @

%%[8.names export(cmmName, cmmName', cmmVar', varUpdate')
-- convert GRIN to C-- names
cmmName' :: HsName -> CmmName
cmmName' = cmmName . show

cmmName :: String -> CmmName
cmmName s = '$' : concatMap escapeChar s
    where
    escapeChar c | isAlphaNum c || c `elem` "_$" = [c]
                     | c == '@'                      = ['@','@']
                     | otherwise                     = '@' : (show (ord c) ++ "@")

-- alternative var name funtions based on HsName
cmmVar'    = cmmVar    . cmmName'
varUpdate' = varUpdate . cmmName'
%%]

% NOTE: As a convention the tags of bindings are named after the their binding name with "@F" prepended

%%[8.CAFs export(CAFEnv,arg,var,var',arg',cafNode4name)
type CAFEnv = [(CmmName, Int)]

arg :: CmmName -> CmmActual
arg  nm = ptrArg $ var nm

var :: CmmName -> CmmExpression
var nm = cmmVar nm

var' nm = var (cmmName' nm)
arg' nm = ptrArg $ var' nm

cafNode4name :: CmmName -> CmmName
cafNode4name cn = "@F" ++ cn

cafOffset :: CmmName -> CAFEnv -> Int
cafOffset nm env = maybe (error $ "no CAF found named '" ++ show nm ++ "'") id $ lookup nm env
%%]

All function calls must match the number of recievers with the number of return
values. Note that we request CmmKindedNames to be exactly the number of return
values in the called function.

%%[8.calls export(call,call',apply,eval)
call :: Maybe CmmFlow -> CmmName -> CmmActuals -> CmmKindedNames -> CmmBodyBuilder
call flow name = fcall "" (cmmVar name)
    where
    fcall = case flow of
                  Nothing   -> (\a b c d   -> cmmTailcall a b c)
                  Just fl   -> (\a b c d   -> cmmCall a b c d fl)

call' ctype name = call ctype (cmmName' name)

-- eval and apply calls
apply                = "$apply"
eval                 = "$eval"

-- tailCallEval  = call True   eval [a] []
-- callEval      = call False eval [a]
%%]

%%[8.reservations export(reserve,many,one,noInit,strInit,ustrInit,listInit)
reserve s t i = CmmDatum_Reserve t size i
    where
    size = if s > 0
           then if s == 1
                    then CmmSize_Single
                    else CmmSize_Sized (int s)
               else CmmSize_Many
many = 0 :: Int
one  = 1 :: Int

noInit        = CmmInitString_None
strInit       = CmmInitString_String
ustrInit      = CmmInitString_UnicodeString
listInit      = CmmInitString_List
%%]

%%[8.assignments export(memUpdate,varUpdate,updates)
-- value updates
memUpdate t l i | i == 0    = CmmLValue_MemRef t l
                | otherwise = CmmLValue_MemRef t (l <+> int i)
varUpdate                   = CmmLValue_Var

updates :: [(CmmLValue,CmmExpression)] -> CmmBodyBuilder
updates = uncurry cmmAssign . unzip
%%]

%%%%%%%%%%%%%%%%%%%%%%%
%% Name registration %%
%%%%%%%%%%%%%%%%%%%%%%%

We keep track of name information on lots of places. Note that the register is only used for locally binded names, e.g. no CAF can occur.

For each introduced identifier:
- what kind of identifier is it. (pointers, floats or anyting else)
  - All functions except the apply function take only pointer arguments!
- what type of identifiers is it. (pointer, tag, or a node value)
  - variables are introduced by means of attribute targetNames, other values
    already have a type.
- the name of the identifier (duh)

%%[8.nameRegister export(CmmNameRegisterElement,CmmNameRegister,nreg2knames,nreg2names,nreg2varDef,noDups,tagVar,elemVar)
type CmmNameRegisterElement = (CmmName, (CmmType, CmmKind))
type CmmNameRegister = [CmmNameRegisterElement]

nreg2knames :: CmmNameRegister -> CmmKindedNames
nreg2knames l = map (\(n, (t, k)) -> (k,n)) l
   

nreg2names :: CmmNameRegister -> CmmNames
nreg2names = map fst

nreg2varDef :: CmmNameRegister -> CmmBodyBuilder
nreg2varDef = foldr makeDecls emptyBuilder
    where
    makeDecls nre decls      = nrege2varDef nre ~> decls
    nrege2varDef :: CmmNameRegisterElement -> CmmBodyBuilder
    nrege2varDef (n, (t, k)) | n == "$__" = id
                             | otherwise  = varDecl False k t [(n, Nothing)]

--noDups :: CmmNameRegister -> CmmNameRegister
noDups :: (Eq a) => [(a,b)] -> [(a,b)]
noDups = nubBy (\x y -> fst x == fst y)

tagVar n = (n, (valType, ""))
elemVar n = (n, (valType, ""))
%%]

%% -----------------------------

%%[8
heapPointer = cmmVar "@hp"
heapLimit = cmmVar "@heapLimit"

allocates (n:nl) = let f = uncurry allocate
                   in foldr ((~>) . f) (f n) nl

allocate p n = let newHp    = heapPointer <+> int n
                   allocate = updates [ (varUpdate p, heapPointer)
                                      , (varUpdate "@hp", newHp)
                                      ]
                   ensureSpace   = cmmNop
                   --ensureSpace = it (prim $ "lt" [newHp, heapLimit])
                   --                 (ffCall "grin_gc" [int n] [])
               in ensureSpace ~> allocate

ffCall name args recievers = cmmCall "C" (cmmVar name) args recievers []
%%]

% vim:et:ts=4:ai:
