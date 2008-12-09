{-| Module      :  Parser
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.Parser
    ( module_, exp_, exp0, type_, atype, contextAndType
    , parseOnlyImports
    ) where

{-
Absent:
- records
- classes (class, instance, default...)
- "newtype"
- strictness annotations
- n+k patterns
- [] and (,) and (,,,) etc as (type) constructor
- empty declarations, qualifiers, alternatives or statements
- "qualified", "as" in imports
- import and export lists

Simplified:
- funlhs 
    For example   x:xs +++ ys = ...  is not allowed, parentheses around x:xs necessary
- pattern binding met pat10 i.p.v. pat0 
    For example   (x:xs) = [1..] (parenthesis are obligatory)
- sections: (fexp op) and (op fexp)
    For example   (+2*3)   is not allowed, should be (+(2*3)) 
- fixity declarations only at top-level
-}

import Helium.Parser.ParseLibrary hiding (satisfy)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Helium.Parser.Lexer
import Helium.Parser.LayoutRule
import qualified Helium.Utils.Texts as Texts

import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils
import Helium.Syntax.UHA_Range

import qualified Helium.Parser.CollectFunctionBindings as CollectFunctionBindings
import Helium.Utils.Utils

parseOnlyImports :: String -> IO [String]
parseOnlyImports fullName = do
    contents <- catch (readFile fullName)
        (\ioError -> 
            let message = "Unable to read file " ++ show fullName 
                       ++ " (" ++ show ioError ++ ")"
            in throw message)
    
    return $ case lexer fullName contents of
        Left _ -> []
        Right (tokens, _) ->
            case runHParser onlyImports fullName (layout tokens) False {- no EOF -} of
                Left _ -> []
                Right imports -> 
                    map stringFromImportDeclaration imports

{-
module  
    ->  "module" modid exports? "where" body  
--      |  body  
-}

module_ :: HParser Module
module_ = addRange $
    do
        lexMODULE
        n <- modid
        let mes = MaybeExports_Nothing
        lexWHERE
        b <- body
        return (\r -> Module_Module r (MaybeName_Just n) mes b)
    <|>
    do 
        b <- body
        return (\r ->
            Module_Module r MaybeName_Nothing MaybeExports_Nothing b)

onlyImports :: HParser [ImportDeclaration]
onlyImports = 
    do
        lexMODULE
        modid
        let mes = MaybeExports_Nothing
        lexWHERE
        lexLBRACE <|> lexINSERTED_LBRACE
        many (do { i <- impdecl; semicolon; return i })
    <|>
    do
        lexLBRACE <|> lexINSERTED_LBRACE
        many (do { i <- impdecl; semicolon; return i })
  where
    semicolon = lexSEMI <|> lexINSERTED_SEMI <|> lexINSERTED_RBRACE
    -- the last of the three is a hack to support files that
    -- only contain imports
    
{-
body  ->  "{" topdecls "}"
topdecls  ->  topdecl1 ";" ... ";" topdecln    (n>=0)  
-}

body = addRange $
    withBraces' $ \explicit -> 
      do{ (is, ds) <- importsThenTopdecls explicit
        ; let groupedDecls = CollectFunctionBindings.decls ds
        ; return $ \r -> Body_Body r is groupedDecls
        }

importsThenTopdecls explicit =
    do
        is <- many (do { i <- impdecl
                       ; if explicit then lexSEMI else lexSEMI <|> lexINSERTED_SEMI 
                       ; return i
                       } )
        ds <- topdeclCombinator topdecl
        return (is, ds)
        
  where
    topdeclCombinator = if explicit then semiSepTerm else semiOrInsertedSemiSepTerm

        
    
{-
topdecl  
    ->  impdecl  
     |  "data" simpletype "=" constrs derivings?
     |  "type" simpletype "=" type
     |  infixdecl
     |  decl  

derivings
    -> "deriving" derivings'

derivings'
    -> tycon 
     | "(" ")"
     | "(" tycon ( "," tycon )* ")"

simpletype  
    ->  tycon tyvar1 ... tyvark  (k>=0)  
-}

{-
    | Data                            
        range                    : Range
        context                  : ContextItems
        simpletype               : SimpleType
        constructors             : Constructors
        derivings                : Names
-}

topdecl :: HParser Declaration
topdecl = addRange (
    do
        lexDATA
        st <- simpleType
        lexASG
        cs <- constrs
        ds <- option [] derivings
        return (\r -> Declaration_Data r [] st cs ds)
    <|>
    do
        lexTYPE
        st <- simpleType
        lexASG
        t <- type_
        return $ \r -> Declaration_Type r st t
    <|>
    infixdecl
    ) 
    <|>
    decl
    <?> Texts.parserDeclaration

derivings :: HParser [Name]
derivings = 
    do
        lexDERIVING
        ds <- 
            do
                cls <- tycls
                return [cls]
            <|>
            do
                lexLPAREN           
                clss <- tycls `sepBy` lexCOMMA
                lexRPAREN
                return clss
        return ds            
    
simpleType :: HParser SimpleType
simpleType =
    addRange (
        do
            c  <- tycon
            vs <- many tyvar
            return $ \r -> SimpleType_SimpleType r c vs
    )
    
{-
infixdecl   ->  fixity [digit] ops  (fixity declaration)  
fixity      ->  "infixl" | "infixr" | "infix"  
ops         ->  op1 "," ... "," opn    (n>=1)  
-}

infixdecl :: HParser (Range -> Declaration)
infixdecl = 
    do
        f <- fixity
        p <- fmap fromInteger (option 9 (fmap read lexInt)) :: HParser Int
        if p < 0 || p > 9 then fail Texts.parserSingleDigitPriority
                          else return ()
        os <- ops
        return $ \r -> Declaration_Fixity r f (MaybeInt_Just p) os

ops :: HParser Names
ops = commas1 op

fixity :: HParser Fixity
fixity = addRange $ 
    do
        lexINFIXL 
        return $ \r -> Fixity_Infixl r
    <|>
    do
        lexINFIXR 
        return $ \r -> Fixity_Infixr r
    <|>
    do
        lexINFIX 
        return $ \r -> Fixity_Infix r
    
{-
constrs  ->  constr1 "|" ... "|" constrn  (n>=1)  
-}

constrs :: HParser Constructors
constrs = constr `sepBy1` lexBAR

{-
constr  ->  btype conop btype  (infix conop)  
         |  con atype1 ... atypek  (arity con = k, k>=0)  
-}

constr :: HParser Constructor
constr = addRange $
    do 
        (t1, n) <- try $ do 
            t1 <- annotatedType btype 
            n <- conop
            return (t1, n)
        t2 <- annotatedType btype
        return (\r -> Constructor_Infix r t1 n t2)
    <|>
    do 
        n <- con 
        ts <- many (annotatedType atype)
        return (\r -> Constructor_Constructor r n ts)
        

{-
Simplified import:
impdecl -> "import" modid impspec?
impspec -> "hiding" "(" import "," ... ")"
import  -> var
-}

impdecl :: HParser ImportDeclaration
impdecl = addRange (
    do
        lexIMPORT
        let q = False
        m <- modid
        let a = MaybeName_Nothing
        i <- option MaybeImportSpecification_Nothing $
                do{ is <- impspec
                  ; return (MaybeImportSpecification_Just is)
                  }
        return $ \r -> ImportDeclaration_Import r q m a i
    ) <?> Texts.parserImportDeclaration

impspec :: HParser ImportSpecification
impspec = addRange $
    do  
        h <- do { lexHIDING; return True }
        is <- parens (commas import_)
        return $ \r -> ImportSpecification_Import r h is

import_ :: HParser Import
import_ = addRange $
    do
        n <- var
        return $ \r -> Import_Variable r n
    
{-
decls   ->  "{" decl1 ";" ... ";" decln "}"    (n>=0)  
-}

decls :: HParser Declarations
decls =
    do
        ds <- withLayout1 decl
        return (CollectFunctionBindings.decls ds)

{- 
decl    ->  vars "::" type  (type signature)  
         |  ( funlhs | pat10 ) rhs          
vars    ->  var1 "," ..."," varn    (n>=1)   
funlhs  ->  var apat*
         |  pat10 varop pat10
         |  "(" funlhs ")" apat *

Rewrite to reduce backtracking:

decl    ->  [[ var ]] decl1
         |  [[ pat10 ]] decl2
         |  funlhs rhs
decl1   ->  "," vars "::" type
         |  "::" type
         |  varop pat10 rhs
         |  "@" apat decl2
         |  apat* rhs
decl2   ->  varop pat10 rhs
         |  rhs
funlhs  ->  [[ var ]] funlhs1
         |  [[ pat10 ]] varop pat10
         |  "(" funlhs ")" apat*
funlhs1 ->  varop pat10
         |  apat*
-}

decl :: HParser Declaration
decl = addRange (
    do 
        nr <- try (withRange var)
        decl1 nr
    <|>
    do
        pr <- try (withRange pat10)
        decl2 pr
    <|>
    do
        l <- funlhs
        b <- normalRhs
        return $ \r -> Declaration_FunctionBindings r
            [FunctionBinding_FunctionBinding r l b]
    ) <?> Texts.parserDeclaration

decl1 :: (Name, Range) -> HParser (Range -> Declaration)
decl1 (n, nr) =
    do
        lexCOMMA
        ns <- vars
        lexCOLCOL
        t <- contextAndType
        return $ \r -> Declaration_TypeSignature r (n:ns) t
    <|>
    do
        lexCOLCOL
        t <- contextAndType
        return $ \r -> Declaration_TypeSignature r [n] t
    <|>
    do  
        o <- varop
        (p, pr) <- withRange pat10
        b <- normalRhs
        let lr = mergeRanges nr pr
        return $ \r -> Declaration_FunctionBindings r
            [FunctionBinding_FunctionBinding r 
                (LeftHandSide_Infix lr (Pattern_Variable nr n) o p) b]
    <|>
    do
        lexAT
        (p, pr) <- withRange apat
        let completeRange = mergeRanges nr pr
            asPat = Pattern_As completeRange n p
        decl2 (asPat, completeRange)
    <|>
    do
        (ps, rs) <- fmap unzip (many (withRange apat))
        let lr = if null rs then nr else mergeRanges nr (last rs)
        b <- normalRhs
        return $ \r -> 
            if null rs then
                Declaration_PatternBinding r (Pattern_Variable nr n) b
            else
                Declaration_FunctionBindings r
                    [FunctionBinding_FunctionBinding r 
                        (LeftHandSide_Function lr n ps) b]                

decl2 :: (Pattern, Range) -> HParser (Range -> Declaration)
decl2 (p1, p1r) = 
    do
        o <- varop
        (p2, p2r) <- withRange pat10
        b <- normalRhs
        let lr = mergeRanges p1r p2r
        return $ \r -> Declaration_FunctionBindings r
            [FunctionBinding_FunctionBinding r 
                (LeftHandSide_Infix lr p1 o p2) b]                
    <|>
    do
        b <- normalRhs
        return $ \r -> Declaration_PatternBinding r p1 b        

funlhs :: HParser LeftHandSide
funlhs = addRange $
    do  
        nr <- try (withRange var)
        funlhs1 nr
    <|>
    do
        p1 <- try pat10
        o <- varop
        p2 <- pat10
        return $ \r -> LeftHandSide_Infix r p1 o p2
    <|>
    do
        l <- parens funlhs
        ps <- many apat
        return $ \r -> LeftHandSide_Parenthesized r l ps 

funlhs1 :: (Name, Range) -> HParser (Range -> LeftHandSide)
funlhs1 (n, nr) = 
    do
        o <- varop
        p <- pat10
        return $ \r -> LeftHandSide_Infix r
                        (Pattern_Variable nr n) o p 
    <|>
    do
        ps <- many apat
        return $ \r -> LeftHandSide_Function r n ps

vars :: HParser [Name]
vars = commas1 var

{-
rhs     ->  "=" exp rhs1
         |  gdexp+ rhs1
rhs1    -> ( "where" decls )?        
gdexp   ->  "|" exp0 "=" exp
-}

normalRhs = rhs lexASG
caseRhs   = rhs lexRARROW

-- The string is "->" for a case rhs and "=" for a normal rhs
rhs :: HParser () -> HParser RightHandSide
rhs equals = addRange $
    do
        equals
        e <- exp_ 
        mds <- option MaybeDeclarations_Nothing rhs1
        return $ \r -> RightHandSide_Expression r e mds
    <|>                        
    do
        gs <- many1 (gdexp equals)
        mds <- option MaybeDeclarations_Nothing rhs1
        return $ \r -> RightHandSide_Guarded r gs mds

rhs1 :: HParser MaybeDeclarations
rhs1 =
    do 
        lexWHERE
        ds <- decls
        return (MaybeDeclarations_Just ds)
                
gdexp :: HParser () -> HParser GuardedExpression
gdexp equals = addRange $
    do
        lexBAR
        g <- exp0
        equals
        e <- exp_
        return $ \r -> GuardedExpression_GuardedExpression r g e
        
{-
exp     ->  exp0 "::" type  (expression type signature)  
         |  exp0  
-}

exp_ = addRange (
    do 
        e <- exp0
        option (\_ -> e) $ 
            do 
                lexCOLCOL
                t <- contextAndType
                return $ \r -> Expression_Typed r e t
    )
    <?> Texts.parserExpression        

contextAndType :: HParser Type
contextAndType = addRange $ do
    mc <- option Nothing (try $ do { c <- scontext; lexDARROW; return (Just c) })
    t <- type_
    case mc of 
        Nothing -> return $ \_ -> t
        Just c  -> return $ \r -> Type_Qualified r c t
    
{-
expi  ->  expi+1 [op(n,i) expi+1]  
 |  lexpi  
 |  rexpi  
lexpi  ->  (lexpi | expi+1) op(l,i) expi+1  
lexp6  ->  - exp7  
rexpi  ->  expi+1 op(r,i) (rexpi | expi+1)  

Simplified, post-processing 

exp0 -> ( "-" )? exp10 ( op ( "-" )? exp10 )*

See noRange in ParseCommon for an explanation of the parsing of infix expressions.
-}

exp0 :: HParser Expression
exp0 = addRange (
    do  
        u <- maybeUnaryMinus
        es <- exprChain
        return $ \r -> Expression_List noRange (u ++ es)
    )
    <?> Texts.parserExpression        

exprChain :: HParser [Expression]
exprChain = 
    do
        e <- exp10
        es <- fmap concat $ many $
            do
                o <- operatorAsExpression False
                u <- maybeUnaryMinus
                e <- exp10
                return ([o] ++ u ++ [e])
        return (e:es)

maybeUnaryMinus = 
    option [] (fmap (:[]) unaryMinus)  
    <?> Texts.parserExpression

unaryMinus :: HParser Expression
unaryMinus = 
    do
        (_, r) <- withRange lexMINDOT 
        return (Expression_Variable noRange (setNameRange floatUnaryMinusName r))
    <|>
    do 
        (_, r) <- withRange lexMIN 
        return (Expression_Variable noRange (setNameRange intUnaryMinusName r))

{-       
exp10   ->  "\" apat1 ... apatn "->" exp  (lambda abstraction, n>=1)  
         |  "let" decls "in" exp  (let expression)  
         |  "if" exp "then" exp "else" exp  (conditional)  
         |  "case" exp "of" alts  (case expression)  
         |  "do" stmts (do expression)  
         |  fexp  
-}

exp10 :: HParser Expression
exp10 = addRange (
    do
        lexBSLASH
        ps <- many1 apat
        lexRARROW
        e <- exp_
        return $ \r -> Expression_Lambda r ps e
    <|>
    do
        lexLET
        ds <- decls
        lexIN
        e <- exp_
        return $ \r -> Expression_Let r ds e
    <|>
    do
        lexIF
        e1 <- exp_
        lexTHEN
        e2 <- exp_
        lexELSE
        e3 <- exp_
        return $ \r -> Expression_If r e1 e2 e3
    <|>
    do
        lexCASE
        e <- exp_
        lexOF
        as <- alts
        return $ \r -> Expression_Case r e as
    <|>
    do
        lexDO
        ss <- stmts
        return $ \r -> Expression_Do r ss
    ) 
    <|>
    fexp
    <?> Texts.parserExpression

{-
fexp  -> aexp+
-}

fexp :: HParser Expression    
fexp = addRange $
    do
        (e:es) <- many1 aexp
        if null es then
            return $ \_ -> e
          else
            return $ \r -> Expression_NormalApplication r e es

{-
aexp    ->  var  (variable)  
         |  con
         |  literal  

         |  "[" "]" 
         |  "[" exp1 "," ... "," expk "]"
         |  "[" exp1 ( "," exp2 )? ".." exp3? "]"
         |  "[" exp "|" qual1 "," ... "," qualn "]"

         |  () 
         |  (op fexp) (left section)
         |  (fexp op) (right section)
         |  ( exp )  (parenthesized expression)  
         |  ( exp1 , ... , expk )  (tuple, k>=2)  
         
Last cases parsed as:

    "(" "-" exprChain ( "," exp_ )* ")"
  | "(" op fexp ")"
  | "(" fexp op ")"
  | "(" ( exp_ )<sepBy ","> ")"
-}

operatorAsExpression :: Bool -> HParser Expression
operatorAsExpression storeRange = (do
    (o, r) <- withRange ( fmap Left varsym <|> fmap Right consym 
                      <|> lexBACKQUOTEs (fmap Left varid <|> fmap Right conid))
    let range = if storeRange then r else noRange                      
    return (case o of
        Left  v -> Expression_Variable    range v
        Right c -> Expression_Constructor range c
     )) <?> Texts.parserOperator
                         
aexp :: HParser Expression    
aexp = addRange (
    do 
        lexLPAREN
        ( -- dit haakje is nodig (snap niet waarom). Arjan
            try (do  -- de try vanwege (-) DEZE PARSER MOET OPNIEUW GESCHREVEN WORDEN !!!
                ue <- do
                    u <- unaryMinus
                    es <- exprChain
                    return (Expression_List noRange (u:es))
                es <- many (do { lexCOMMA; exp_ })
                lexRPAREN
                return $ 
                    if null es then
                        \r -> Expression_Parenthesized r ue
                    else 
                        \r -> Expression_Tuple r (ue:es))
            <|>                
            do      -- operator followed by optional expression
                    -- either full section (if there is no expression) or 
                    -- a left section (if there is)
                opExpr <- operatorAsExpression True
                me <- option Nothing (fmap Just fexp)
                lexRPAREN
                return $ \r -> 
                    Expression_InfixApplication r
                        MaybeExpression_Nothing
                        opExpr
                        (case me of 
                            Nothing -> MaybeExpression_Nothing
                            Just e  -> MaybeExpression_Just e) 
            <|>
            try (do -- right section, expression followed by operator
                    -- or a parenthesized expression (if no operator is found)
                e <- fexp
                mo <- option Nothing (fmap Just (operatorAsExpression True))
                lexRPAREN
                return $ \r ->
                    case mo of
                        Nothing -> Expression_Parenthesized r e
                        Just opExpr -> 
                            Expression_InfixApplication r
                                (MaybeExpression_Just e)
                                opExpr
                                MaybeExpression_Nothing
            )
            <|>
            do -- unit "()", expression between parenthesis or a tuple
                es <- commas exp_
                lexRPAREN
                return $ \r -> case es of
                    [] -> Expression_Constructor r (Name_Special r [] "()") -- !!!Name
                    [e] -> Expression_Parenthesized r e
                    _ -> Expression_Tuple r es
         )
    <|>
    do
        n <- varid
        return $ \r -> Expression_Variable r n
    <|>
    do
        n <- conid
        return $ \r -> Expression_Constructor r n
    <|>
    do 
        l <- literal
        return $ \r -> Expression_Literal r l
    <|>
    do
        lexLBRACKET
        aexp1
    ) <?> Texts.parserExpression

{-
Last four cases, rewritten to eliminate backtracking

aexp    -> ...
         | "[" aexp1
aexp1   -> "]" 
         | exp aexp2 "]"
aexp2   -> "|" qual1 "," ... "," qualn
         | ".." exp?
         | "," exp aexp3 
         |               (empty)
aexp3   -> ".." exp?
         | ( "," exp )* 
-}

aexp1 :: HParser (Range -> Expression)
aexp1 =
    do
        lexRBRACKET
        return $ \r -> Expression_Constructor r
                        (Name_Special r [] "[]") -- !!!Name
    <|>
    do
        e1 <- exp_
        e2 <- aexp2 e1
        lexRBRACKET
        return e2
        
aexp2 :: Expression -> HParser (Range -> Expression)    
aexp2 e1 = 
    do
        lexBAR
        qs <- commas1 qual
        return $ \r -> Expression_Comprehension r e1 qs
    <|> 
    do
        lexDOTDOT
        option (\r -> Expression_Enum r e1
                        MaybeExpression_Nothing 
                        MaybeExpression_Nothing) $
            do
                e2 <- exp_
                return $ \r -> Expression_Enum r e1
                                MaybeExpression_Nothing 
                                (MaybeExpression_Just e2) 
    <|>
    do
        lexCOMMA
        e2 <- exp_
        aexp3 e1 e2
    <|>
    return (\r -> Expression_List r [e1])

aexp3 :: Expression -> Expression -> HParser (Range -> Expression)    
aexp3 e1 e2 =
    do
        lexDOTDOT
        option (\r -> Expression_Enum r e1 
                        (MaybeExpression_Just e2)  
                        MaybeExpression_Nothing) $
            do
                e3 <- exp_
                return $ \r -> Expression_Enum r e1 
                                (MaybeExpression_Just e2)
                                (MaybeExpression_Just e3) 
    <|>
    do
        es <- many (do { lexCOMMA; exp_ })
        return $ \r -> Expression_List r (e1:e2:es)              

{-
stmts  -> "{" stmt1 ";" ... ";" stmtn "}"   (n>=0)  
-}

stmts :: HParser Statements
stmts = 
    withLayout stmt

{-
stmt    ->  "let" decls  
         |  pat "<-" exp  
         |  exp
-}

stmt :: HParser Statement
stmt = addRange $
    do
        lexLET
        ds <- decls
        option (\r -> Statement_Let r ds) $ do
            lexIN
            e <- exp_
            return (\r -> Statement_Expression r (Expression_Let r ds e))
    <|>
    do
        p <- try $
            do
                p <- pat
                lexLARROW
                return p
        e <- exp_
        return $ \r -> Statement_Generator r p e
    <|>
    do
        e <- exp_
        return $ \r -> Statement_Expression r e
        
{-
alts    ->  "{" alt1 ";" ... ";" altn "}" (n>=0)  
-}

alts :: HParser Alternatives
alts =
    withLayout alt

{-
alt -> pat rhs
-}

alt :: HParser Alternative
alt = addRange $
    do
        p <- pat
        b <- caseRhs
        return $ \r -> Alternative_Alternative r p b

{-
qual    ->  "let" decls  (local declaration)  
         |  pat "<-" exp  (generator)  
         |  exp  (guard)  
-}

qual :: HParser Qualifier
qual = addRange $
    do
        lexLET
        ds <- decls
        option (\r -> Qualifier_Let r ds) $ do
            lexIN
            e <- exp_
            return (\r -> Qualifier_Guard r (Expression_Let r ds e))
    <|>
    do
        p <- try $
            do
                p <- pat
                lexLARROW
                return p
        e <- exp_
        return $ \r -> Qualifier_Generator r p e
    <|>
    do
        e <- exp_
        return $ \r -> Qualifier_Guard r e

{-
pat  ->  pat0  
pati  ->  pati+1 [conop(n,i) pati+1]  
 |  lpati  
 |  rpati  
lpati  ->  (lpati | pati+1) conop(l,i) pati+1  
lpat6  ->  - (integer | float)  (negative literal)  
rpati  ->  pati+1 conop(r,i) (rpati | pati+1)  

See noRange in ParseCommon for an explanation of the parsing of infix expressions.

-}

pat :: HParser Pattern
pat = addRange $
    do  
        u <- unaryMinusPat
        ps <- fmap concat $ many $
            do
                o <- do { n <- conop; return (Pattern_Variable noRange n) }
                u <- unaryMinusPat
                return (o : u)
        return $ \r -> Pattern_List noRange (u ++ ps)
        
unaryMinusPat :: HParser [Pattern]
unaryMinusPat = 
    do 
        (n, mr) <- withRange (do { lexMINDOT; return floatUnaryMinusName } <|> 
                              do { lexMIN;    return intUnaryMinusName   } )
        (l, lr) <- withRange numericLiteral
        return 
            [ Pattern_Variable noRange (setNameRange n mr)
            , Pattern_Literal lr l
            ]
    <|>
    do
        p <- pat10
        return [p]
    
{-
pat10   ->  con apat*
         |  apat  
-}

pat10 :: HParser Pattern
pat10 = addRange (
    do  
        n  <- try con    
        ps <- many apat
        return $ \r -> Pattern_Constructor r n ps
    )
    <|>
    apat
    <?> Texts.parserPattern
       
{-
apat    ->  var ( "@" apat )?

         |  "(" ")"
         |  "(" pat ")"  (parenthesized pattern)  
         |  "(" pat1 "," ... "," patk ")"  (tuple pattern, k>=2)  

         |  "[" "]" 
         |  "[" pat1 "," ... "," patk "]"  (list pattern, k>=1)  

         |  "_"  (wildcard)  
         |  con  (arity con = 0)  
         |  literal  
         |  "~" apat    (irrefutable pattern)
-}

apat :: HParser Pattern
apat = addRange (
    do
        v <- try var -- because of parentheses
        option (\r -> Pattern_Variable r v) $ do
            lexAT
            p <- apat
            return $ \r -> Pattern_As r v p
    <|>
    do
        ps <- parens (commas pat)
        return $ \r -> case ps of
            [] -> Pattern_Constructor r (Name_Special r [] "()") [] -- !!!Name
            [p] -> Pattern_Parenthesized r p
            _ -> Pattern_Tuple r ps
    <|>
    do
        ps <- brackets (commas pat)
        return $ \r -> case ps of
            [] -> Pattern_Constructor r (Name_Special r [] "[]") [] -- !!!Name
            _ -> Pattern_List r ps
    <|> 
    do
        lexUNDERSCORE
        return $ \r -> Pattern_Wildcard r
    <|>
    do
        n <- con
        return $ \r -> Pattern_Constructor r n []
    <|>
    do
        l <- literal
        return $ \r -> Pattern_Literal r l
    <|>
    do  
        lexTILDE
        p <- apat
        return $ \r -> Pattern_Irrefutable r p
    ) <?> Texts.parserPattern

{-
scontext -> class | "(" class1 "," ... "," classn ")"    (n>=0)
simpleclass -> tycls tyvar
         (other case in Haskell report at 'class' is not supported in Helium
          because we do not have type variable application)
-}

scontext :: HParser ContextItems
scontext = 
    do { c <- simpleclass; return [c] }
    <|>
    parens (commas simpleclass)

simpleclass :: HParser ContextItem
simpleclass = addRange (do
    c <- tycon
    (v, vr) <- withRange tyvar
    return $ \r -> ContextItem_ContextItem r c [Type_Variable vr v]
    )
    
{-
type  ->  btype ( "->" type )? 
-}

type_ :: HParser Type
type_ = addRange (
    do 
        left <- btype
        option (\_ -> left) $
            do
                (_, rangeArrow) <- withRange (lexRARROW)
                right <- type_
                return (\r -> Type_Application r False
                        (Type_Constructor rangeArrow (Name_Special rangeArrow [] "->")) [left, right]) -- !!!Name
    ) <?> Texts.parserType

{-
btype  ->  atype+
-}

btype :: HParser Type
btype = addRange (
    do
        ts <- many1 atype
        return $ \r -> case ts of
            [t] -> t
            (t:ts) -> Type_Application r True t ts
    ) <?> Texts.parserType

{-
atype   ->  tycon
         |  tyvar  
         |  "(" ")"  (unit type)  
         |  "(" type1 "," ... "," typek ")"  (tuple type, k>=2)  
         |  "(" type ")"  (parenthesized constructor)  
         |  "[" type "]"  (list type)  
-}

atype :: HParser Type
atype = addRange (
    do
        c <- tycon
        return (\r -> Type_Constructor r c)
    <|>
    do
        c <- tyvar
        return (\r -> Type_Variable r c)
    <|>
    do
        ts <- parens (commas type_)
        return (\r -> case ts of
            [] -> Type_Constructor r (Name_Special r [] "()") -- !!!Name
            [t] -> Type_Parenthesized r t
            _ -> let n = Name_Special r []  -- !!!Name
                            ( "(" ++ replicate (length ts - 1) ',' ++ ")" )
                 in Type_Application r False (Type_Constructor r n) ts
         )
    <|>
    do
        t <- brackets type_
        return $ \r ->
            let n = Name_Special r [] "[]" -- !!!Name
            in Type_Application r False (Type_Constructor r n) [t]
    ) <?> Texts.parserType

annotatedType :: HParser Type -> HParser AnnotatedType
annotatedType p = addRange $
    do
        t <- p
        return (\r -> AnnotatedType_AnnotatedType r False t)

literal = addRange (
    do
        i <- lexInt
        return $ \r -> Literal_Int r i
    <|>
    do
        d <- lexDouble
        return $ \r -> Literal_Float r d
    <|>
    do
        c <- lexChar
        return $ \r -> Literal_Char r c
    <|>
    do
        s <- lexString
        return $ \r -> Literal_String r s
    ) <?> Texts.parserLiteral

numericLiteral = addRange (
    do
        i <- lexInt
        return $ \r -> Literal_Int r i
    <|>
    do
        d <- lexDouble
        return $ \r -> Literal_Float r d
    ) <?> Texts.parserNumericLiteral
