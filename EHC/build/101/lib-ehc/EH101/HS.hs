

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/HS.ag)
module EH101.HS(module UU.Scanner.Position
, AGItf (..), Module (..), Body (..), Declaration (..), Declarations, MaybeDeclarations, Type (..), Types, MaybeType, Expression (..), Expressions, MaybeExpression, FunctionBinding (..), FunctionBindings, LeftHandSide (..), RightHandSide (..), Pattern (..), Patterns, Literal (..), Name, Names, MaybeName, MaybeNames, Strings, Range (..), Fixity (..)
, TypeAnnotation (..), ExpressionAnnotation (..), PatternAnnotation (..)
, mkRngNm, mkRngNm', mkRngStr
, TypeLeftHandSide (..), TypePattern (..), TypePatterns
, Constructor (..), Constructors
, GuardedExpression (..), GuardedExpressions, Alternative (..), Alternatives, Qualifier (..), Qualifiers
, Kind (..), Kinds, KindAnnotation (..)
, FieldDeclaration (..), FieldDeclarations
, RecordPatternBinding (..), RecordPatternBindings
, RecordExpressionBinding (..), RecordExpressionBindings
, RowRecordExpressionUpdate (..), RowRecordExpressionUpdates, RowRecordPatternBinding (..), RowRecordPatternBindings, RowTypeUpdate (..), RowTypeUpdates
, module EH101.Base.Target
, ContextItem (..), ContextItems
, Statement (..), Statements
, ContextedExpression (..), ContextedExpressions, ContextedPattern (..), ContextedPatterns
, FunctionalDependency (..), FunctionalDependencies
, Export (..), Exports, MaybeExports, ImportDeclaration (..), ImportDeclarations, ImportSpecification (..), MaybeImportSpecification, Import (..), Imports
, Deriving (..), Derivings
, Pragma (..), Pragmas) where

import EH101.Base.Common
import UU.Scanner.Position
import EH.Util.ScanUtils
import EH101.Scanner.Token
import EH101.Base.Target (FFIWay)













































type Name = HsName
type Names = [Name]
type MaybeName = Maybe Name
type MaybeNames = Maybe Names





mkRngNm' :: (Position t,HSNM t) => (Range -> HsName -> r) -> t -> (r,Range)
mkRngNm' ast t
  = (ast r (mkHNm t),r)
  where r = mkRange1 t

mkRngNm :: (Position t,HSNM t) => (Range -> HsName -> r) -> t -> r
mkRngNm ast t = fst (mkRngNm' ast t)

mkRngStr :: (Range -> String -> r) -> Token -> r
mkRngStr ast t = ast (mkRange1 t) (tokMkStr t)





instance SemApp Type where
  semApp        = \e1 e2 -> Type_NormalApplication emptyRange e1 [e2]
  semAppTop     = id
  semCon        = mkRngNm Type_Constructor
  semParens     = Type_Parenthesized emptyRange
  mkApp (a:as)  = if null as then a else Type_NormalApplication emptyRange a as
  semRngCon r n = Type_Constructor r (mkHNm n)


  semRngVar r n = Type_Variable r (mkHNm n)



instance SemApp Expression where
  semApp            = \e1 e2 -> Expression_NormalApplication emptyRange e1 [e2]
  semAppTop         = id
  semCon            = mkRngNm Expression_Constructor
  semParens         = Expression_Parenthesized emptyRange
  mkApp (a:as)      = if null as then a else Expression_NormalApplication emptyRange a as
  semRngCon r n     = Expression_Constructor r (mkHNm n)
  semRngVar r n     = Expression_Variable r (mkHNm n)

-- AGItf -------------------------------------------------------
data AGItf  = AGItf_AGItf !(Module ) 
-- Alternative -------------------------------------------------
data Alternative  = Alternative_Alternative !(Range) !(Pattern ) !(RightHandSide ) 
                  | Alternative_Empty !(Range) 
-- Alternatives ------------------------------------------------
type Alternatives  = [Alternative ]
-- Body --------------------------------------------------------
data Body  = Body_Body !(Range) !(ImportDeclarations ) !(Declarations ) 
-- Constructor -------------------------------------------------
data Constructor  = Constructor_Constructor !(Range) !(Name) !(Types ) 
                  | Constructor_Contexted !(Range) !(ContextItems ) !(Constructor ) 
                  | Constructor_GADTFunction !(Range) !(Name) !(Type ) 
                  | Constructor_Infix !(Range) !(Type ) !(Name) !(Type ) 
                  | Constructor_Record !(Range) !(Name) !(FieldDeclarations ) 
-- Constructors ------------------------------------------------
type Constructors  = [Constructor ]
-- ContextItem -------------------------------------------------
data ContextItem  = ContextItem_Arrow !(Range) !(ContextItem ) !(ContextItem ) 
                  | ContextItem_Class !(Range) !(Name) !(Types ) 
                  | ContextItem_Equal !(Range) !(Type ) !(Type ) 
                  | ContextItem_Forall !(Range) !(Names) !(ContextItem ) 
                  | ContextItem_Implicits !(Range) 
                  | ContextItem_NoImplicits !(Range) 
                  | ContextItem_RowLacksLabel !(Range) !(Name) !(Name) 
-- ContextItems ------------------------------------------------
type ContextItems  = [ContextItem ]
-- ContextedExpression -----------------------------------------
data ContextedExpression  = ContextedExpression_Contexted !(Range) !(Expression ) !(ContextItem ) 
-- ContextedExpressions ----------------------------------------
type ContextedExpressions  = [ContextedExpression ]
-- ContextedPattern --------------------------------------------
data ContextedPattern  = ContextedPattern_Contexted !(Range) !(Pattern ) !(ContextItem ) 
-- ContextedPatterns -------------------------------------------
type ContextedPatterns  = [ContextedPattern ]
-- Declaration -------------------------------------------------
data Declaration  = Declaration_Class !(Range) !(ContextItems ) !(TypeLeftHandSide ) !(FunctionalDependencies ) !(MaybeDeclarations ) 
                  | Declaration_Data !(Range) !(ContextItems ) !(TypeLeftHandSide ) !(Constructors ) !(Derivings ) 
                  | Declaration_Default !(Range) !(MaybeName) !(Types ) 
                  | Declaration_Empty !(Range) 
                  | Declaration_Fixity !(Range) !(Fixity) !((Maybe Int)) !(Names) 
                  | Declaration_ForeignExport !(Range) !(FFIWay) !((Maybe String)) !(Name) !(Type ) 
                  | Declaration_ForeignImport !(Range) !(FFIWay) !((Maybe String)) !((Maybe String)) !(Name) !(Type ) 
                  | Declaration_FunctionBindings !(Range) !(FunctionBindings ) 
                  | Declaration_FusionConversion !(Range) !(Name) !(Name) 
                  | Declaration_FusionDeclaration !(Range) !(Name) 
                  | Declaration_GADT !(Range) !(ContextItems ) !(TypeLeftHandSide ) !(Constructors ) !(Derivings ) 
                  | Declaration_Instance !(Range) !(InstVariant) !(MaybeName) !(Bool) !(ContextItems ) !(Type ) !(MaybeDeclarations ) 
                  | Declaration_InstanceUseImplicitly !(Range) !(Expression ) !(Name) !(Types ) 
                  | Declaration_KindSignature !(Range) !(Names) !(Kind ) 
                  | Declaration_Module !(Range) !(MaybeName) !(MaybeExports ) 
                  | Declaration_Newtype !(Range) !(ContextItems ) !(TypeLeftHandSide ) !(Constructor ) !(Derivings ) 
                  | Declaration_PatternBinding !(Range) !(Pattern ) !(RightHandSide ) 
                  | Declaration_Pragma !(Range) !(Pragma ) 
                  | Declaration_Type !(Range) !(TypeLeftHandSide ) !(Type ) 
                  | Declaration_TypeSignature !(Range) !(Names) !(Type ) 
-- Declarations ------------------------------------------------
type Declarations  = [Declaration ]
-- Deriving ----------------------------------------------------
data Deriving  = Deriving_Deriving !(Range) !(MaybeName) !(Bool) !(Name) 
-- Derivings ---------------------------------------------------
type Derivings  = [Deriving ]
-- Export ------------------------------------------------------
data Export  = Export_Module !(Range) !(Name) 
             | Export_TypeOrClass !(Range) !(Name) !(MaybeNames) 
             | Export_TypeOrClassComplete !(Range) !(Name) 
             | Export_Variable !(Range) !(Name) 
-- Exports -----------------------------------------------------
type Exports  = [Export ]
-- Expression --------------------------------------------------
data Expression  = Expression_Annotate !(Range) !(ExpressionAnnotation ) !(Expression ) 
                 | Expression_Case !(Range) !(Expression ) !(Alternatives ) 
                 | Expression_Comprehension !(Range) !(Expression ) !(Qualifiers ) 
                 | Expression_Constructor !(Range) !(Name) 
                 | Expression_Do !(Range) !(Statements ) 
                 | Expression_Enum !(Range) !(Expression ) !(MaybeExpression ) !(MaybeExpression ) 
                 | Expression_If !(Range) !(Expression ) !(Expression ) !(Expression ) 
                 | Expression_ImplicitApplication !(Range) !(Expression ) !(ContextedExpressions ) 
                 | Expression_ImplicitLambda !(Range) !(ContextedPatterns ) !(Expression ) 
                 | Expression_ImpredicativeApplication !(Range) !(Expression ) !(Expressions ) 
                 | Expression_InfixApplication !(Range) !(Expression ) !(Expression ) !(Expression ) 
                 | Expression_InfixApplicationChainTop !(Range) !(Expression ) 
                 | Expression_Lambda !(Range) !(Patterns ) !(Expression ) 
                 | Expression_Let !(Range) !(Bool) !(Declarations ) !(Expression ) 
                 | Expression_List !(Range) !(Expressions ) 
                 | Expression_Literal !(Range) !(Literal ) 
                 | Expression_Negate !(Range) !(Expression ) 
                 | Expression_NormalApplication !(Range) !(Expression ) !(Expressions ) 
                 | Expression_Parenthesized !(Range) !(Expression ) 
                 | Expression_RecordConstruction !(Range) !(Name) !(RecordExpressionBindings ) 
                 | Expression_RecordUpdate !(Range) !(Expression ) !(RecordExpressionBindings ) 
                 | Expression_RowRecordEmpty !(Range) 
                 | Expression_RowRecordSelect !(Range) !(Expression ) !(Name) 
                 | Expression_RowRecordUpdate !(Range) !(Expression ) !(RowRecordExpressionUpdates ) 
                 | Expression_SectionApplication !(Range) !(MaybeExpression ) !(Expression ) !(MaybeExpression ) 
                 | Expression_Tuple !(Range) !(Expressions ) 
                 | Expression_TupleConstructor !(Range) !(Int) 
                 | Expression_Typed !(Range) !(Expression ) !(Type ) 
                 | Expression_Variable !(Range) !(Name) 
-- ExpressionAnnotation ----------------------------------------
data ExpressionAnnotation  = ExpressionAnnotation_Empty 
-- Expressions -------------------------------------------------
type Expressions  = [Expression ]
-- FieldDeclaration --------------------------------------------
data FieldDeclaration  = FieldDeclaration_FieldDeclaration !(Range) !(Names) !(Type ) 
-- FieldDeclarations -------------------------------------------
type FieldDeclarations  = [FieldDeclaration ]
-- FunctionBinding ---------------------------------------------
data FunctionBinding  = FunctionBinding_FunctionBinding !(Range) !(LeftHandSide ) !(RightHandSide ) 
-- FunctionBindings --------------------------------------------
type FunctionBindings  = [FunctionBinding ]
-- FunctionalDependencies --------------------------------------
type FunctionalDependencies  = [FunctionalDependency ]
-- FunctionalDependency ----------------------------------------
data FunctionalDependency  = FunctionalDependency_Dependency !(Range) !(Names) !(Names) 
-- GuardedExpression -------------------------------------------
data GuardedExpression  = GuardedExpression_GuardedExpression !(Range) !(Expression ) !(Expression ) 
-- GuardedExpressions ------------------------------------------
type GuardedExpressions  = [GuardedExpression ]
-- Import ------------------------------------------------------
data Import  = Import_TypeOrClass !(Range) !(Name) !(MaybeNames) 
             | Import_TypeOrClassComplete !(Range) !(Name) 
             | Import_Variable !(Range) !(Name) 
-- ImportDeclaration -------------------------------------------
data ImportDeclaration  = ImportDeclaration_Empty !(Range) 
                        | ImportDeclaration_Import !(Range) !(Bool) !(Name) !(MaybeName) !(MaybeImportSpecification ) 
-- ImportDeclarations ------------------------------------------
type ImportDeclarations  = [ImportDeclaration ]
-- ImportSpecification -----------------------------------------
data ImportSpecification  = ImportSpecification_Import !(Range) !(Bool) !(Imports ) 
-- Imports -----------------------------------------------------
type Imports  = [Import ]
-- Kind --------------------------------------------------------
data Kind  = Kind_Annotate !(Range) !(KindAnnotation ) !(Kind ) 
           | Kind_Constructor !(Range) !(Name) 
           | Kind_Forall !(Range) !(Names) !(Kind ) 
           | Kind_InfixApplication !(Range) !(Kind ) !(Kind ) !(Kind ) 
           | Kind_NormalApplication !(Range) !(Kind ) !(Kinds ) 
           | Kind_Parenthesized !(Range) !(Kind ) 
           | Kind_Variable !(Range) !(Name) 
-- KindAnnotation ----------------------------------------------
data KindAnnotation  = KindAnnotation_Empty 
-- Kinds -------------------------------------------------------
type Kinds  = [Kind ]
-- LeftHandSide ------------------------------------------------
data LeftHandSide  = LeftHandSide_Function !(Range) !(Name) !(Patterns ) 
                   | LeftHandSide_Infix !(Range) !(Pattern ) !(Name) !(Pattern ) 
                   | LeftHandSide_Parenthesized !(Range) !(LeftHandSide ) !(Patterns ) 
                   | LeftHandSide_Typed !(Range) !(LeftHandSide ) !(Type ) 
-- Literal -----------------------------------------------------
data Literal  = Literal_Char !(Range) !(String) 
              | Literal_Float !(Range) !(String) 
              | Literal_Int !(Range) !(Int) !(String) 
              | Literal_String !(Range) !(String) 
-- MaybeDeclarations -------------------------------------------
type MaybeDeclarations  = Maybe Declarations 
-- MaybeExports ------------------------------------------------
type MaybeExports  = Maybe Exports 
-- MaybeExpression ---------------------------------------------
type MaybeExpression  = Maybe Expression 
-- MaybeImportSpecification ------------------------------------
type MaybeImportSpecification  = Maybe ImportSpecification 
-- MaybeType ---------------------------------------------------
type MaybeType  = Maybe Type 
-- Module ------------------------------------------------------
data Module  = Module_Module !(Range) !(MaybeName) !(Pragmas ) !(MaybeExports ) !(Body ) 
-- Pattern -----------------------------------------------------
data Pattern  = Pattern_Annotate !(Range) !(PatternAnnotation ) !(Pattern ) 
              | Pattern_As !(Range) !(Name) !(Pattern ) 
              | Pattern_Bang !(Range) !(Pattern ) 
              | Pattern_Constructor !(Range) !(Name) !(Patterns ) 
              | Pattern_InfixConstructor !(Range) !(Pattern ) !(Name) !(Pattern ) 
              | Pattern_Irrefutable !(Range) !(Pattern ) 
              | Pattern_List !(Range) !(Patterns ) 
              | Pattern_Literal !(Range) !(Int) !(Literal ) 
              | Pattern_Parenthesized !(Range) !(Pattern ) 
              | Pattern_Record !(Range) !(Name) !(RecordPatternBindings ) 
              | Pattern_RowRecordBinding !(Range) !(Pattern ) !(RowRecordPatternBindings ) 
              | Pattern_RowRecordEmpty !(Range) 
              | Pattern_Tuple !(Range) !(Int) !(Patterns ) 
              | Pattern_Typed !(Range) !(Pattern ) !(Type ) 
              | Pattern_Variable !(Range) !(Name) 
              | Pattern_Wildcard !(Range) 
-- PatternAnnotation -------------------------------------------
data PatternAnnotation  = PatternAnnotation_Empty 
-- Patterns ----------------------------------------------------
type Patterns  = [Pattern ]
-- Pragma ------------------------------------------------------
data Pragma  = Pragma_Derivable !(Range) !(Name) !(Name) !(Name) 
             | Pragma_ExcludeIfTarget !(Range) !(([String])) 
             | Pragma_Language !(Range) !(Names) 
             | Pragma_OptionsGHC !(Range) !(Names) 
-- Pragmas -----------------------------------------------------
type Pragmas  = [Pragma ]
-- Qualifier ---------------------------------------------------
data Qualifier  = Qualifier_Empty !(Range) 
                | Qualifier_Generator !(Range) !(Pattern ) !(Expression ) 
                | Qualifier_Guard !(Range) !(Expression ) 
                | Qualifier_Let !(Range) !(Declarations ) 
-- Qualifiers --------------------------------------------------
type Qualifiers  = [Qualifier ]
-- RecordExpressionBinding -------------------------------------
data RecordExpressionBinding  = RecordExpressionBinding_Binding !(Range) !(Name) !(Expression ) 
-- RecordExpressionBindings ------------------------------------
type RecordExpressionBindings  = [RecordExpressionBinding ]
-- RecordPatternBinding ----------------------------------------
data RecordPatternBinding  = RecordPatternBinding_Binding !(Range) !(Name) !(Pattern ) 
                           | RecordPatternBinding_Pun !(Range) !(Name) 
-- RecordPatternBindings ---------------------------------------
type RecordPatternBindings  = [RecordPatternBinding ]
-- RightHandSide -----------------------------------------------
data RightHandSide  = RightHandSide_Expression !(Range) !(Expression ) !(MaybeDeclarations ) 
                    | RightHandSide_Guarded !(Range) !(GuardedExpressions ) !(MaybeDeclarations ) 
-- RowRecordExpressionUpdate -----------------------------------
data RowRecordExpressionUpdate  = RowRecordExpressionUpdate_Extends !(Range) !(MaybeName) !(Expression ) 
                                | RowRecordExpressionUpdate_Update !(Range) !(Name) !(Expression ) 
-- RowRecordExpressionUpdates ----------------------------------
type RowRecordExpressionUpdates  = [RowRecordExpressionUpdate ]
-- RowRecordPatternBinding -------------------------------------
data RowRecordPatternBinding  = RowRecordPatternBinding_Binding !(Range) !(MaybeName) !(Pattern ) 
-- RowRecordPatternBindings ------------------------------------
type RowRecordPatternBindings  = [RowRecordPatternBinding ]
-- RowTypeUpdate -----------------------------------------------
data RowTypeUpdate  = RowTypeUpdate_Extends !(Range) !(MaybeName) !(Type ) 
-- RowTypeUpdates ----------------------------------------------
type RowTypeUpdates  = [RowTypeUpdate ]
-- Statement ---------------------------------------------------
data Statement  = Statement_Empty !(Range) 
                | Statement_Expression !(Range) !(Expression ) 
                | Statement_Generator !(Range) !(Pattern ) !(Expression ) 
                | Statement_Let !(Range) !(Declarations ) 
-- Statements --------------------------------------------------
type Statements  = [Statement ]
-- Strings -----------------------------------------------------
type Strings  = [(String)]
-- Type --------------------------------------------------------
data Type  = Type_Annotate !(Range) !(TypeAnnotation ) !(Type ) 
           | Type_Constructor !(Range) !(Name) 
           | Type_Exists !(Range) !(Names) !(Type ) 
           | Type_Forall !(Range) !(Names) !(Type ) 
           | Type_InfixApplication !(Range) !(Type ) !(Type ) !(Type ) 
           | Type_InfixApplicationChainTop !(Range) !(Type ) 
           | Type_MonoWildcard !(Range) 
           | Type_NamedWildcard !(Range) !(Name) 
           | Type_NormalApplication !(Range) !(Type ) !(Types ) 
           | Type_Parenthesized !(Range) !(Type ) 
           | Type_Qualified !(Range) !(ContextItems ) !(Type ) 
           | Type_RowEmpty !(Range) 
           | Type_RowRecEmpty !(Range) 
           | Type_RowRecUpdate !(Range) !(Type ) !(RowTypeUpdates ) 
           | Type_RowSumEmpty !(Range) 
           | Type_RowSumUpdate !(Range) !(Type ) !(RowTypeUpdates ) 
           | Type_RowUpdate !(Range) !(Type ) !(RowTypeUpdates ) 
           | Type_SectionApplication !(Range) !(MaybeType ) !(Type ) !(MaybeType ) 
           | Type_TupleConstructor !(Range) !(Int) 
           | Type_Variable !(Range) !(Name) 
           | Type_Wildcard !(Range) 
-- TypeAnnotation ----------------------------------------------
data TypeAnnotation  = TypeAnnotation_AnnotationName !(Name) 
                     | TypeAnnotation_AnnotationVar !(Name) !(Name) 
                     | TypeAnnotation_Strict 
-- TypeLeftHandSide --------------------------------------------
data TypeLeftHandSide  = TypeLeftHandSide_Function !(Range) !(Name) !(TypePatterns ) 
                       | TypeLeftHandSide_Infix !(Range) !(TypePattern ) !(Name) !(TypePattern ) 
                       | TypeLeftHandSide_Parenthesized !(Range) !(TypeLeftHandSide ) !(TypePatterns ) 
-- TypePattern -------------------------------------------------
data TypePattern  = TypePattern_Variable !(Range) !(Name) 
-- TypePatterns ------------------------------------------------
type TypePatterns  = [TypePattern ]
-- Types -------------------------------------------------------
type Types  = [Type ]