-- UUAGC 0.9.3 (src/helium/syntax/UHA_Syntax.ag)
-- Alternative -------------------------------------------------
data Alternative = Alternative_Alternative (Range) (Pattern) (RightHandSide)
                 | Alternative_Empty (Range)
-- Alternatives ------------------------------------------------
type Alternatives = [Alternative]
-- AnnotatedType -----------------------------------------------
data AnnotatedType = AnnotatedType_AnnotatedType (Range) (Bool) (Type)
-- AnnotatedTypes ----------------------------------------------
type AnnotatedTypes = [AnnotatedType]
-- Body --------------------------------------------------------
data Body = Body_Body (Range) (ImportDeclarations) (Declarations)
-- Constructor -------------------------------------------------
data Constructor = Constructor_Constructor (Range) (Name) (AnnotatedTypes)
                 | Constructor_Infix (Range) (AnnotatedType) (Name) (AnnotatedType)
                 | Constructor_Record (Range) (Name) (FieldDeclarations)
-- Constructors ------------------------------------------------
type Constructors = [Constructor]
-- ContextItem -------------------------------------------------
data ContextItem = ContextItem_ContextItem (Range) (Name) (Types)
-- ContextItems ------------------------------------------------
type ContextItems = [ContextItem]
-- Declaration -------------------------------------------------
data Declaration = Declaration_Class (Range) (ContextItems) (SimpleType) (MaybeDeclarations)
                 | Declaration_Data (Range) (ContextItems) (SimpleType) (Constructors) (Names)
                 | Declaration_Default (Range) (Types)
                 | Declaration_Empty (Range)
                 | Declaration_Fixity (Range) (Fixity) (MaybeInt) (Names)
                 | Declaration_FunctionBindings (Range) (FunctionBindings)
                 | Declaration_Instance (Range) (ContextItems) (Name) (Types) (MaybeDeclarations)
                 | Declaration_Newtype (Range) (ContextItems) (SimpleType) (Constructor) (Names)
                 | Declaration_PatternBinding (Range) (Pattern) (RightHandSide)
                 | Declaration_Type (Range) (SimpleType) (Type)
                 | Declaration_TypeSignature (Range) (Names) (Type)
-- Declarations ------------------------------------------------
type Declarations = [Declaration]
-- Export ------------------------------------------------------
data Export = Export_Module (Range) (Name)
            | Export_TypeOrClass (Range) (Name) (MaybeNames)
            | Export_TypeOrClassComplete (Range) (Name)
            | Export_Variable (Range) (Name)
-- Exports -----------------------------------------------------
type Exports = [Export]
-- Expression --------------------------------------------------
data Expression = Expression_Case (Range) (Expression) (Alternatives)
                | Expression_Comprehension (Range) (Expression) (Qualifiers)
                | Expression_Constructor (Range) (Name)
                | Expression_Do (Range) (Statements)
                | Expression_Enum (Range) (Expression) (MaybeExpression) (MaybeExpression)
                | Expression_If (Range) (Expression) (Expression) (Expression)
                | Expression_InfixApplication (Range) (MaybeExpression) (Expression) (MaybeExpression)
                | Expression_Lambda (Range) (Patterns) (Expression)
                | Expression_Let (Range) (Declarations) (Expression)
                | Expression_List (Range) (Expressions)
                | Expression_Literal (Range) (Literal)
                | Expression_Negate (Range) (Expression)
                | Expression_NegateFloat (Range) (Expression)
                | Expression_NormalApplication (Range) (Expression) (Expressions)
                | Expression_Parenthesized (Range) (Expression)
                | Expression_RecordConstruction (Range) (Name) (RecordExpressionBindings)
                | Expression_RecordUpdate (Range) (Expression) (RecordExpressionBindings)
                | Expression_Tuple (Range) (Expressions)
                | Expression_Typed (Range) (Expression) (Type)
                | Expression_Variable (Range) (Name)
-- Expressions -------------------------------------------------
type Expressions = [Expression]
-- FieldDeclaration --------------------------------------------
data FieldDeclaration = FieldDeclaration_FieldDeclaration (Range) (Names) (AnnotatedType)
-- FieldDeclarations -------------------------------------------
type FieldDeclarations = [FieldDeclaration]
-- Fixity ------------------------------------------------------
data Fixity = Fixity_Infix (Range)
            | Fixity_Infixl (Range)
            | Fixity_Infixr (Range)
-- FunctionBinding ---------------------------------------------
data FunctionBinding = FunctionBinding_FunctionBinding (Range) (LeftHandSide) (RightHandSide)
-- FunctionBindings --------------------------------------------
type FunctionBindings = [FunctionBinding]
-- GuardedExpression -------------------------------------------
data GuardedExpression = GuardedExpression_GuardedExpression (Range) (Expression) (Expression)
-- GuardedExpressions ------------------------------------------
type GuardedExpressions = [GuardedExpression]
-- Import ------------------------------------------------------
data Import = Import_TypeOrClass (Range) (Name) (MaybeNames)
            | Import_TypeOrClassComplete (Range) (Name)
            | Import_Variable (Range) (Name)
-- ImportDeclaration -------------------------------------------
data ImportDeclaration = ImportDeclaration_Empty (Range)
                       | ImportDeclaration_Import (Range) (Bool) (Name) (MaybeName) (MaybeImportSpecification)
-- ImportDeclarations ------------------------------------------
type ImportDeclarations = [ImportDeclaration]
-- ImportSpecification -----------------------------------------
data ImportSpecification = ImportSpecification_Import (Range) (Bool) (Imports)
-- Imports -----------------------------------------------------
type Imports = [Import]
-- LeftHandSide ------------------------------------------------
data LeftHandSide = LeftHandSide_Function (Range) (Name) (Patterns)
                  | LeftHandSide_Infix (Range) (Pattern) (Name) (Pattern)
                  | LeftHandSide_Parenthesized (Range) (LeftHandSide) (Patterns)
-- Literal -----------------------------------------------------
data Literal = Literal_Char (Range) (String)
             | Literal_Float (Range) (String)
             | Literal_Int (Range) (String)
             | Literal_String (Range) (String)
-- MaybeDeclarations -------------------------------------------
data MaybeDeclarations = MaybeDeclarations_Just (Declarations)
                       | MaybeDeclarations_Nothing 
-- MaybeExports ------------------------------------------------
data MaybeExports = MaybeExports_Just (Exports)
                  | MaybeExports_Nothing 
-- MaybeExpression ---------------------------------------------
data MaybeExpression = MaybeExpression_Just (Expression)
                     | MaybeExpression_Nothing 
-- MaybeImportSpecification ------------------------------------
data MaybeImportSpecification = MaybeImportSpecification_Just (ImportSpecification)
                              | MaybeImportSpecification_Nothing 
-- MaybeInt ----------------------------------------------------
data MaybeInt = MaybeInt_Just (Int)
              | MaybeInt_Nothing 
-- MaybeName ---------------------------------------------------
data MaybeName = MaybeName_Just (Name)
               | MaybeName_Nothing 
-- MaybeNames --------------------------------------------------
data MaybeNames = MaybeNames_Just (Names)
                | MaybeNames_Nothing 
-- Module ------------------------------------------------------
data Module = Module_Module (Range) (MaybeName) (MaybeExports) (Body)
-- Name --------------------------------------------------------
data Name = Name_Identifier (Range) (Strings) (String)
          | Name_Operator (Range) (Strings) (String)
          | Name_Special (Range) (Strings) (String)
-- Names -------------------------------------------------------
type Names = [Name]
-- Pattern -----------------------------------------------------
data Pattern = Pattern_As (Range) (Name) (Pattern)
             | Pattern_Constructor (Range) (Name) (Patterns)
             | Pattern_InfixConstructor (Range) (Pattern) (Name) (Pattern)
             | Pattern_Irrefutable (Range) (Pattern)
             | Pattern_List (Range) (Patterns)
             | Pattern_Literal (Range) (Literal)
             | Pattern_Negate (Range) (Literal)
             | Pattern_NegateFloat (Range) (Literal)
             | Pattern_Parenthesized (Range) (Pattern)
             | Pattern_Record (Range) (Name) (RecordPatternBindings)
             | Pattern_Successor (Range) (Name) (Literal)
             | Pattern_Tuple (Range) (Patterns)
             | Pattern_Variable (Range) (Name)
             | Pattern_Wildcard (Range)
-- Patterns ----------------------------------------------------
type Patterns = [Pattern]
-- Position ----------------------------------------------------
data Position = Position_Position (String) (Int) (Int)
              | Position_Unknown 
-- Qualifier ---------------------------------------------------
data Qualifier = Qualifier_Empty (Range)
               | Qualifier_Generator (Range) (Pattern) (Expression)
               | Qualifier_Guard (Range) (Expression)
               | Qualifier_Let (Range) (Declarations)
-- Qualifiers --------------------------------------------------
type Qualifiers = [Qualifier]
-- Range -------------------------------------------------------
data Range = Range_Range (Position) (Position)
-- RecordExpressionBinding -------------------------------------
data RecordExpressionBinding = RecordExpressionBinding_RecordExpressionBinding (Range) (Name) (Expression)
-- RecordExpressionBindings ------------------------------------
type RecordExpressionBindings = [RecordExpressionBinding]
-- RecordPatternBinding ----------------------------------------
data RecordPatternBinding = RecordPatternBinding_RecordPatternBinding (Range) (Name) (Pattern)
-- RecordPatternBindings ---------------------------------------
type RecordPatternBindings = [RecordPatternBinding]
-- RightHandSide -----------------------------------------------
data RightHandSide = RightHandSide_Expression (Range) (Expression) (MaybeDeclarations)
                   | RightHandSide_Guarded (Range) (GuardedExpressions) (MaybeDeclarations)
-- SimpleType --------------------------------------------------
data SimpleType = SimpleType_SimpleType (Range) (Name) (Names)
-- Statement ---------------------------------------------------
data Statement = Statement_Empty (Range)
               | Statement_Expression (Range) (Expression)
               | Statement_Generator (Range) (Pattern) (Expression)
               | Statement_Let (Range) (Declarations)
-- Statements --------------------------------------------------
type Statements = [Statement]
-- Strings -----------------------------------------------------
type Strings = [String]
-- Type --------------------------------------------------------
data Type = Type_Application (Range) (Bool) (Type) (Types)
          | Type_Constructor (Range) (Name)
          | Type_Exists (Range) (Names) (Type)
          | Type_Forall (Range) (Names) (Type)
          | Type_Parenthesized (Range) (Type)
          | Type_Qualified (Range) (ContextItems) (Type)
          | Type_Variable (Range) (Name)
-- Types -------------------------------------------------------
type Types = [Type]
