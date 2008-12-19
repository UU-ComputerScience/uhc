-- |
-- Abstract Syntax Tree for the Common Intermediate Language.
-- Note; currently this is just a subset of CIL.
--

module Language.Cil.Syntax (
    Name
  , Assembly      (..)
  , TypeDef       (..)
  , GenParam      (..)
  , Visibility    (..)
  , FieldDef      (..)
  , MethodDef     (..)
  , Parameter     (..)
  , Directive     (..)
  , Local         (..)
  , Label
  , OpCode        (..)
  , Association   (..)
  , PrimitiveType (..)
  ) where

-- | Represents a name in the CIL world.
-- These need to confirm to certain restrictions, altough these aren't
-- currently checked.
type Name = String

-- | Represents the top level Assembly.
-- This is the root of a CIL program.
data Assembly =
    Assembly Name [TypeDef]

-- | A Type definition in CIL, either a class or a value type.
data TypeDef =
    Class Visibility Name [FieldDef] [MethodDef]
  | GenericClass Visibility Name [GenParam] [FieldDef] [MethodDef]

data Visibility =
    AssemblyVisible
  | Family
  | FamilyAndAssembly
  | FamilyOrAssembly
  | Private
  | Public

-- | A parameter to a generic class.
-- Not fully implemented yet, constraints aren't supported.
data GenParam =
    GenParam -- constraintFlags :: [ConstraintFlag]
             -- constraints     :: [Name]
             {- paramName       -} Name

data FieldDef =
    Field Visibility PrimitiveType Name

-- | A Method definition in CIL.
-- Currently, only static methods are implemented.
data MethodDef =
    Constructor  Visibility [Parameter] [Directive] [OpCode]
  | StaticMethod Visibility PrimitiveType Name [Parameter] [Directive] [OpCode]
  -- InstanceMethodLanguage.Haskell.Exts.Build

-- | Represents a formal parameter to a method.
data Parameter =
    Param PrimitiveType Name

-- | Directive meta data for method definitions.
data Directive =
    EntryPoint
  | LocalsInit [Local]
  | MaxStack Int

-- | Local variables used inside a method definition.
data Local =
    Local PrimitiveType Name

-- | Represents a Label in CIL.
type Label = String

-- | CIL OpCodes inside a method definition.
-- See <http://msdn.microsoft.com/en-us/library/system.reflection.emit.opcodes_members.aspx>
-- for a more complete list with documentation.
data OpCode =
    Label Label OpCode -- ^ Meta instruction. Give an instruction a label, used in jumps.
  | Add                -- ^ Adds two values and pushes the result onto the evaluation stack.
  | And                -- ^ Computes the bitwise AND of two values and pushes the result onto the evaluation stack.
  | Beq Label          -- ^ Transfers control to a target instruction if two values are equal.
  | Bge Label          -- ^ Transfers control to a target instruction if the first value is greater than or equal to the second value.
  | Bgt Label          -- ^ Transfers control to a target instruction if the first value is greater than the second value.
  | Ble Label          -- ^ Transfers control to a target instruction if the first value is less than or equal to the second value.
  | Blt Label          -- ^ Transfers control to a target instruction if the first value is less than the second value.
  | Box PrimitiveType  -- ^ Converts a value type to an object reference (type O).
  | Br Label           -- ^ Unconditionally transfers control to a target instruction.
  | Brfalse Label      -- ^ Transfers control to a target instruction if value is false, a null reference, or zero.
  | Brtrue Label       -- ^ Transfers control to a target instruction if value is true, not null, or non-zero.
  | Break              -- ^ Signals the Common Language Infrastructure (CLI) to inform the debugger that a break point has been tripped.
  | Call { association  :: Association     -- ^ Method is associated with class or instance.
         , returnType   :: PrimitiveType   -- ^ Return type of the method.
         , assemblyName :: Name            -- ^ Name of the assembly where the method lives.
         , typeName     :: Name            -- ^ Name of the type of which the method is a member.
         , methodName   :: Name            -- ^ Name of the method.
         , paramTypes   :: [PrimitiveType] -- ^ Types of the formal parameters of the method.
         } -- ^ Calls the indicated method.
  | Ceq                -- ^ Compares two values. If they are equal, the integer value 1 /(int32)/ is pushed onto the evaluation stack; otherwise 0 /(int32)/ is pushed onto the evaluation stack.
  | Dup                -- ^ Copies the current topmost value on the evaluation stack, and then pushes the copy onto the evaluation stack.
  | Ldarg Int
  | Ldc_i4 Int
  | Ldfld { fieldType    :: PrimitiveType
          , assemblyName :: Name
          , typeName     :: Name
          , fieldName    :: Name
          } -- ^ Finds the value of a field in the object whose reference is currently on the evaluation stack.
  | Ldloc Int
  | Ldloca Int
  | Ldstr String
  | Neg
  | Newobj { association  :: Association
           , returnType   :: PrimitiveType
           , assemblyName :: Name
           , typeName     :: Name
           , paramTypes   :: [PrimitiveType]
           } -- ^ Creates a new object or a new instance of a value type, pushing an object reference (type O) onto the evaluation stack.
  | Nop
  | Pop
  | Rem
  | Ret
  | Stfld { fieldType    :: PrimitiveType
          , assemblyName :: Name
          , typeName     :: Name
          , fieldName    :: Name
          } -- ^ Replaces the value stored in the field of an object reference or pointer with a new value.
  | Stloc Int          -- ^ Pops the current value from the top of the evaluation stack and stores it in a the local variable list at a specified index.
  | Sub                -- ^ Subtracts one value from another and pushes the result onto the evaluation stack. 
  | Tail               -- ^ Performs a postfixed method call instruction such that the current method's stack frame is removed before the actual call instruction is executed.

data Association =
    Static
  | Instance

data PrimitiveType =
    Void
  | Bool
  | Char
  | Byte
  | Int32
  | Int64
  | String
  | Object
  | ValueType Name Name
  | ReferenceType Name Name
  | GenericType Int

