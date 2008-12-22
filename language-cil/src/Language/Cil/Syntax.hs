-- |
-- Abstract Syntax Tree for the Common Intermediate Language.
-- Note; currently this is just a subset of CIL.
--

module Language.Cil.Syntax (
    Name
  , Offset
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

-- | Represents an offset, e.g. for local variables or arguments
type Offset = Int

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
    Constructor  Visibility [Parameter] [Directive] [(Label, OpCode)]
  | StaticMethod Visibility PrimitiveType Name [Parameter] [Directive] [(Label, OpCode)]
  -- InstanceMethod

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
    Add                -- ^ Pops 2 values, adds the values, pushes result.
  | And                -- ^ Pops 2 values, do bitwise AND between the values, pushes result.
  | Beq Label          -- ^ Pops 2 values, if first value is equal to second value, jump to specified label.
  | Bge Label          -- ^ Pops 2 values, if first value is greater or equal to second value, jump to specified label.
  | Bgt Label          -- ^ Pops 2 values, if first value is greater than second value, jump to specified label.
  | Ble Label          -- ^ Pops 2 values, if first value is lesser or equal to second value, jump to specified label.
  | Blt Label          -- ^ Pops 2 values, if first value is lesser or equal to second value, jump to specified label.
  | Box PrimitiveType  -- ^ Pops 1 value, boxes value type, pushes object reference.
  | Br Label           -- ^ Unconditionally jump to specified label.
  | Brfalse Label      -- ^ Pops 1 value, if value is false, null reference or zero, jump to specified label.
  | Brtrue Label       -- ^ Pops 1 value, if value is true, not null or non-zero, jump to specified label.
  | Call { association  :: Association     -- ^ Method is associated with class or instance.
         , returnType   :: PrimitiveType   -- ^ Return type of the method.
         , assemblyName :: Name            -- ^ Name of the assembly where the method resides.
         , typeName     :: Name            -- ^ Name of the type of which the method is a member.
         , methodName   :: Name            -- ^ Name of the method.
         , paramTypes   :: [PrimitiveType] -- ^ Types of the formal parameters of the method.
         } -- ^ Pops /n/ values, calls specified method, pushes return value (where /n/ is the number of formal parameters of the method).
  | Ceq                -- ^ Pops 2 values, if they are equal, pushes 1 to stack; otherwise, pushes 0.
  | Dup                -- ^ Pops 1 value, copies it, pushes the same value twise.
  | Ldarg Offset       -- ^ Loads /n/-th argument to current method onto stack.
  | Ldarg_0            -- ^ Loads 0th argument to current method onto stack.
  | Ldarg_1            -- ^ Loads 1th argument to current method onto stack.
  | Ldarg_2            -- ^ Loads 2th argument to current method onto stack.
  | Ldarg_3            -- ^ Loads 3th argument to current method onto stack.
  | Ldc_i4 Int         -- ^ Loads the supplied 32-bit integer onto the stack.
  | Ldc_i4_0           -- ^ Loads the value 0 onto the stack.
  | Ldc_i4_1           -- ^ Loads the value 1 onto the stack.
  | Ldc_i4_2           -- ^ Loads the value 2 onto the stack.
  | Ldc_i4_3           -- ^ Loads the value 3 onto the stack.
  | Ldc_i4_4           -- ^ Loads the value 4 onto the stack.
  | Ldc_i4_5           -- ^ Loads the value 5 onto the stack.
  | Ldc_i4_6           -- ^ Loads the value 6 onto the stack.
  | Ldc_i4_7           -- ^ Loads the value 7 onto the stack.
  | Ldc_i4_8           -- ^ Loads the value 8 onto the stack.
  | Ldc_i4_m1          -- ^ Loads the value -1 onto the stack.
  | Ldfld { fieldType    :: PrimitiveType  -- ^ Type of the field.
          , assemblyName :: Name           -- ^ Name of the assembly where the field resides.
          , typeName     :: Name           -- ^ Name of the type of which the field is a member.
          , fieldName    :: Name           -- ^ Name of the field.
          } -- ^ Pops object reference, find value of specified field on object, pushes value to the stack.
  | Ldloc Offset       -- ^ Pushes value of local variable, specified by index, to the stack.
  | Ldloc_0            -- ^ Pushes 0th local variable to the stack.
  | Ldloc_1            -- ^ Pushes 1th local variable to the stack.
  | Ldloc_2            -- ^ Pushes 2th local variable to the stack.
  | Ldloc_3            -- ^ Pushes 3th local variable to the stack.
  | Ldloca Offset      -- ^ Pushes address of local variable, specified by index, to the stack.
  | Ldstr String       -- ^ Pushes an object reference to the specified string constant.
  | Neg                -- ^ Pops 1 value, negates the value, pushes the value.
  | Newobj { returnType   :: PrimitiveType    -- ^ Return type of the constructor (almost alway Void).
           , assemblyName :: Name             -- ^ Name of the assembly where the constructor resides.
           , typeName     :: Name             -- ^ Name of the type of which the constructor is a member.
           , paramTypes   :: [PrimitiveType]  -- ^ Types of the formal paramters of the constructor.
           } -- ^ Creates a new object or instance of a value type. Pops /n/ values, calls the specified constructor, pushes a new object reference onto the stack (where /n/ is the number of formal parameters of the constructor).
  | Nop                -- ^ No operation is performed.
  | Pop                -- ^ Pops the top of the stack.
  | Rem                -- ^ Pops 2 values, devides the first value by the second value, pushes the remainder.
  | Ret                -- ^ Returns from the current method. Pushes top of the stack to the top of the callers stack (if stack is not empty).
  | Stfld { fieldType    :: PrimitiveType  -- ^ Type of the field.
          , assemblyName :: Name           -- ^ Name of the assembly where the field resides.
          , typeName     :: Name           -- ^ Name of the type of which the field is a member.
          , fieldName    :: Name           -- ^ Name of the field.
          } -- ^ Replaces the value stored in the field of an object reference or pointer with a new value.
  | Stloc Offset       -- ^ Pops 1 value, stores it in the local variable specified by index.
  | Stloc_0            -- ^ Pops 1 value, stores it in the 0th local variable.
  | Stloc_1            -- ^ Pops 1 value, stores it in the 1th local variable.
  | Stloc_2            -- ^ Pops 1 value, stores it in the 2th local variable.
  | Stloc_3            -- ^ Pops 1 value, stores it in the 3th local variable.
  | Sub                -- ^ Pops 2 values, substracts second value from the first value, pushes result.
  | Tail               -- ^ Performs subsequent call as a tail call, by replacing current stack frame with callee stack frame.
  | Tailcall OpCode    -- ^ Performs provided call as a tail call, by replacing current stack frame with callee stack frame.

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
  | GenericType Offset

