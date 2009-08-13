%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cil Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Exposes some convenient functions for dealing with language-cil from UHC.

Contains some implicit rules of how to generate fancy names. For example:
\begin{verbatim}
data Single = Single Int
\end{verbatim}
becomes:
\begin{verbatim}
class Single {
	public Object Value
}
\end{verbatim}
Whereas:
\begin{verbatim}
data Pair = Pair Int Int
\end{verbatim}
becomes:
\begin{verbatim}
class Pair {
	public Object First
	public Object Second
}
\end{verbatim}

%%]

%%[(8 codegen clr) module {%{EH}Cil.Common} import({%{EH}Base.Common},{%{EH}Cil.TyTag})
%%]
%%[(8 codegen clr) hs export(hsn2TypeDottedName, toFieldName, toFieldTypes, toTypeDefs)
%%]
%%[(8 codegen clr) hs import(Language.Cil)
%%]
%%[(8 codegen clr) hs import(Debug.Trace)
%%]
%%[(8 codegen clr) hs import(Data.Char (toLower))
%%]
%%[(8 codegen clr) hs import(Data.List (groupBy, sortBy))
%%]

%%[(8 codegen clr)
namespace :: DottedName
namespace = "Haskell.Ehc"

hsn2TypeDottedName :: HsName -> DottedName
hsn2TypeDottedName hsn = namespace ++ "." ++ fancyName hsn

toFieldName :: TyTag -> Int -> DottedName
toFieldName (TyCon  _ cNm _ a ma) x   = toFieldName' (fieldNames ma)           cNm a x
toFieldName (TyFun  _ fNm a) x        = toFieldName' (fieldNames a)            fNm a x
toFieldName (TyPApp _ pNm needs fa) x = toFieldName' (fieldNames (fa - needs)) pNm needs x

toFieldName' :: [DottedName] -> HsName -> Int -> Int -> DottedName
toFieldName' flds nm x y =
  if y <= x
  then flds !! y
  else error $ "[Cil.Common.toFieldName] "
                 ++ (hsnShowAlphanumeric nm) ++ " doesn't have "
                 ++ (show y) ++ " fields."

-- Can also be used to get the type of the stored fields, since they exactly match the constructor argument types.
toFieldTypes :: TyTag -> [PrimitiveType]
toFieldTypes con@(TyCon hsn _ _ x _) =
  case (hsnShowAlphanumeric hsn) of
    "Int"          -> [Int32]
    "Char"         -> [Char]
    "PackedString" -> [String]
    _              -> replicate x Object
toFieldTypes (TyFun _ _ args)        = replicate args Object
toFieldTypes (TyPApp _ _ needs args) = replicate (args - needs) Object

toTypeDefs :: DottedName -> [TyTag] -> [TypeDef]
toTypeDefs callbackNm tags =
  let stags = sortBy (\x y -> cmpHsNameOnNm (toTypeName x) (toTypeName y)) tags
      gtags = groupBy (\x y -> toTypeName x == toTypeName y) stags
  in map (toTypeDef callbackNm) gtags

toTypeDef :: DottedName -> [TyTag] -> TypeDef
toTypeDef callbackNm tags =
  case (hsnShowAlphanumeric hsNm) of
    "Char"         -> charTypeDef callbackNm tags
    "Int"          -> intTypeDef  callbackNm tags
    "PackedString" -> packedStringTypeDef callbackNm tags
    "comma0"       -> unitTypeDef callbackNm tags
    "comma2"       -> tupleTypeDef 2  callbackNm tags
    "comma3"       -> tupleTypeDef 3  callbackNm tags
    "comma4"       -> tupleTypeDef 4  callbackNm tags
    "comma5"       -> tupleTypeDef 5  callbackNm tags
    "comma6"       -> tupleTypeDef 6  callbackNm tags
    "comma7"       -> tupleTypeDef 7  callbackNm tags
    "comma8"       -> tupleTypeDef 8  callbackNm tags
    "comma9"       -> tupleTypeDef 9  callbackNm tags
    "comma10"      -> tupleTypeDef 10 callbackNm tags
    _              -> classDef [CaPublic] tyNm noExtends [] []
                        [ defaultCtor [] ]
                        (map (subTys callbackNm tyNm) tags)
  where
    hsNm = toTypeName (head tags)
    tyNm = namespace ++ "." ++ hsnShowAlphanumeric hsNm

unitTypeDef :: DottedName -> [TyTag] -> TypeDef
unitTypeDef callbackNm tags = toTypeDef callbackNm [ TyCon hsn hsn 0 0 0 ]
  where
    hsn = hsnFromString "Unit"

tupleTypeDef :: Int -> DottedName -> [TyTag] -> TypeDef
tupleTypeDef x callbackNm tags = toTypeDef callbackNm [ TyCon hsn hsn 0 x x ]
  where
    hsn = hsnFromString ("Tuple`" ++ show x)

charTypeDef :: DottedName -> [TyTag] -> TypeDef
charTypeDef callbackNm tags = simpleTypeDef Char "Char" callbackNm tags

intTypeDef :: DottedName -> [TyTag] -> TypeDef
intTypeDef callbackNm tags = simpleTypeDef Int32 "Int" callbackNm tags

packedStringTypeDef :: DottedName -> [TyTag] -> TypeDef
packedStringTypeDef callbackNm tags = simpleTypeDef String "PackedString" callbackNm tags

simpleTypeDef :: PrimitiveType -> DottedName -> DottedName -> [TyTag] -> TypeDef
simpleTypeDef ty tyNm callbackNm (_:tags) = -- drop first constructor, that was the simple type
  classDef [CaPublic] fullName noExtends noImplements []
    [ defaultCtor []]
    ( classDef [CaNestedPublic] tyNm (extends fullName) []
       [ Field [FaPublic] ty "Value"]
       [ Constructor [MaPublic] Void [ Param ty "value" ]
           [ ldarg 0
           , call [CcInstance] Void "" fullName ".ctor" []
           , ldarg 0
           , ldarg 1
           , stfld ty "" (fullName ++ "/" ++ tyNm) "Value"
           , ret
           ]
       , Method [MaVirtual, MaPublic] String "ToString" []
           [ ldstr tyNm
           , ldstr " "
           , ldarg 0
           , ldflda ty "" tySubTyNm "Value"
           , call [CcInstance] String "" (cil ty "") "ToString" []
           , call [] String "" "string" "Concat" [String, String, String]
           , ret
           ]
       ]
       []
     : (map (subTys callbackNm fullName) tags)
     )
  where
    fullName  = namespace ++ "." ++ tyNm
    tySubTyNm = fullName ++ "/" ++ tyNm

subTys :: DottedName -> DottedName -> TyTag -> TypeDef
subTys callbackNm tyNm (TyFun _ fnm args) =
  classDef [CaNestedPublic] subTyNm (extends tyNm) noImplements
    ( fields args args)
    [ ctor args args tyNm subTyNm
    , toString args args tyNm subTyNm

    {-
      Currently the Invoke method isn't used.

      The idea is to inline all the relevant code into here, but that would
      require the Invoke method to have arguments and call fnNm correctly.

      A second idea is to cache the result of the Invoke method, such that, the
      second time it's called, it will already know the answer.
      Also, this can be combined with updating the variable at the callsite of
      the Invoke method, like so:
        if (x is Fun)
           x = x.Invoke()
    -}

    -- , Method [MaStatic, MaPublic] Object "Invoke" []
    --     [ call [] Object "" callbackNm fnNm []
    --     , ret
    --     ]
    ]
    []
  where
    fnNm      = hsnShowAlphanumeric fnm
    subTyNm   = "<Thunk>" ++ fnNm
subTys callbackNm tyNm (TyPApp _ fnm needs args) =
  classDef [CaNestedPublic] subTyNm (extends tyNm) noImplements
    (fields arity arity)
    [ ctor arity arity tyNm subTyNm
    , toString arity arity tyNm subTyNm

    -- See comment for TyFun Invoke method.

    -- , Method [MaStatic, MaPublic] Object "Invoke" []
    --     [ call [] Object "" callbackNm fnNm []
    --     , ret
    --     ]
    ]
    []
  where
    fnNm      = hsnShowAlphanumeric fnm
    subTyNm   = "<PApp>" ++ fnNm ++ "`" ++ show needs
    arity     = args - needs
subTys csNm tyNm (TyCon _ cnm _ a ma) =
  classDef [CaNestedPublic] subTyNm (extends tyNm) []
    (fields ma a)
    [ ctor ma a tyNm subTyNm
    , toString ma a tyNm subTyNm
    ]
    []
  where
    subTyNm   = hsnShowAlphanumeric cnm

{-
  Future idea: All calls to ToString are currently virtual calls.
  With the correct type information, all these can probabily be changed to be non-virtual calls.
-}
toString :: Int -> Int -> DottedName -> DottedName -> MethodDef
toString maxArity arity tyNm conNm =
    Method [MaVirtual, MaPublic] String "ToString" [] $
      [ ldstr (conNm) ]
      ++ foldr
           (\(Field _ t n) ms ->
               [ ldstr " ("
               , ldarg 0
               , ldfld Object "" tyConNm n
               , callvirt String "" "object" "ToString" []
               , call [] String "" "string" "Concat" [String, String, String]
               , ldstr ")"
               , call [] String "" "string" "Concat" [String, String]
               ] ++ ms) [] flds
      ++ [ ret ]
  where
    flds    = fields maxArity arity
    tyConNm = tyNm ++ "/" ++ conNm

ctor :: Int -> Int -> DottedName -> DottedName -> MethodDef
ctor maxArity arity tyNm conNm =
  Constructor [MaPublic] Void (map (\(Field _ t n) -> Param t (pNm n)) flds)
    $
    [ ldarg 0
    , call [CcInstance] Void "" tyNm ".ctor" []
    ]
    ++
    concatMap (\((Field _ t n), x) ->
                 [ ldarg 0
                 , ldarg x
                 , stfld Object "" tyConNm n
                 ]) (zip flds [1..])
    ++
    [ ret ]
  where
    pNm ""     = ""
    pNm (c:cs) = toLower c : cs
    flds       = fields maxArity arity
    tyConNm    = tyNm ++ "/" ++ conNm

fields :: Int -> Int -> [FieldDef]
fields maxArity arity = map (Field [FaPublic] Object) (take arity $ fieldNames maxArity)

fieldNames :: Int -> [DottedName]
fieldNames 1 = [ "Value" ]
fieldNames n | n < 11    = [ "First", "Second", "Third", "Fourth", "Fifth"
                           , "Sixth", "Seventh", "Eighth", "Ninth", "Tenth" ]
                           ++ drop 7 (fieldNames 99)
             | otherwise = map (\n -> "Field" ++ show n) [1..]
%%]

