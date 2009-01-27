%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cil Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Cil.Common} import({%{EH}Base.Common},{%{EH}Cil.TyTag})
%%]
%%[8 hs export(hsn2TypeDottedName, toFieldName, toFieldTypes, toTypeDefs)
%%]
%%[(8 codegen grin) hs import(Language.Cil)
%%]
%%[(8 codegen grin) hs import(Debug.Trace)
%%]
%%[(8 codegen grin) hs import(Data.Char (toLower))
%%]
%%[(8 codegen grin) hs import(Data.List (groupBy, sortBy))
%%]

%%[8
namespace :: DottedName
namespace = "Haskell.Ehc"

hsn2TypeDottedName :: HsName -> DottedName
hsn2TypeDottedName hsn = namespace ++ "." ++ fancyName hsn

toFieldName :: TyTag -> Int -> DottedName
toFieldName (TyCon _ cNm _ x mx) y =
  let flds = fieldNames mx
  in if y <= x
     then flds !! y
     else error $ "[Cil.Common.toFieldName] "
                    ++ (hsnShowAlphanumeric cNm) ++ " doesn't have "
                    ++ (show y) ++ " fields."

-- Can also be used to get the type of the stored fields, since they exactly match the constructor argument types.
toFieldTypes :: TyTag -> [PrimitiveType]
toFieldTypes (TyFun _ _)             = []
toFieldTypes con@(TyCon hsn _ _ x _) =
  case (hsnShowAlphanumeric hsn) of
    "Int"          -> [Int32]
    "Char"         -> [Char]
    "PackedString" -> [String]
    _              -> replicate x Object

toTypeDefs :: DottedName -> [TyTag] -> [TypeDef]
toTypeDefs callbackNm tags =
  let stags = sortBy (\x y -> compare (toTypeName x) (toTypeName y)) tags
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
    _              -> classDef Public tyNm noExtends [] []
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
  classDef Public fullName noExtends noImplements []
    [ defaultCtor []]
    ( classDef Private tyNm (extends fullName) []
       [ Field Instance2 Public ty "Value"]
       [ Constructor Public [ Param ty "value" ]
           [ ldarg 0
           , call Instance Void "" fullName ".ctor" []
           , ldarg 0
           , ldarg 1
           , stfld ty "" (fullName ++ "/" ++ tyNm) "Value"
           , ret
           ]
       ]
       []
     : (map (subTys callbackNm fullName) tags)
     )
  where
    fullName = namespace ++ "." ++ tyNm

subTys :: DottedName -> DottedName -> TyTag -> TypeDef
subTys callbackNm tyNm (TyFun _ fnm) =
  classDef Public subTyNm (extends tyNm) noImplements []
    [ defaultCtor []
    , Method Static Public Object "Invoke" []
        [ call StaticCallConv Object "" callbackNm fnNm []
        , ret
        ]
    ]
    []
  where
    fnNm      = hsnShowAlphanumeric fnm
    subTyNm   = "<Thunk>" ++ fnNm
subTys callbackNm tyNm (TyPApp _ fnm needs) =
  classDef Public subTyNm (extends tyNm) noImplements []
    [ defaultCtor []
    , Method Static Public Object "Invoke" []
        [ call StaticCallConv Object "" callbackNm fnNm []
        , ret
        ]
    ]
    []
  where
    fnNm      = hsnShowAlphanumeric fnm
    subTyNm   = "<PApp>" ++ fnNm ++ "`" ++ show needs
subTys csNm tyNm (TyCon _ cnm _ a ma) =
  classDef Public subTyNm (extends tyNm) []
    fields
    [ctor]
    []
  where
    subTyNm   = hsnShowAlphanumeric cnm
    tySubTyNm = tyNm ++ "/" ++ subTyNm
    fields    = map (Field Instance2 Public Object) (take a $ fieldNames ma)
    pNm ""     = ""
    pNm (c:cs) = toLower c : cs
    ctor      = Constructor Public (map (\(Field _ _ t n) -> Param t (pNm n)) fields)
                  $
                  [ ldarg 0
                  , call Instance Void "" tyNm ".ctor" []
                  ]
                  ++
                  concatMap (\((Field _ _ t n), x) -> [ldarg 0, ldarg x, stfld Object "" tySubTyNm n]) (zip fields [1..])
                  ++
                  [ ret ]

fieldNames :: Int -> [DottedName]
fieldNames 1 = [ "Value" ]
fieldNames n | n < 11    = [ "First", "Second", "Third", "Fourth", "Fifth"
                           , "Sixth", "Seventh", "Eighth", "Ninth", "Tenth" ]
                           ++ drop 7 (fieldNames 99)
             | otherwise = map (\n -> "Field" ++ show n) [1..]
%%]

