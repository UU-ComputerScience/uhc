%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cil Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Cil.Common} import({%{EH}Base.Common})
%%]
%%[8 hs export(namespace, ctag2TypeDottedName, ctag2ConstrDottedName, hsn2TypeDottedName, hsn2ConstrDottedName, ctag2ParamTypes, ctagTypeDef)
%%]
%%[(8 codegen grin) hs import(Language.Cil)
%%]
%%[(8 codegen grin) hs import(Data.Char (toLower))
%%]

%%[8
namespace :: DottedName
namespace = "Haskell.Ehc"

ctag2TypeDottedName :: CTag -> DottedName
ctag2TypeDottedName (CTag tyNm _ _ _ _) =
  namespace ++ "." ++ fancyName tyNm

ctag2ConstrDottedName :: CTag -> DottedName
ctag2ConstrDottedName (CTag tyNm cnNm _ _ _) =
  namespace ++ "." ++ fancyName tyNm ++ "/" ++ fancyName cnNm

hsn2TypeDottedName :: HsName -> DottedName
hsn2TypeDottedName hsn =
  namespace ++ "." ++ fancyName hsn

hsn2ConstrDottedName :: HsName -> HsName -> DottedName
hsn2ConstrDottedName hsn1 hsn2 =
  namespace ++ "." ++ fancyName hsn1 ++ "/" ++ fancyName hsn2

fancyName :: HsName -> DottedName
fancyName hsn =
  case (hsnShowAlphanumeric hsn) of
    "comma0"  -> "Unit"
    "comma2"  -> "Tuple`2"
    "comma3"  -> "Tuple`3"
    "comma4"  -> "Tuple`4"
    "comma5"  -> "Tuple`5"
    "comma6"  -> "Tuple`6"
    "comma7"  -> "Tuple`7"
    "comma8"  -> "Tuple`8"
    "comma9"  -> "Tuple`9"
    "comma10" -> "Tuple`10"
    name      -> name

ctag2ParamTypes :: CTag -> [PrimitiveType]
ctag2ParamTypes (CTag hsn _ _ x _) =
  case (hsnShowAlphanumeric hsn) of
    "Int"          -> [Int32]
    "Char"         -> [Char]
    "PackedString" -> [String]
    _              -> replicate x Object

ctagTypeDef :: (HsName, [(HsName, CTag)]) -> TypeDef
ctagTypeDef (anm, hcx) =
  case (hsnShowAlphanumeric anm) of
     "Char"         -> charTypeDef
     "Int"          -> intTypeDef
     "PackedString" -> packedStringTypeDef
     "comma0"       -> unitTypeDef
     "comma2"       -> tupleTypeDef 2
     "comma3"       -> tupleTypeDef 3
     "comma4"       -> tupleTypeDef 4
     "comma5"       -> tupleTypeDef 5
     "comma6"       -> tupleTypeDef 6
     "comma7"       -> tupleTypeDef 7
     "comma8"       -> tupleTypeDef 8
     "comma9"       -> tupleTypeDef 9
     "comma10"      -> tupleTypeDef 10
     _              -> classDef Public tyNm noExtends [] []
                         [ defaultCtor [] ] (map subTys hcx)
  where
    tyNm = namespace ++ "." ++ hsnShowAlphanumeric anm
    pNm ""     = ""
    pNm (c:cs) = toLower c : cs
    subTys (snm, (CTag _ _ t a ma)) =
      classDef Public subTyNm (extends tyNm) []
        fields
        [ctor]
        []
      where
        subTyNm   = hsnShowAlphanumeric snm
        tySubTyNm = tyNm ++ "/" ++ subTyNm
        fields    = map (Field Instance2 Public Object) (take a $ fieldNames ma)
        ctor      = Constructor Public (map (\(Field _ _ t n) -> Param t (pNm n)) fields)
                      $
                      [ ldarg 0
                      , call Instance Void "" tyNm ".ctor" []
                      ]
                      ++
                      concatMap (\((Field _ _ t n), x) -> [ldarg 0, ldarg x, stfld Object "" tySubTyNm n]) (zip fields [1..])
                      ++
                      [ ret ]

unitTypeDef :: TypeDef
unitTypeDef = ctagTypeDef (hsn, [(hsn, CTag hsn hsn 0 0 0)])
  where
    hsn = hsnFromString "Unit"

tupleTypeDef :: Int -> TypeDef
tupleTypeDef x = ctagTypeDef (hsn, [(hsn, CTag hsn hsn 0 x x)])
  where
    hsn = hsnFromString ("Tuple`" ++ show x)

charTypeDef :: TypeDef
charTypeDef = simpleTypeDef Char "Char"

intTypeDef :: TypeDef
intTypeDef = simpleTypeDef Int32 "Int"

packedStringTypeDef :: TypeDef
packedStringTypeDef = simpleTypeDef String "PackedString"

simpleTypeDef :: PrimitiveType -> DottedName -> TypeDef
simpleTypeDef ty tyNm =
  classDef Public fullName noExtends noImplements []
    [ defaultCtor []]
    [ classDef Private tyNm (extends fullName) []
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
     ]
  where
    fullName = namespace ++ "." ++ tyNm

fieldNames :: Int -> [DottedName]
fieldNames 1 = [ "Value" ]
fieldNames n | n < 11    = [ "First", "Second", "Third", "Fourth", "Fifth"
                           , "Sixth", "Seventh", "Eighth", "Ninth", "Tenth" ]
                           ++ drop 7 (fieldNames 99)
             | otherwise = map (\n -> "Field" ++ show n) [1..]
%%]

