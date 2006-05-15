$Id$

This file contains utility functions for primitives in GRIN as well as the
primitives information table.

(ffi "prim...")

- abstract values representing the result of a primitive operation in the HPT analysis
- meta info for code generation
- code snippets to generate the code for a primitive (C--)

%%[8.abstractValues import(HeapPointsToFixpoint,"EHCommon(HsName(..))", "qualified Data.Set as Set", GrinCode,GRINCCommon)
unboxedBasic = AV_Nodes $ Map.fromList [ (GrTag_Unboxed, [AV_Basic])
                                       ]
booleanNodes = AV_Nodes $ Map.fromList [ (GrTag_Lit GrTagCon 0 (HNm "_False"), [AV_Basic])
                                       , (GrTag_Lit GrTagCon 1 (HNm "_True" ), [AV_Basic])
                                       ]
compareNodes = AV_Nodes $ Map.fromList [ (GrTag_Lit GrTagCon 0 (HNm "_EQ"), [AV_Basic])
                                       , (GrTag_Lit GrTagCon 1 (HNm "_GT"), [AV_Basic])
                                       , (GrTag_Lit GrTagCon 2 (HNm "_LT"), [AV_Basic])
                                       ]
%%]

%%[8.codeGeneration import(Cmm.CmmCode, Cmm.CmmBuilding) export(false_node, true_node)
--buildin datatype
true_tag   = cmmVar "@C$_True"
false_tag  = cmmVar "@C$_False"
eq_tag     = cmmVar "@C$_EQ"
lt_tag     = cmmVar "@C$_LT"
gt_tag     = cmmVar "@C$_GT"

true_node   = [true_tag , int 0]
false_node  = [false_tag, int 0]
eq_node     = [eq_tag   , int 0]
lt_node     = [lt_tag   , int 0]
gt_node     = [gt_tag   , int 0]

-- emitting code for primitives
emitPrimModInt (l:r:[]) tn = assignOrReturn tn [(cmmVar l <%> cmmVar r)]
emitPrimDivInt (l:r:[]) tn = assignOrReturn tn [(cmmVar l </> cmmVar r)]
emitPrimMulInt (l:r:[]) tn = assignOrReturn tn [(cmmVar l <*> cmmVar r)]
emitPrimAddInt (l:r:[]) tn = assignOrReturn tn [(cmmVar l <+> cmmVar r)]
emitPrimSubInt (l:r:[]) tn = assignOrReturn tn [(cmmVar l <-> cmmVar r)]

emitPrimGtInt = booleanCompare "gt"
emitPrimLtInt = booleanCompare "lt"
emitPrimEqInt = booleanCompare "eq"

booleanCompare s (l:r:[]) (Right (t:f:[])) = ite (prim s [valArg $ cmmVar l, valArg $ cmmVar r]) t f

emitPrimCmpInt (l:r:[]) tn
  = ite (prim "gt" [valArg $ cmmVar l,valArg $ cmmVar r])
        (assignOrReturn tn gt_node)
        (ite (prim "lt" [valArg $ cmmVar l,valArg $ cmmVar r])
             (assignOrReturn tn lt_node)
             (assignOrReturn tn eq_node)
        )

assignOrReturn (Left tn) expr = if null tn
                                then cmmReturn "" (map valArg expr)
                                else updates (zipWith (\l r -> (varUpdate l,r)) tn expr)
%%]

%%[8.primitivesMap import("qualified Data.Map as Map")
type PrimitiveInfo = (Int
                     , [String]
                     , CmmNames -> Either CmmNames [CmmBodyBuilder] -> CmmBodyBuilder
                     , AbstractValue
                     )

primitivesMap  ::  Map.Map String PrimitiveInfo
primitivesMap  =   Map.fromList primitivesTable
    where
    -- primitivesTable: list of name |-> (return size, required imports, arguments -> result vars or bodies -> primitive, AV)
    primitivesTable
      =  [ ("primAddInt"   , (1, [], emitPrimAddInt,  unboxedBasic))
         , ("primSubInt"   , (1, [], emitPrimSubInt,  unboxedBasic))
         , ("primMulInt"   , (1, [], emitPrimMulInt,  unboxedBasic))
         , ("primDivInt"   , (1, [], emitPrimDivInt,  unboxedBasic))
         , ("primModInt"   , (1, [], emitPrimModInt,  unboxedBasic))
         , ("primShlWord"  , (1, [], undefined     ,  unboxedBasic))
         , ("primShrWord"  , (1, [], undefined     ,  unboxedBasic))
         , ("primAndWord"  , (1, [], undefined     ,  unboxedBasic))
         , ("primXorWord"  , (1, [], undefined     ,  unboxedBasic))
         , ("primOrWord"   , (1, [], undefined     ,  unboxedBasic))
         , ("primEqInt"    , (2, [], emitPrimEqInt ,  booleanNodes))
         , ("primLtInt"    , (2, [], emitPrimLtInt ,  booleanNodes))
         , ("primGtInt"    , (2, [], emitPrimGtInt ,  booleanNodes))
         , ("primCmpInt"   , (2, [], emitPrimCmpInt,  compareNodes))
        ]
%%]

%%[8.utils export(isPrim,isConditionalPrim,codeGenInfo,primSize, primImports,primCode,primAV)
isPrim             = ("prim" ==) . take 4
isConditionalPrim  = flip elem ["primEqInt", "primLtInt", "primGtInt"]

getPrimInfo :: (PrimitiveInfo -> b) -> String -> b
getPrimInfo f prim = f (Map.findWithDefault (error $ "prim '" ++ prim ++ "' not found!") prim primitivesMap)

primSize     =  getPrimInfo (\ (a, _, _, _) -> a)
primImports  =  getPrimInfo (\ (_, b, _, _) -> b)
primCode     =  getPrimInfo (\ (_, _, c, _) -> c)
primAV       =  getPrimInfo (\ (_, _, _, d) -> d)
codeGenInfo  =  getPrimInfo (\ (a, b, c, _) -> (a, b, c))
%%]

% vim:et:ts=4:ai:
