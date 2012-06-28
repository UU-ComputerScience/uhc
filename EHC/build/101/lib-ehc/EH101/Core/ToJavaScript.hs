

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/ToJavaScript.ag)
module EH101.Core.ToJavaScript(cmod2JavaScriptModule) where

import EH101.Base.Common
import EH101.Opts
import EH101.Ty
import EH101.Core
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Maybe
import Data.List
import EH.Util.Utils
import qualified EH.Util.FastSeq as Seq
import EH101.Base.Builtin
import EH101.Base.Builtin2
import EH101.BuiltinPrims
import EH101.Base.BasicAnnot
import EH101.Base.GenJavaLike
import EH101.Gam.DataGam
import qualified EH101.JavaScript as J
import EH101.Foreign.Extract
import EH101.Base.Debug
import EH.Util.Pretty



















cmod2JavaScriptModule :: EHCOpts -> DataGam -> CModule -> J.JavaScriptModule
cmod2JavaScriptModule opts dataGam cmod
  =  js_Syn_CodeAGItf t
  where t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                                          (Inh_CodeAGItf { opts_Inh_CodeAGItf = opts
                                                         , dataGam_Inh_CodeAGItf = dataGam
                                                         })



nmFunSuff :: Int -> String
nmFunSuff n | n >  5    = "N"
            | n >= 0    = show n
            | otherwise = ""
-- nmApplyN n  = "_a" ++ nmFunSuff n ++ "_"
nmEvalN  n  = "_e" ++ nmFunSuff n ++ "_"
nmEval      = nmEvalN (-1)
nmFunN   n  = mkHNm ("_F" {- ++ nmFunSuff n -} ++ "_")
nmAppN   n  = "_A" {- ++ nmFunSuff n -} ++ "_"
nmTag       = mkHNm "_tag_"
nmInd       = mkHNm "_i_"
nmIndSet    = mkHNm "_i_set_"
nmSwitchRes = mkHNm "_sw"



hsnJavaScriptVar :: Bool -> HsName -> HsName -> HsName -> HsName
hsnJavaScriptVar isGlobal = hsnJavaLikeVar (hsn, hsnPrefix "$", ('$':))
  where hsn | isGlobal  = id
            | otherwise = hsnQualified



type CVarInfo = CVarInfo' () HsName Int
type CVarMp   = CVarMp'   () HsName Int



tyDefault = ()



jvRef :: CVarMp -> CVarInfo -> J.Expr
jvRef
  = cvarToRef
      ( \_       -> J.Expr_This
      , \_ o     -> jsVar o
      , \_ _ f   -> jsVar f
      , \_ e _ f -> J.Expr_ObjFld e (mkHNm f)
      , \  e o   -> J.Expr_ArrInx e o
      , jsIntConst
      )



-- | tracing
jsTr :: PP x => String -> x -> Seq.Seq J.Stat
jsTr m x = Seq.singleton $ J.Stat_Expr $ J.Expr_Call (jsVar $ mkHNm "trace") [J.Expr_Str m, J.Expr_Inline $ showPP $ pp x]

-- constant
jsIntConst :: Integral x => x -> J.Expr
jsIntConst i = J.Expr_Int $ fromIntegral i

-- var
jsVar :: HSNM x => x -> J.Expr
jsVar nm = J.Expr_Var $ mkHNm nm

-- call
jsCall :: HSNM n => n -> [J.Expr] -> J.Expr
jsCall f as = J.Expr_Call (jsVar $ mkHNm f) as

-- apply
jsApp :: J.Expr -> [J.Expr] -> J.Expr
jsApp f as
  = J.Expr_New $ jsCall nm (f : as')
  where (nm,_,as') = jsArgsPack as
        nArgs      = length as'

-- lam
jsFun :: HsName -> [HsName] -> [J.Stat] -> J.Expr
jsFun fNm as stats
  = J.Expr_New $ jsCall (nmFunN $ length as) (extra ++ [J.Expr_Fun Nothing as stat])
  where stat = J.Stat_Block stats
        extra = []

-- force evaluation
jsEvl :: J.Expr -> J.Expr
jsEvl x = jsCall nmEval [x]

-- assign
jsAssign :: HSNM x => x -> J.Expr -> J.Stat
jsAssign n e = J.Stat_Assign (jsVar $ mkHNm n) e

-- new tuple
jsNewTup :: [J.Expr] -> J.Expr
jsNewTup = J.Expr_Arr

-- | field names used for data constructors, either as provided by program, or made up here
--   20101012 AD, note: internally generated datatypes not yet have correct meta info, so fill up names as needed, as temporary hack
jsDataFldNames :: DataGam -> CTag -> [HsName]
jsDataFldNames dataGam ctag
  = zipWith (\o mbn -> maybe o (hsnSafeJavaLike . hsnQualified) mbn) hsnLclSupply $ nms ++ fill
  where nms  = maybe [] (\(_,dti) -> map fst $ dtiFldTyL dti) $ dataGamTagLookup ctag dataGam
        fill = repeat Nothing

-- either new data constructor or tuple
jsNewTupOrData :: DataGam -> HsName -> HsName -> CTag -> [J.Expr] -> J.Expr
jsNewTupOrData dataGam _ _ ctag as
  = case ctag of
      CTag _ _ t _ _ -> J.Expr_Obj $ ((nmTag,jsIntConst t):)
                                    $ zip (jsDataFldNames dataGam ctag) as
      CTagRec        -> jsNewTup as

-- | body
jsBody :: (J.Expr -> J.Stat) -> Seq.Seq JBind -> Seq.Seq J.Stat -> Maybe J.Expr -> [J.Stat]
jsBody mkRet binds stats lastExpr
  = Seq.toList $
                Seq.map (\(JBind _ n _ e _) -> J.jsVarDecl n e) binds
    `Seq.union` stats
    `Seq.union` Seq.fromList (map mkRet $ maybeToList lastExpr)



-- pack > 5 args into tuple, otherwise normal
jsArgsPack :: [J.Expr] -> (String,(),[J.Expr])
jsArgsPack = javalikeArgsPack (-1) ((),(),J.Expr_Arr,nmAppN)



-- unpack > 5 args from tuple, otherwise normal
jsArgsUnpack :: ([HsName]->[HsName]) -> [HsName] -> ([(HsName,())],[(J.Expr,())],[(HsName,CVarInfo)])
jsArgsUnpack toRef = javalikeArgsUnpack maxBound ((), (), (), toRef, jsIntConst, \_ a -> a, \n -> take n hsnLclSupply)



type JBind  = JBind'  () J.Expr ()
type JBinds = JBinds' () J.Expr ()



jBind :: HsName -> HsName -> J.Expr -> JBinds
jBind = jBind' (tyDefault, id, const ())



-- | construct the ffi call
ffiJavaScriptMkCall
  ::  Ty                          -- type of the imported function
  ->  ForeignExtraction
  ->  EHCOpts
  ->  Bool                        -- do eval of args
  ->  [Maybe HsName]              -- list of (possibly) type constructor names of arguments
  ->  Maybe HsName                -- and result
  ->  ( [J.Expr -> J.Expr]        -- additional unwrapping for each argument
      ,  J.Expr -> J.Expr         -- and result
      , [J.Expr] -> J.Expr        -- and primitive call itself
      )
ffiJavaScriptMkCall ty
     ent
  = javalikeMkFFICall
      ( const tyDefault
      , \_ _ -> bx
      , \_ -> bx
      , \_ _ impExtract
          -> case impExtract of
               ForeignExtraction_Plain {forextractEnt = impEntNm}
                 -> jsVar $ mkHNm impEntNm
               ForeignExtraction_Wrapper
                 -> panic "ffiJavaScriptMkCall.mkPrimFun: wrapper not implemented"
               ForeignExtraction_Dynamic
                 -> panic "ffiJavaScriptMkCall.mkPrimFun: dynamic not implemented"
      , \_ _ -> mk
      , mkWrap
      , mkDyn
      , jsEvl, tyDefault
      )
      ent
  where bx = (id,tyDefault)
        mkfargs f as -- TODO: Is this redundant?
          | isJust mbArgThis = (J.Expr_Sel this f, as')
          | otherwise        = (f,as)
          where mbArgThis@(~(Just nr)) = forextractMbThisArgNr ent
                this = as !! inx
                inx = nr-1
                as' = take inx as ++ drop nr as
        mkObj as = jsCall "primToPlainObj" as
        mk = foreignexprEval (J.Expr_Sel,J.Expr_ArrInx,J.Expr_Call,id,mkObj,J.Expr_New,jsVar,J.Expr_Str) (forextractForeignExpr ent)
        -- TODO: Reuse some of the foreign export code for this? It's more or less generating the same code...
        -- TODO: Document that we require callback functions to be in IO
        mkWrap n (e:_) = mkWrapFn as bdy
          where as   = map (mkHNm . ('v':) . show) [1..(tyNumArgs ty - 1)]
                rn   = mkHNm "res"
                bdy  = [ J.jsVarDecl rn $ jsEvl $ jsApp e $ (map jsVar as) ++ [J.Expr_Arr []]
                       , J.Stat_Ret $ jsEvl $ J.Expr_ArrInx (jsVar rn) (jsIntConst 1) ]

        mkDyn _ (e:es) = J.Expr_Call e es
        mkcall (f,as) -- TODO: Is this redundant?
          | forextractOptIsPtr ent = f
          | otherwise              = J.Expr_Call f as



tyNumArgs :: Ty -> Int
tyNumArgs (Ty_App t@(Ty_App {}) arg)             = 1 + tyNumArgs t + tyNumArgs arg
tyNumArgs (Ty_App _ (Ty_App t@(Ty_App {}) arg))  = 1 + tyNumArgs t + tyNumArgs arg
tyNumArgs _                                      = 0

mkWrapFn :: [HsName] -> [J.Stat] -> J.Expr
mkWrapFn as bdy = J.Expr_Fun Nothing as $ J.Stat_Block bdy

-- CAlt --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         scrutinees           : [Scrutinee]
   visit 1:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attribute:
         altsJsL              : [(Int,Seq.Seq J.Stat,J.Expr)]
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 1:
            local whatAbove   : {WhatExpr}
            local mbLamNm     : _
            local lev         : _
            local cvarMpOffsets : _
            local scrutineeTag : _
            local altsJsL     : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = ( ([Scrutinee]),T_CAlt_1 )
type T_CAlt_1  = CVarMp ->
                 DataGam ->
                 EvalCtx ->
                 UID ->
                 Bool ->
                 Bool ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 HsName ->
                 CVarInfo ->
                 HsName ->
                 ( ([(Int,Seq.Seq J.Stat,J.Expr)]),UID)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (case (pat_ ) of
     { ( _patIscrutinees,pat_1) ->
         (case (_patIscrutinees) of
          { _lhsOscrutinees ->
          (case ((let sem_CAlt_Alt_1 :: T_CAlt_1 
                      sem_CAlt_Alt_1  =
                          (\ _lhsIcvarMp
                             _lhsIdataGam
                             _lhsIevalCtx
                             _lhsIgUniq
                             _lhsIisLamBody
                             _lhsIisStrict
                             _lhsIlev
                             _lhsImoduleClassNm
                             _lhsIopts
                             _lhsIpkgNm
                             _lhsIscrutineeCVarInfo
                             _lhsItopClassNm ->
                               (case (ExprIsOther) of
                                { _whatAbove ->
                                (case (_whatAbove) of
                                 { _exprOwhatAbove ->
                                 (case (_lhsItopClassNm) of
                                  { _exprOtopClassNm ->
                                  (case (_lhsIpkgNm) of
                                   { _exprOpkgNm ->
                                   (case (_lhsIopts) of
                                    { _exprOopts ->
                                    (case (_lhsImoduleClassNm) of
                                     { _exprOmoduleClassNm ->
                                     (case (Nothing) of
                                      { _mbLamNm ->
                                      (case (_mbLamNm) of
                                       { _exprOmbLamNm ->
                                       (case (_lhsIlev + 1) of
                                        { _lev ->
                                        (case (_lev) of
                                         { _exprOlev ->
                                         (case (_lhsIgUniq) of
                                          { _patOgUniq ->
                                          (case (_lhsItopClassNm) of
                                           { _patOtopClassNm ->
                                           (case (_lhsIscrutineeCVarInfo) of
                                            { _patOscrutineeCVarInfo ->
                                            (case (_lhsIpkgNm) of
                                             { _patOpkgNm ->
                                             (case (_lhsIopts) of
                                              { _patOopts ->
                                              (case (_lhsImoduleClassNm) of
                                               { _patOmoduleClassNm ->
                                               (case (_lev) of
                                                { _patOlev ->
                                                (case (_lhsIdataGam) of
                                                 { _patOdataGam ->
                                                 (case (_lhsIcvarMp) of
                                                  { _patOcvarMp ->
                                                  (case (pat_1 _patOcvarMp _patOdataGam _patOgUniq _patOlev _patOmoduleClassNm _patOopts _patOpkgNm _patOscrutineeCVarInfo _patOtopClassNm ) of
                                                   { ( _patIfldNmL,_patIgUniq,_patIoffsetBinds,_patIpatCVarMp) ->
                                                       (case (_patIgUniq) of
                                                        { _exprOgUniq ->
                                                        (case (_lhsIevalCtx) of
                                                         { _exprOevalCtx ->
                                                         (case (_lhsIdataGam) of
                                                          { _exprOdataGam ->
                                                          (case (Map.fromList [ (n,cvi) | (n,cvi,_) <- _patIoffsetBinds ]) of
                                                           { _cvarMpOffsets ->
                                                           (case (Map.unions [_cvarMpOffsets, _patIpatCVarMp, _lhsIcvarMp]) of
                                                            { _exprOcvarMp ->
                                                            (case (case _patIscrutinees of
                                                                     (Scrutinee_Tag (CTag _ cn tag _ _) : _)
                                                                       -> ( tag
                                                                          )
                                                                     (Scrutinee_Int i : _)
                                                                       -> ( i
                                                                          )
                                                                     _ -> (0)) of
                                                             { _scrutineeTag ->
                                                             (case (expr_ ) of
                                                              { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                                                                  (case (_lhsIisStrict) of
                                                                   { _exprOisStrict ->
                                                                   (case (_lhsIisLamBody) of
                                                                    { _exprOisLamBody ->
                                                                    (case (True) of
                                                                     { _exprOisTopTup ->
                                                                     (case (True) of
                                                                      { _exprOisTopApp ->
                                                                      (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                                                       { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                                           (case ([(_scrutineeTag,_exprIjstats,_exprIjs)]) of
                                                                            { _altsJsL ->
                                                                            (case (_altsJsL) of
                                                                             { _lhsOaltsJsL ->
                                                                             (case (_exprIgUniq) of
                                                                              { _lhsOgUniq ->
                                                                              ( _lhsOaltsJsL,_lhsOgUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                  in  sem_CAlt_Alt_1)) of
           { ( sem_CAlt_1) ->
           ( _lhsOscrutinees,sem_CAlt_1) }) }) })
-- CAltL -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         scrutinees           : [Scrutinee]
   visit 1:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attribute:
         altsJsL              : [(Int,Seq.Seq J.Stat,J.Expr)]
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
      alternative Nil:
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = ( ([Scrutinee]),T_CAltL_1 )
type T_CAltL_1  = CVarMp ->
                  DataGam ->
                  EvalCtx ->
                  UID ->
                  Bool ->
                  Bool ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  HsName ->
                  CVarInfo ->
                  HsName ->
                  ( ([(Int,Seq.Seq J.Stat,J.Expr)]),UID)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIscrutinees,tl_1) ->
         (case (hd_ ) of
          { ( _hdIscrutinees,hd_1) ->
              (case (_hdIscrutinees ++ _tlIscrutinees) of
               { _lhsOscrutinees ->
               (case ((let sem_CAltL_Cons_1 :: T_CAltL_1 
                           sem_CAltL_Cons_1  =
                               (\ _lhsIcvarMp
                                  _lhsIdataGam
                                  _lhsIevalCtx
                                  _lhsIgUniq
                                  _lhsIisLamBody
                                  _lhsIisStrict
                                  _lhsIlev
                                  _lhsImoduleClassNm
                                  _lhsIopts
                                  _lhsIpkgNm
                                  _lhsIscrutineeCVarInfo
                                  _lhsItopClassNm ->
                                    (case (_lhsItopClassNm) of
                                     { _tlOtopClassNm ->
                                     (case (_lhsIscrutineeCVarInfo) of
                                      { _tlOscrutineeCVarInfo ->
                                      (case (_lhsIpkgNm) of
                                       { _tlOpkgNm ->
                                       (case (_lhsIopts) of
                                        { _tlOopts ->
                                        (case (_lhsImoduleClassNm) of
                                         { _tlOmoduleClassNm ->
                                         (case (_lhsIlev) of
                                          { _tlOlev ->
                                          (case (_lhsIgUniq) of
                                           { _hdOgUniq ->
                                           (case (_lhsItopClassNm) of
                                            { _hdOtopClassNm ->
                                            (case (_lhsIscrutineeCVarInfo) of
                                             { _hdOscrutineeCVarInfo ->
                                             (case (_lhsIpkgNm) of
                                              { _hdOpkgNm ->
                                              (case (_lhsIopts) of
                                               { _hdOopts ->
                                               (case (_lhsImoduleClassNm) of
                                                { _hdOmoduleClassNm ->
                                                (case (_lhsIlev) of
                                                 { _hdOlev ->
                                                 (case (_lhsIisStrict) of
                                                  { _hdOisStrict ->
                                                  (case (_lhsIisLamBody) of
                                                   { _hdOisLamBody ->
                                                   (case (_lhsIevalCtx) of
                                                    { _hdOevalCtx ->
                                                    (case (_lhsIdataGam) of
                                                     { _hdOdataGam ->
                                                     (case (_lhsIcvarMp) of
                                                      { _hdOcvarMp ->
                                                      (case (hd_1 _hdOcvarMp _hdOdataGam _hdOevalCtx _hdOgUniq _hdOisLamBody _hdOisStrict _hdOlev _hdOmoduleClassNm _hdOopts _hdOpkgNm _hdOscrutineeCVarInfo _hdOtopClassNm ) of
                                                       { ( _hdIaltsJsL,_hdIgUniq) ->
                                                           (case (_hdIgUniq) of
                                                            { _tlOgUniq ->
                                                            (case (_lhsIevalCtx) of
                                                             { _tlOevalCtx ->
                                                             (case (_lhsIdataGam) of
                                                              { _tlOdataGam ->
                                                              (case (_lhsIcvarMp) of
                                                               { _tlOcvarMp ->
                                                               (case (_lhsIisStrict) of
                                                                { _tlOisStrict ->
                                                                (case (_lhsIisLamBody) of
                                                                 { _tlOisLamBody ->
                                                                 (case (tl_1 _tlOcvarMp _tlOdataGam _tlOevalCtx _tlOgUniq _tlOisLamBody _tlOisStrict _tlOlev _tlOmoduleClassNm _tlOopts _tlOpkgNm _tlOscrutineeCVarInfo _tlOtopClassNm ) of
                                                                  { ( _tlIaltsJsL,_tlIgUniq) ->
                                                                      (case (_hdIaltsJsL ++ _tlIaltsJsL) of
                                                                       { _lhsOaltsJsL ->
                                                                       (case (_tlIgUniq) of
                                                                        { _lhsOgUniq ->
                                                                        ( _lhsOaltsJsL,_lhsOgUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                       in  sem_CAltL_Cons_1)) of
                { ( sem_CAltL_1) ->
                ( _lhsOscrutinees,sem_CAltL_1) }) }) }) })
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (case ([]) of
     { _lhsOscrutinees ->
     (case ((let sem_CAltL_Nil_1 :: T_CAltL_1 
                 sem_CAltL_Nil_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIevalCtx
                        _lhsIgUniq
                        _lhsIisLamBody
                        _lhsIisStrict
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsIscrutineeCVarInfo
                        _lhsItopClassNm ->
                          (case ([]) of
                           { _lhsOaltsJsL ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            ( _lhsOaltsJsL,_lhsOgUniq) }) }))
             in  sem_CAltL_Nil_1)) of
      { ( sem_CAltL_1) ->
      ( _lhsOscrutinees,sem_CAltL_1) }) })
-- CBind -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         bindNmL              : [HsName]
         nm                   : HsName
   visit 1:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         jbinds               : JBinds
         usedModNmS           : FvS
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local bindNmL     : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = ( ([HsName]),HsName,T_CBind_1 )
type T_CBind_1  = CVarMp ->
                  DataGam ->
                  EvalCtx ->
                  UID ->
                  Bool ->
                  Bool ->
                  Bool ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  HsName ->
                  HsName ->
                  ( UID,JBinds,FvS)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (case ([nm_]) of
     { _bindNmL ->
     (case (_bindNmL) of
      { _lhsObindNmL ->
      (case (nm_) of
       { _lhsOnm ->
       (case ((let sem_CBind_Bind_1 :: T_CBind_1 
                   sem_CBind_Bind_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisGlobal
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIletBindingsCateg
                          _lhsIlev
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm ->
                            (case (_lhsIgUniq) of
                             { _bindAspectsOgUniq ->
                             (case (_lhsItopClassNm) of
                              { _bindAspectsOtopClassNm ->
                              (case (_lhsIpkgNm) of
                               { _bindAspectsOpkgNm ->
                               (case (_lhsIopts) of
                                { _bindAspectsOopts ->
                                (case (_lhsImoduleClassNm) of
                                 { _bindAspectsOmoduleClassNm ->
                                 (case (_lhsIlev) of
                                  { _bindAspectsOlev ->
                                  (case (_lhsIletBindingsCateg) of
                                   { _bindAspectsOletBindingsCateg ->
                                   (case (_lhsIisStrict) of
                                    { _bindAspectsOisStrict ->
                                    (case (_lhsIisLamBody) of
                                     { _bindAspectsOisLamBody ->
                                     (case (_lhsIisGlobal) of
                                      { _bindAspectsOisGlobal ->
                                      (case (_lhsIevalCtx) of
                                       { _bindAspectsOevalCtx ->
                                       (case (_lhsIdataGam) of
                                        { _bindAspectsOdataGam ->
                                        (case (_lhsIcvarMp) of
                                         { _bindAspectsOcvarMp ->
                                         (case (nm_) of
                                          { _bindAspectsOnm ->
                                          (case (bindAspects_ _bindAspectsOcvarMp _bindAspectsOdataGam _bindAspectsOevalCtx _bindAspectsOgUniq _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOmoduleClassNm _bindAspectsOnm _bindAspectsOopts _bindAspectsOpkgNm _bindAspectsOtopClassNm ) of
                                           { ( _bindAspectsIbindNmL,_bindAspectsIgUniq,_bindAspectsIjbinds,_bindAspectsIusedModNmS) ->
                                               (case (_bindAspectsIgUniq) of
                                                { _lhsOgUniq ->
                                                (case (_bindAspectsIjbinds) of
                                                 { _lhsOjbinds ->
                                                 (case (_bindAspectsIusedModNmS) of
                                                  { _lhsOusedModNmS ->
                                                  ( _lhsOgUniq,_lhsOjbinds,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CBind_Bind_1)) of
        { ( sem_CBind_1) ->
        ( _lhsObindNmL,_lhsOnm,sem_CBind_1) }) }) }) })
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ctag                 : CTag
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attributes:
         dataFldNmL           : [HsName]
         gUniq                : UID
      synthesized attributes:
         offsetBinds          : [(HsName,CVarInfo,J.Expr)]
         patCVarMp            : CVarMp
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = CTag ->
                   CVarMp ->
                   ([HsName]) ->
                   DataGam ->
                   UID ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   HsName ->
                   CVarInfo ->
                   HsName ->
                   ( ([HsName]),UID,([(HsName,CVarInfo,J.Expr)]),CVarMp)
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIctag
       _lhsIcvarMp
       _lhsIdataFldNmL
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIdataFldNmL) of
          { _lhsOdataFldNmL ->
          (case (_lhsIgUniq) of
           { _lhsOgUniq ->
           (case ([]) of
            { _lhsOoffsetBinds ->
            (case (Map.empty) of
             { _lhsOpatCVarMp ->
             ( _lhsOdataFldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ctag                 : CTag
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attributes:
         dataFldNmL           : [HsName]
         gUniq                : UID
      synthesized attributes:
         offsetBinds          : [(HsName,CVarInfo,J.Expr)]
         patCVarMp            : CVarMp
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
      alternative Nil:
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = CTag ->
                    CVarMp ->
                    ([HsName]) ->
                    DataGam ->
                    UID ->
                    Int ->
                    HsName ->
                    EHCOpts ->
                    HsName ->
                    CVarInfo ->
                    HsName ->
                    ( ([HsName]),UID,([(HsName,CVarInfo,J.Expr)]),CVarMp)
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIctag
       _lhsIcvarMp
       _lhsIdataFldNmL
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIdataFldNmL) of
          { _hdOdataFldNmL ->
          (case (_lhsItopClassNm) of
           { _hdOtopClassNm ->
           (case (_lhsIscrutineeCVarInfo) of
            { _hdOscrutineeCVarInfo ->
            (case (_lhsIpkgNm) of
             { _hdOpkgNm ->
             (case (_lhsIopts) of
              { _hdOopts ->
              (case (_lhsImoduleClassNm) of
               { _hdOmoduleClassNm ->
               (case (_lhsIlev) of
                { _hdOlev ->
                (case (_lhsIgUniq) of
                 { _hdOgUniq ->
                 (case (_lhsIdataGam) of
                  { _hdOdataGam ->
                  (case (_lhsIcvarMp) of
                   { _hdOcvarMp ->
                   (case (_lhsIctag) of
                    { _hdOctag ->
                    (case (hd_ _hdOctag _hdOcvarMp _hdOdataFldNmL _hdOdataGam _hdOgUniq _hdOlev _hdOmoduleClassNm _hdOopts _hdOpkgNm _hdOscrutineeCVarInfo _hdOtopClassNm ) of
                     { ( _hdIdataFldNmL,_hdIgUniq,_hdIoffsetBinds,_hdIpatCVarMp) ->
                         (case (_hdIdataFldNmL) of
                          { _tlOdataFldNmL ->
                          (case (_lhsItopClassNm) of
                           { _tlOtopClassNm ->
                           (case (_lhsIscrutineeCVarInfo) of
                            { _tlOscrutineeCVarInfo ->
                            (case (_lhsIpkgNm) of
                             { _tlOpkgNm ->
                             (case (_lhsIopts) of
                              { _tlOopts ->
                              (case (_lhsImoduleClassNm) of
                               { _tlOmoduleClassNm ->
                               (case (_lhsIlev) of
                                { _tlOlev ->
                                (case (_hdIgUniq) of
                                 { _tlOgUniq ->
                                 (case (_lhsIdataGam) of
                                  { _tlOdataGam ->
                                  (case (_lhsIcvarMp) of
                                   { _tlOcvarMp ->
                                   (case (_lhsIctag) of
                                    { _tlOctag ->
                                    (case (tl_ _tlOctag _tlOcvarMp _tlOdataFldNmL _tlOdataGam _tlOgUniq _tlOlev _tlOmoduleClassNm _tlOopts _tlOpkgNm _tlOscrutineeCVarInfo _tlOtopClassNm ) of
                                     { ( _tlIdataFldNmL,_tlIgUniq,_tlIoffsetBinds,_tlIpatCVarMp) ->
                                         (case (_tlIdataFldNmL) of
                                          { _lhsOdataFldNmL ->
                                          (case (_tlIgUniq) of
                                           { _lhsOgUniq ->
                                           (case (_hdIoffsetBinds ++ _tlIoffsetBinds) of
                                            { _lhsOoffsetBinds ->
                                            (case (_hdIpatCVarMp `Map.union` _tlIpatCVarMp) of
                                             { _lhsOpatCVarMp ->
                                             ( _lhsOdataFldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIctag
       _lhsIcvarMp
       _lhsIdataFldNmL
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIdataFldNmL) of
          { _lhsOdataFldNmL ->
          (case (_lhsIgUniq) of
           { _lhsOgUniq ->
           (case ([]) of
            { _lhsOoffsetBinds ->
            (case (Map.empty) of
             { _lhsOpatCVarMp ->
             ( _lhsOdataFldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         bindNmL              : [HsName]
   visit 1:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         jbinds               : JBinds
         usedModNmS           : FvS
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
      alternative Nil:
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = ( ([HsName]),T_CBindL_1 )
type T_CBindL_1  = CVarMp ->
                   DataGam ->
                   EvalCtx ->
                   UID ->
                   Bool ->
                   Bool ->
                   Bool ->
                   CBindCateg ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   HsName ->
                   HsName ->
                   ( UID,JBinds,FvS)
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIbindNmL,tl_1) ->
         (case (hd_ ) of
          { ( _hdIbindNmL,_hdInm,hd_1) ->
              (case (_hdIbindNmL ++ _tlIbindNmL) of
               { _lhsObindNmL ->
               (case ((let sem_CBindL_Cons_1 :: T_CBindL_1 
                           sem_CBindL_Cons_1  =
                               (\ _lhsIcvarMp
                                  _lhsIdataGam
                                  _lhsIevalCtx
                                  _lhsIgUniq
                                  _lhsIisGlobal
                                  _lhsIisLamBody
                                  _lhsIisStrict
                                  _lhsIletBindingsCateg
                                  _lhsIlev
                                  _lhsImoduleClassNm
                                  _lhsIopts
                                  _lhsIpkgNm
                                  _lhsItopClassNm ->
                                    (case (_lhsIgUniq) of
                                     { _hdOgUniq ->
                                     (case (_lhsItopClassNm) of
                                      { _hdOtopClassNm ->
                                      (case (_lhsIpkgNm) of
                                       { _hdOpkgNm ->
                                       (case (_lhsIopts) of
                                        { _hdOopts ->
                                        (case (_lhsImoduleClassNm) of
                                         { _hdOmoduleClassNm ->
                                         (case (_lhsIlev) of
                                          { _hdOlev ->
                                          (case (_lhsIletBindingsCateg) of
                                           { _hdOletBindingsCateg ->
                                           (case (_lhsIisStrict) of
                                            { _hdOisStrict ->
                                            (case (_lhsIisLamBody) of
                                             { _hdOisLamBody ->
                                             (case (_lhsIisGlobal) of
                                              { _hdOisGlobal ->
                                              (case (_lhsIevalCtx) of
                                               { _hdOevalCtx ->
                                               (case (_lhsIdataGam) of
                                                { _hdOdataGam ->
                                                (case (_lhsIcvarMp) of
                                                 { _hdOcvarMp ->
                                                 (case (hd_1 _hdOcvarMp _hdOdataGam _hdOevalCtx _hdOgUniq _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOletBindingsCateg _hdOlev _hdOmoduleClassNm _hdOopts _hdOpkgNm _hdOtopClassNm ) of
                                                  { ( _hdIgUniq,_hdIjbinds,_hdIusedModNmS) ->
                                                      (case (_hdIgUniq) of
                                                       { _tlOgUniq ->
                                                       (case (_lhsItopClassNm) of
                                                        { _tlOtopClassNm ->
                                                        (case (_lhsIpkgNm) of
                                                         { _tlOpkgNm ->
                                                         (case (_lhsIopts) of
                                                          { _tlOopts ->
                                                          (case (_lhsImoduleClassNm) of
                                                           { _tlOmoduleClassNm ->
                                                           (case (_lhsIlev) of
                                                            { _tlOlev ->
                                                            (case (_lhsIletBindingsCateg) of
                                                             { _tlOletBindingsCateg ->
                                                             (case (_lhsIisStrict) of
                                                              { _tlOisStrict ->
                                                              (case (_lhsIisLamBody) of
                                                               { _tlOisLamBody ->
                                                               (case (_lhsIisGlobal) of
                                                                { _tlOisGlobal ->
                                                                (case (_lhsIevalCtx) of
                                                                 { _tlOevalCtx ->
                                                                 (case (_lhsIdataGam) of
                                                                  { _tlOdataGam ->
                                                                  (case (_lhsIcvarMp) of
                                                                   { _tlOcvarMp ->
                                                                   (case (tl_1 _tlOcvarMp _tlOdataGam _tlOevalCtx _tlOgUniq _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOmoduleClassNm _tlOopts _tlOpkgNm _tlOtopClassNm ) of
                                                                    { ( _tlIgUniq,_tlIjbinds,_tlIusedModNmS) ->
                                                                        (case (_tlIgUniq) of
                                                                         { _lhsOgUniq ->
                                                                         (case (_hdIjbinds `Seq.union` _tlIjbinds) of
                                                                          { _lhsOjbinds ->
                                                                          (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
                                                                           { _lhsOusedModNmS ->
                                                                           ( _lhsOgUniq,_lhsOjbinds,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                       in  sem_CBindL_Cons_1)) of
                { ( sem_CBindL_1) ->
                ( _lhsObindNmL,sem_CBindL_1) }) }) }) })
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (case ([]) of
     { _lhsObindNmL ->
     (case ((let sem_CBindL_Nil_1 :: T_CBindL_1 
                 sem_CBindL_Nil_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIevalCtx
                        _lhsIgUniq
                        _lhsIisGlobal
                        _lhsIisLamBody
                        _lhsIisStrict
                        _lhsIletBindingsCateg
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsItopClassNm ->
                          (case (_lhsIgUniq) of
                           { _lhsOgUniq ->
                           (case (Seq.empty) of
                            { _lhsOjbinds ->
                            (case (Set.empty) of
                             { _lhsOusedModNmS ->
                             ( _lhsOgUniq,_lhsOjbinds,_lhsOusedModNmS) }) }) }))
             in  sem_CBindL_Nil_1)) of
      { ( sem_CBindL_1) ->
      ( _lhsObindNmL,sem_CBindL_1) }) })
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         mbLamNm              : Maybe (HsName,HsName)
         moduleClassNm        : HsName
         nm                   : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         bindNmL              : [HsName]
         jbinds               : JBinds
         js                   : J.Expr
         usedModNmS           : FvS
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
            local varnm       : {HsName}
            local isCAF       : _
            local js          : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local whatAbove   : {WhatExpr}
            local varnm       : {HsName}
            local foreignEntInfo : _
            local expEntNm    : _
            local jsArgL      : _
            local js          : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local js          : _
            local varnm       : {HsName}
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local js          : _
            local varnm       : {HsName}
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local js          : _
            local varnm       : {HsName}
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
            local varnm       : {HsName}
-}
-- cata
sem_CBound :: CBound  ->
              T_CBound 
sem_CBound (CBound_Bind _bindMeta _expr )  =
    (sem_CBound_Bind (sem_CMetas _bindMeta ) (sem_CExpr _expr ) )
sem_CBound (CBound_FFE _callconv _expEnt _expr _ty )  =
    (sem_CBound_FFE _callconv _expEnt (sem_CExpr _expr ) _ty )
sem_CBound (CBound_Meta _aspectKeyS _cmetas )  =
    (sem_CBound_Meta _aspectKeyS (sem_CMetas _cmetas ) )
sem_CBound (CBound_RelevTy _aspectKeyS _relevTy )  =
    (sem_CBound_RelevTy _aspectKeyS _relevTy )
sem_CBound (CBound_Ty _aspectKeyS _ty )  =
    (sem_CBound_Ty _aspectKeyS _ty )
sem_CBound (CBound_Val _aspectKeyS _expr )  =
    (sem_CBound_Val _aspectKeyS (sem_CExpr _expr ) )
-- semantic domain
type T_CBound  = CVarMp ->
                 DataGam ->
                 EvalCtx ->
                 UID ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 CBindCateg ->
                 Int ->
                 (Maybe (HsName,HsName)) ->
                 HsName ->
                 HsName ->
                 EHCOpts ->
                 HsName ->
                 HsName ->
                 ( ([HsName]),UID,JBinds,(J.Expr),FvS)
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _bindMetaOgUniq ->
           (case (_lhsItopClassNm) of
            { _bindMetaOtopClassNm ->
            (case (_lhsIpkgNm) of
             { _bindMetaOpkgNm ->
             (case (_lhsIopts) of
              { _bindMetaOopts ->
              (case (_lhsImoduleClassNm) of
               { _bindMetaOmoduleClassNm ->
               (case (_lhsIlev) of
                { _bindMetaOlev ->
                (case (_lhsIdataGam) of
                 { _bindMetaOdataGam ->
                 (case (_lhsIcvarMp) of
                  { _bindMetaOcvarMp ->
                  (case (bindMeta_ _bindMetaOcvarMp _bindMetaOdataGam _bindMetaOgUniq _bindMetaOlev _bindMetaOmoduleClassNm _bindMetaOopts _bindMetaOpkgNm _bindMetaOtopClassNm ) of
                   { ( _bindMetaIgUniq) ->
                       (case (_bindMetaIgUniq) of
                        { _exprOgUniq ->
                        (case (expr_ ) of
                         { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                             (case (ExprIsBind) of
                              { _whatAbove ->
                              (case (_whatAbove) of
                               { _exprOwhatAbove ->
                               (case (_lhsItopClassNm) of
                                { _exprOtopClassNm ->
                                (case (_lhsIpkgNm) of
                                 { _exprOpkgNm ->
                                 (case (_lhsIopts) of
                                  { _exprOopts ->
                                  (case (_lhsImoduleClassNm) of
                                   { _exprOmoduleClassNm ->
                                   (case (_lhsIlev) of
                                    { _exprOlev ->
                                    (case (_lhsIisLamBody) of
                                     { _exprOisLamBody ->
                                     (case (_lhsIevalCtx) of
                                      { _exprOevalCtx ->
                                      (case (_lhsIdataGam) of
                                       { _exprOdataGam ->
                                       (case (_lhsIcvarMp) of
                                        { _exprOcvarMp ->
                                        (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                                         { _exprOisStrict ->
                                         (case (True) of
                                          { _exprOisTopTup ->
                                          (case (True) of
                                           { _exprOisTopApp ->
                                           (case (hsnJavaScriptVar _lhsIisGlobal _lhsIpkgNm _lhsItopClassNm _lhsInm) of
                                            { _varnm ->
                                            (case (Just (_varnm,_lhsInm)) of
                                             { _exprOmbLamNm ->
                                             (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                              { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                  (case (_exprIgUniq) of
                                                   { _lhsOgUniq ->
                                                   (case (_exprIwhatBelow /= ExprIsLam) of
                                                    { _isCAF ->
                                                    (case (let str  = if _lhsIevalCtx == EvalCtx_Eval then jsEvl else id
                                                               dflt = _exprIjs
                                                               caf  = jsApp (jsFun _lhsInm [] $ jsBody J.Stat_Ret _exprIjbinds _exprIjstats (Just dflt)) []
                                                           in  if _lhsIisGlobal
                                                               then if _isCAF
                                                                    then caf
                                                                    else dflt
                                                               else if Seq.null _exprIjstats
                                                                    then str dflt
                                                                    else str caf) of
                                                     { _js ->
                                                     (case (jBind _lhsInm _varnm _js) of
                                                      { _lhsOjbinds ->
                                                      (case (_js) of
                                                       { _lhsOjs ->
                                                       (case (maybe Set.empty Set.singleton $ hsnQualifier _varnm) of
                                                        { _lhsOusedModNmS ->
                                                        ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _exprOgUniq ->
           (case (expr_ ) of
            { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                (case (ExprIsLam) of
                 { _whatAbove ->
                 (case (_whatAbove) of
                  { _exprOwhatAbove ->
                  (case (_lhsItopClassNm) of
                   { _exprOtopClassNm ->
                   (case (_lhsIpkgNm) of
                    { _exprOpkgNm ->
                    (case (_lhsIopts) of
                     { _exprOopts ->
                     (case (_lhsImoduleClassNm) of
                      { _exprOmoduleClassNm ->
                      (case (_lhsIlev) of
                       { _exprOlev ->
                       (case (_lhsIisLamBody) of
                        { _exprOisLamBody ->
                        (case (_lhsIevalCtx) of
                         { _exprOevalCtx ->
                         (case (_lhsIdataGam) of
                          { _exprOdataGam ->
                          (case (_lhsIcvarMp) of
                           { _exprOcvarMp ->
                           (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                            { _exprOisStrict ->
                            (case (True) of
                             { _exprOisTopTup ->
                             (case (True) of
                              { _exprOisTopApp ->
                              (case (hsnJavaScriptVar _lhsIisGlobal _lhsIpkgNm _lhsItopClassNm _lhsInm) of
                               { _varnm ->
                               (case (Just (_varnm,_lhsInm)) of
                                { _exprOmbLamNm ->
                                (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                 { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                     (case (_exprIgUniq) of
                                      { _lhsOgUniq ->
                                      (case (foreignEntExtract expEnt_) of
                                       { _foreignEntInfo ->
                                       (case (forextractEnt _foreignEntInfo) of
                                        { _expEntNm ->
                                        (case (maybe [] (map (hsnJavaScriptVar False _lhsIpkgNm _lhsItopClassNm)) _exprImbLam) of
                                         { _jsArgL ->
                                         (case (mkWrapFn _jsArgL
                                                  $ jsBody J.Stat_Ret _exprIjbinds _exprIjstats (Just _exprIjsLamBody)) of
                                          { _js ->
                                          (case (let n = mkHNm _expEntNm
                                                 in  jBind n n _js) of
                                           { _lhsOjbinds ->
                                           (case (_js) of
                                            { _lhsOjs ->
                                            (case (maybe Set.empty Set.singleton $ hsnQualifier _varnm) of
                                             { _lhsOusedModNmS ->
                                             ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _cmetasOgUniq ->
           (case (_lhsItopClassNm) of
            { _cmetasOtopClassNm ->
            (case (_lhsIpkgNm) of
             { _cmetasOpkgNm ->
             (case (_lhsIopts) of
              { _cmetasOopts ->
              (case (_lhsImoduleClassNm) of
               { _cmetasOmoduleClassNm ->
               (case (_lhsIlev) of
                { _cmetasOlev ->
                (case (_lhsIdataGam) of
                 { _cmetasOdataGam ->
                 (case (_lhsIcvarMp) of
                  { _cmetasOcvarMp ->
                  (case (cmetas_ _cmetasOcvarMp _cmetasOdataGam _cmetasOgUniq _cmetasOlev _cmetasOmoduleClassNm _cmetasOopts _cmetasOpkgNm _cmetasOtopClassNm ) of
                   { ( _cmetasIgUniq) ->
                       (case (_cmetasIgUniq) of
                        { _lhsOgUniq ->
                        (case (Seq.empty) of
                         { _lhsOjbinds ->
                         (case (J.Expr_Str "*** ERR CBound ***") of
                          { _js ->
                          (case (_js) of
                           { _lhsOjs ->
                           (case (hsnJavaScriptVar _lhsIisGlobal _lhsIpkgNm _lhsItopClassNm _lhsInm) of
                            { _varnm ->
                            (case (maybe Set.empty Set.singleton $ hsnQualifier _varnm) of
                             { _lhsOusedModNmS ->
                             ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _lhsOgUniq ->
           (case (Seq.empty) of
            { _lhsOjbinds ->
            (case (J.Expr_Str "*** ERR CBound ***") of
             { _js ->
             (case (_js) of
              { _lhsOjs ->
              (case (hsnJavaScriptVar _lhsIisGlobal _lhsIpkgNm _lhsItopClassNm _lhsInm) of
               { _varnm ->
               (case (maybe Set.empty Set.singleton $ hsnQualifier _varnm) of
                { _lhsOusedModNmS ->
                ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOusedModNmS) }) }) }) }) }) }) }))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _lhsOgUniq ->
           (case (Seq.empty) of
            { _lhsOjbinds ->
            (case (J.Expr_Str "*** ERR CBound ***") of
             { _js ->
             (case (_js) of
              { _lhsOjs ->
              (case (hsnJavaScriptVar _lhsIisGlobal _lhsIpkgNm _lhsItopClassNm _lhsInm) of
               { _varnm ->
               (case (maybe Set.empty Set.singleton $ hsnQualifier _varnm) of
                { _lhsOusedModNmS ->
                ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOusedModNmS) }) }) }) }) }) }) }))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _exprOgUniq ->
           (case (expr_ ) of
            { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                (case (ExprIsBind) of
                 { _whatAbove ->
                 (case (_whatAbove) of
                  { _exprOwhatAbove ->
                  (case (_lhsItopClassNm) of
                   { _exprOtopClassNm ->
                   (case (_lhsIpkgNm) of
                    { _exprOpkgNm ->
                    (case (_lhsIopts) of
                     { _exprOopts ->
                     (case (_lhsImoduleClassNm) of
                      { _exprOmoduleClassNm ->
                      (case (_lhsIlev) of
                       { _exprOlev ->
                       (case (_lhsIisTopTup) of
                        { _exprOisTopTup ->
                        (case (_lhsIisTopApp) of
                         { _exprOisTopApp ->
                         (case (_lhsIisLamBody) of
                          { _exprOisLamBody ->
                          (case (_lhsIevalCtx) of
                           { _exprOevalCtx ->
                           (case (_lhsIdataGam) of
                            { _exprOdataGam ->
                            (case (_lhsIcvarMp) of
                             { _exprOcvarMp ->
                             (case (_lhsIisStrict || _exprIwhatBelow == ExprIsLam) of
                              { _exprOisStrict ->
                              (case (hsnJavaScriptVar _lhsIisGlobal _lhsIpkgNm _lhsItopClassNm _lhsInm) of
                               { _varnm ->
                               (case (Just (_varnm,_lhsInm)) of
                                { _exprOmbLamNm ->
                                (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                 { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                     (case (_exprIgUniq) of
                                      { _lhsOgUniq ->
                                      (case (_exprIjbinds) of
                                       { _lhsOjbinds ->
                                       (case (_exprIjs) of
                                        { _lhsOjs ->
                                        (case (maybe Set.empty Set.singleton $ hsnQualifier _varnm) of
                                         { _lhsOusedModNmS ->
                                         ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         moduleClassNm        : HsName
         nm                   : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         bindNmL              : [HsName]
         jbinds               : JBinds
         usedModNmS           : FvS
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
      alternative Nil:
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = CVarMp ->
                  DataGam ->
                  EvalCtx ->
                  UID ->
                  Bool ->
                  Bool ->
                  Bool ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  HsName ->
                  EHCOpts ->
                  HsName ->
                  HsName ->
                  ( ([HsName]),UID,JBinds,FvS)
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsItopClassNm) of
          { _tlOtopClassNm ->
          (case (_lhsIpkgNm) of
           { _tlOpkgNm ->
           (case (_lhsIopts) of
            { _tlOopts ->
            (case (_lhsInm) of
             { _tlOnm ->
             (case (_lhsImoduleClassNm) of
              { _tlOmoduleClassNm ->
              (case (_lhsIlev) of
               { _tlOlev ->
               (case (_lhsIletBindingsCateg) of
                { _tlOletBindingsCateg ->
                (case (_lhsIisStrict) of
                 { _tlOisStrict ->
                 (case (_lhsIisLamBody) of
                  { _tlOisLamBody ->
                  (case (_lhsIisGlobal) of
                   { _tlOisGlobal ->
                   (case (_lhsIgUniq) of
                    { _hdOgUniq ->
                    (case (_lhsItopClassNm) of
                     { _hdOtopClassNm ->
                     (case (_lhsIpkgNm) of
                      { _hdOpkgNm ->
                      (case (_lhsIopts) of
                       { _hdOopts ->
                       (case (_lhsInm) of
                        { _hdOnm ->
                        (case (_lhsImoduleClassNm) of
                         { _hdOmoduleClassNm ->
                         (case (_lhsIlev) of
                          { _hdOlev ->
                          (case (_lhsIletBindingsCateg) of
                           { _hdOletBindingsCateg ->
                           (case (_lhsIisStrict) of
                            { _hdOisStrict ->
                            (case (_lhsIisLamBody) of
                             { _hdOisLamBody ->
                             (case (_lhsIisGlobal) of
                              { _hdOisGlobal ->
                              (case (_lhsIevalCtx) of
                               { _hdOevalCtx ->
                               (case (_lhsIdataGam) of
                                { _hdOdataGam ->
                                (case (_lhsIcvarMp) of
                                 { _hdOcvarMp ->
                                 (case (True) of
                                  { _hdOisTopTup ->
                                  (case (True) of
                                   { _hdOisTopApp ->
                                   (case (Nothing) of
                                    { _hdOmbLamNm ->
                                    (case (hd_ _hdOcvarMp _hdOdataGam _hdOevalCtx _hdOgUniq _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOletBindingsCateg _hdOlev _hdOmbLamNm _hdOmoduleClassNm _hdOnm _hdOopts _hdOpkgNm _hdOtopClassNm ) of
                                     { ( _hdIbindNmL,_hdIgUniq,_hdIjbinds,_hdIjs,_hdIusedModNmS) ->
                                         (case (_hdIgUniq) of
                                          { _tlOgUniq ->
                                          (case (_lhsIevalCtx) of
                                           { _tlOevalCtx ->
                                           (case (_lhsIdataGam) of
                                            { _tlOdataGam ->
                                            (case (_lhsIcvarMp) of
                                             { _tlOcvarMp ->
                                             (case (tl_ _tlOcvarMp _tlOdataGam _tlOevalCtx _tlOgUniq _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOmoduleClassNm _tlOnm _tlOopts _tlOpkgNm _tlOtopClassNm ) of
                                              { ( _tlIbindNmL,_tlIgUniq,_tlIjbinds,_tlIusedModNmS) ->
                                                  (case (_hdIbindNmL ++ _tlIbindNmL) of
                                                   { _lhsObindNmL ->
                                                   (case (_tlIgUniq) of
                                                    { _lhsOgUniq ->
                                                    (case (_hdIjbinds `Seq.union` _tlIjbinds) of
                                                     { _lhsOjbinds ->
                                                     (case (_hdIusedModNmS `Set.union` _tlIusedModNmS) of
                                                      { _lhsOusedModNmS ->
                                                      ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImoduleClassNm
       _lhsInm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case ([]) of
          { _lhsObindNmL ->
          (case (_lhsIgUniq) of
           { _lhsOgUniq ->
           (case (Seq.empty) of
            { _lhsOjbinds ->
            (case (Set.empty) of
             { _lhsOusedModNmS ->
             ( _lhsObindNmL,_lhsOgUniq,_lhsOjbinds,_lhsOusedModNmS) }) }) }) }))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         nmArgL               : [HsName]
         whatBelow            : WhatExpr
   visit 1:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lev                  : Int
         mbLamNm              : Maybe (HsName,HsName)
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
         whatAbove            : WhatExpr
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         appFunKind           : AppFunKind
         argUnpackWrapL       : [J.Expr -> J.Expr]
         jbinds               : JBinds
         js                   : J.Expr
         jsArgFunL            : [J.Expr]
         jsLamBody            : J.Expr
         jstats               : Seq.Seq J.Stat
         mbFFIApp             : Maybe ( Ty
                                  , Bool
                                  , FFIWay
                                  , ForeignEnt
                                  , [Ty]
                                  )
         mbLam                : Maybe [HsName]
         mbVar                : Maybe HsName
         mkFFI                : [J.Expr] -> J.Expr
         resPackWrap          : J.Expr -> J.Expr
         usedModNmS           : FvS
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 1:
            local js          : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local mbLamNm     : _
            local isTopTup    : _
            local isTopApp'   : _
            local _tup1       : _
            local argUnpackWrapL : _
            local letBindingsCateg : _
            local isGlobal    : _
            local argUnpackWrap : _
            local jsArgFunL   : _
            local js          : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local _tup4       : {(UID,UID)}
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local _tup2       : _
            local scrutineeCVarInfo : _
            local lUniq       : {UID}
            local scrutineeTagJS : _
            local _tup3       : _
            local js          : _
            local jstatsCase  : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local js          : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative CoeArg:
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local whatBelow   : _
         visit 1:
            local argTyLresTy : {( TyL, Ty )}
            local resTy       : _
            local resMbCon    : _
            local argTyL      : {TyL}
            local argMbConL   : _
            local foreignEntInfo : _
            local _tup5       : _
            local mkArgsJS    : _
            local mkFFI       : _
            local mkResJS     : _
            local js          : _
            local mbPrimNeedEval : {Maybe PrimitiveNeedsEval}
            local primResNeedsEval : {Bool}
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local js          : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local js          : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local js          : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local argNm       : _
            local nmArgL      : {[HsName]}
            local whatBelow   : _
         visit 1:
            local lev         : _
            local letBindingsCateg : _
            local isGlobal    : _
            local _tup7       : _
            local hasFunHere  : _
            local _tup6       : _
            local lamBindings : _
            local cvarMp      : _
            local whatAbove   : {WhatExpr}
            local mbLamNm     : _
            local isTopTup    : _
            local isTopApp    : {Bool}
            local _tup8       : _
            local origLamNm   : _
            local lamNm       : _
            local jsArgTyL    : _
            local js          : _
            local jsLamBody   : _
            intra nmArgL      : {[HsName]}
            intra argNm       : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local letBindingsCateg : _
            local isGlobal    : _
            local evalCtx     : _
            local nmToRefAssocL : _
            local _tup10      : _
            local cvarMpNew   : _
            local cvarMp      : _
            local whatAbove   : {WhatExpr}
            local isTopTup    : _
            local isTopApp    : {Bool}
            local _tup9       : _
            local jbindsLet   : _
            local js          : _
            local jstatsLet   : _
      alternative String:
         child str            : {String}
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local whatBelow   : _
         visit 1:
            local js          : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopApp    : {Bool}
            local isTopTup    : _
            local js          : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopApp    : {Bool}
            local isTopTup    : _
            local js          : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local whatBelow   : _
         visit 1:
            local whatAbove   : {WhatExpr}
            local isTopApp    : {Bool}
            local isTopTup    : _
            local js          : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local nm          : {HsName}
            local whatBelow   : _
         visit 1:
            local varnm       : _
            local cvi         : _
            local js          : _
            local mbVar       : {Maybe HsName}
            intra nm          : {HsName}
-}
-- cata
sem_CExpr :: CExpr  ->
             T_CExpr 
sem_CExpr (CExpr_Ann _ann _expr )  =
    (sem_CExpr_Ann (sem_CExprAnn _ann ) (sem_CExpr _expr ) )
sem_CExpr (CExpr_App _func _arg )  =
    (sem_CExpr_App (sem_CExpr _func ) (sem_CBound _arg ) )
sem_CExpr (CExpr_Case _expr _alts _dflt )  =
    (sem_CExpr_Case (sem_CExpr _expr ) (sem_CAltL _alts ) (sem_CExpr _dflt ) )
sem_CExpr (CExpr_CaseAltFail _failReason _errorExpr )  =
    (sem_CExpr_CaseAltFail _failReason (sem_CExpr _errorExpr ) )
sem_CExpr (CExpr_Char _char )  =
    (sem_CExpr_Char _char )
sem_CExpr (CExpr_CoeArg )  =
    (sem_CExpr_CoeArg )
sem_CExpr (CExpr_FFI _callconv _safety _impEnt _ty )  =
    (sem_CExpr_FFI _callconv _safety _impEnt _ty )
sem_CExpr (CExpr_Hole _uid )  =
    (sem_CExpr_Hole _uid )
sem_CExpr (CExpr_HoleLet _bindsUid _body )  =
    (sem_CExpr_HoleLet _bindsUid (sem_CExpr _body ) )
sem_CExpr (CExpr_ImplsApp _func _uid )  =
    (sem_CExpr_ImplsApp (sem_CExpr _func ) _uid )
sem_CExpr (CExpr_ImplsLam _uid _body )  =
    (sem_CExpr_ImplsLam _uid (sem_CExpr _body ) )
sem_CExpr (CExpr_Int _int )  =
    (sem_CExpr_Int _int )
sem_CExpr (CExpr_Integer _integer )  =
    (sem_CExpr_Integer _integer )
sem_CExpr (CExpr_Lam _bind _body )  =
    (sem_CExpr_Lam (sem_CBind _bind ) (sem_CExpr _body ) )
sem_CExpr (CExpr_Let _categ _binds _body )  =
    (sem_CExpr_Let _categ (sem_CBindL _binds ) (sem_CExpr _body ) )
sem_CExpr (CExpr_String _str )  =
    (sem_CExpr_String _str )
sem_CExpr (CExpr_Tup _tag )  =
    (sem_CExpr_Tup _tag )
sem_CExpr (CExpr_TupDel _expr _tag _nm _offset )  =
    (sem_CExpr_TupDel (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) )
sem_CExpr (CExpr_TupIns _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupIns (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_TupUpd _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupUpd (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_Var _ref )  =
    (sem_CExpr_Var _ref )
-- semantic domain
type T_CExpr  = ( ([HsName]),WhatExpr,T_CExpr_1 )
type T_CExpr_1  = CVarMp ->
                  DataGam ->
                  EvalCtx ->
                  UID ->
                  Bool ->
                  Bool ->
                  Bool ->
                  Bool ->
                  Int ->
                  (Maybe (HsName,HsName)) ->
                  HsName ->
                  EHCOpts ->
                  HsName ->
                  HsName ->
                  WhatExpr ->
                  ( AppFunKind,([J.Expr -> J.Expr]),UID,JBinds,(J.Expr),([J.Expr]),(J.Expr),(Seq.Seq J.Stat),(Maybe ( Ty
                                                                                                                                                , Bool
                                                                                                                                                , FFIWay
                                                                                                                                                , ForeignEnt
                                                                                                                                                , [Ty]
                                                                                                                                                )),(Maybe [HsName]),(Maybe HsName),([J.Expr] -> J.Expr),(J.Expr -> J.Expr),FvS)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (case (expr_ ) of
     { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
         (case (_exprInmArgL) of
          { _lhsOnmArgL ->
          (case (_exprIwhatBelow) of
           { _lhsOwhatBelow ->
           (case ((let sem_CExpr_Ann_1 :: T_CExpr_1 
                       sem_CExpr_Ann_1  =
                           (\ _lhsIcvarMp
                              _lhsIdataGam
                              _lhsIevalCtx
                              _lhsIgUniq
                              _lhsIisLamBody
                              _lhsIisStrict
                              _lhsIisTopApp
                              _lhsIisTopTup
                              _lhsIlev
                              _lhsImbLamNm
                              _lhsImoduleClassNm
                              _lhsIopts
                              _lhsIpkgNm
                              _lhsItopClassNm
                              _lhsIwhatAbove ->
                                (case (_lhsIwhatAbove) of
                                 { _exprOwhatAbove ->
                                 (case (_lhsItopClassNm) of
                                  { _exprOtopClassNm ->
                                  (case (_lhsIpkgNm) of
                                   { _exprOpkgNm ->
                                   (case (_lhsIopts) of
                                    { _exprOopts ->
                                    (case (_lhsImoduleClassNm) of
                                     { _exprOmoduleClassNm ->
                                     (case (_lhsImbLamNm) of
                                      { _exprOmbLamNm ->
                                      (case (_lhsIlev) of
                                       { _exprOlev ->
                                       (case (_lhsIisTopTup) of
                                        { _exprOisTopTup ->
                                        (case (_lhsIisTopApp) of
                                         { _exprOisTopApp ->
                                         (case (_lhsIisStrict) of
                                          { _exprOisStrict ->
                                          (case (_lhsIisLamBody) of
                                           { _exprOisLamBody ->
                                           (case (_lhsIgUniq) of
                                            { _annOgUniq ->
                                            (case (_lhsItopClassNm) of
                                             { _annOtopClassNm ->
                                             (case (_lhsIpkgNm) of
                                              { _annOpkgNm ->
                                              (case (_lhsIopts) of
                                               { _annOopts ->
                                               (case (_lhsImoduleClassNm) of
                                                { _annOmoduleClassNm ->
                                                (case (_lhsIlev) of
                                                 { _annOlev ->
                                                 (case (_lhsIdataGam) of
                                                  { _annOdataGam ->
                                                  (case (_lhsIcvarMp) of
                                                   { _annOcvarMp ->
                                                   (case (ann_ _annOcvarMp _annOdataGam _annOgUniq _annOlev _annOmoduleClassNm _annOopts _annOpkgNm _annOtopClassNm ) of
                                                    { ( _annIgUniq) ->
                                                        (case (_annIgUniq) of
                                                         { _exprOgUniq ->
                                                         (case (_lhsIevalCtx) of
                                                          { _exprOevalCtx ->
                                                          (case (_lhsIdataGam) of
                                                           { _exprOdataGam ->
                                                           (case (_lhsIcvarMp) of
                                                            { _exprOcvarMp ->
                                                            (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                                             { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                                 (case (_exprIappFunKind) of
                                                                  { _lhsOappFunKind ->
                                                                  (case (_exprIargUnpackWrapL) of
                                                                   { _lhsOargUnpackWrapL ->
                                                                   (case (_exprIgUniq) of
                                                                    { _lhsOgUniq ->
                                                                    (case (_exprIjbinds) of
                                                                     { _lhsOjbinds ->
                                                                     (case (_exprIjs) of
                                                                      { _js ->
                                                                      (case (_js) of
                                                                       { _lhsOjs ->
                                                                       (case (_exprIjsArgFunL) of
                                                                        { _lhsOjsArgFunL ->
                                                                        (case (_exprIjsLamBody) of
                                                                         { _lhsOjsLamBody ->
                                                                         (case (_exprIjstats) of
                                                                          { _lhsOjstats ->
                                                                          (case (_exprImbFFIApp) of
                                                                           { _lhsOmbFFIApp ->
                                                                           (case (_exprImbLam) of
                                                                            { _lhsOmbLam ->
                                                                            (case (_exprImbVar) of
                                                                             { _lhsOmbVar ->
                                                                             (case (_exprImkFFI) of
                                                                              { _lhsOmkFFI ->
                                                                              (case (_exprIresPackWrap) of
                                                                               { _lhsOresPackWrap ->
                                                                               (case (_exprIusedModNmS) of
                                                                                { _lhsOusedModNmS ->
                                                                                ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_CExpr_Ann_1)) of
            { ( sem_CExpr_1) ->
            ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (func_ ) of
      { ( _funcInmArgL,_funcIwhatBelow,func_1) ->
          (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _funcIwhatBelow) of
           { _whatBelow ->
           (case (_whatBelow) of
            { _lhsOwhatBelow ->
            (case ((let sem_CExpr_App_1 :: T_CExpr_1 
                        sem_CExpr_App_1  =
                            (\ _lhsIcvarMp
                               _lhsIdataGam
                               _lhsIevalCtx
                               _lhsIgUniq
                               _lhsIisLamBody
                               _lhsIisStrict
                               _lhsIisTopApp
                               _lhsIisTopTup
                               _lhsIlev
                               _lhsImbLamNm
                               _lhsImoduleClassNm
                               _lhsIopts
                               _lhsIpkgNm
                               _lhsItopClassNm
                               _lhsIwhatAbove ->
                                 (case (maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove) of
                                  { _whatAbove ->
                                  (case (_whatAbove) of
                                   { _funcOwhatAbove ->
                                   (case (_lhsItopClassNm) of
                                    { _funcOtopClassNm ->
                                    (case (_lhsIpkgNm) of
                                     { _funcOpkgNm ->
                                     (case (_lhsIopts) of
                                      { _funcOopts ->
                                      (case (_lhsImoduleClassNm) of
                                       { _funcOmoduleClassNm ->
                                       (case (Nothing) of
                                        { _mbLamNm ->
                                        (case (_mbLamNm) of
                                         { _funcOmbLamNm ->
                                         (case (_lhsIlev) of
                                          { _funcOlev ->
                                          (case (True) of
                                           { _isTopTup ->
                                           (case (_isTopTup) of
                                            { _funcOisTopTup ->
                                            (case (_lhsIisStrict) of
                                             { _funcOisStrict ->
                                             (case (_lhsIisLamBody) of
                                              { _funcOisLamBody ->
                                              (case (_lhsIgUniq) of
                                               { _funcOgUniq ->
                                               (case (_lhsIevalCtx) of
                                                { _funcOevalCtx ->
                                                (case (_lhsIdataGam) of
                                                 { _funcOdataGam ->
                                                 (case (_lhsIcvarMp) of
                                                  { _funcOcvarMp ->
                                                  (case (False) of
                                                   { _funcOisTopApp ->
                                                   (case (func_1 _funcOcvarMp _funcOdataGam _funcOevalCtx _funcOgUniq _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlev _funcOmbLamNm _funcOmoduleClassNm _funcOopts _funcOpkgNm _funcOtopClassNm _funcOwhatAbove ) of
                                                    { ( _funcIappFunKind,_funcIargUnpackWrapL,_funcIgUniq,_funcIjbinds,_funcIjs,_funcIjsArgFunL,_funcIjsLamBody,_funcIjstats,_funcImbFFIApp,_funcImbLam,_funcImbVar,_funcImkFFI,_funcIresPackWrap,_funcIusedModNmS) ->
                                                        (case (_funcIappFunKind) of
                                                         { _lhsOappFunKind ->
                                                         (case (isNothing $ whatExprMbApp _lhsIwhatAbove) of
                                                          { _isTopApp' ->
                                                          (case (hdAndTl' id _funcIargUnpackWrapL) of
                                                           { __tup1 ->
                                                           (case (__tup1) of
                                                            { (_,_argUnpackWrapL) ->
                                                            (case (if _isTopApp' then [] else _argUnpackWrapL) of
                                                             { _lhsOargUnpackWrapL ->
                                                             (case (_funcIgUniq) of
                                                              { _argOgUniq ->
                                                              (case (_lhsItopClassNm) of
                                                               { _argOtopClassNm ->
                                                               (case (_lhsIpkgNm) of
                                                                { _argOpkgNm ->
                                                                (case (_lhsIopts) of
                                                                 { _argOopts ->
                                                                 (case (_lhsImoduleClassNm) of
                                                                  { _argOmoduleClassNm ->
                                                                  (case (_mbLamNm) of
                                                                   { _argOmbLamNm ->
                                                                   (case (_lhsIlev) of
                                                                    { _argOlev ->
                                                                    (case (acoreBindcategPlain) of
                                                                     { _letBindingsCateg ->
                                                                     (case (_letBindingsCateg) of
                                                                      { _argOletBindingsCateg ->
                                                                      (case (_isTopTup) of
                                                                       { _argOisTopTup ->
                                                                       (case (_lhsIisStrict) of
                                                                        { _argOisStrict ->
                                                                        (case (_lhsIisLamBody) of
                                                                         { _argOisLamBody ->
                                                                         (case (False) of
                                                                          { _isGlobal ->
                                                                          (case (_isGlobal) of
                                                                           { _argOisGlobal ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _argOevalCtx ->
                                                                            (case (_lhsIdataGam) of
                                                                             { _argOdataGam ->
                                                                             (case (_lhsIcvarMp) of
                                                                              { _argOcvarMp ->
                                                                              (case (True) of
                                                                               { _argOisTopApp ->
                                                                               (case (hsnUnknown) of
                                                                                { _argOnm ->
                                                                                (case (arg_ _argOcvarMp _argOdataGam _argOevalCtx _argOgUniq _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOletBindingsCateg _argOlev _argOmbLamNm _argOmoduleClassNm _argOnm _argOopts _argOpkgNm _argOtopClassNm ) of
                                                                                 { ( _argIbindNmL,_argIgUniq,_argIjbinds,_argIjs,_argIusedModNmS) ->
                                                                                     (case (_argIgUniq) of
                                                                                      { _lhsOgUniq ->
                                                                                      (case (_funcIjbinds `Seq.union` _argIjbinds) of
                                                                                       { _lhsOjbinds ->
                                                                                       (case (__tup1) of
                                                                                        { (_argUnpackWrap,_) ->
                                                                                        (case (_argUnpackWrap _argIjs : _funcIjsArgFunL) of
                                                                                         { _jsArgFunL ->
                                                                                         (case (if _isTopApp'
                                                                                                then let (f:as) = reverse _jsArgFunL
                                                                                                     in  case _funcIappFunKind of
                                                                                                           AppFunKind_Tag tag -> jsNewTupOrData _lhsIdataGam _lhsIpkgNm _lhsItopClassNm tag as
                                                                                                           AppFunKind_FFI     -> _funcIresPackWrap $ _funcImkFFI       as
                                                                                                           _                  -> jsApp f as
                                                                                                else J.Expr_Str "*** ERR APP ***") of
                                                                                          { _js ->
                                                                                          (case (_js) of
                                                                                           { _lhsOjs ->
                                                                                           (case (_jsArgFunL) of
                                                                                            { _lhsOjsArgFunL ->
                                                                                            (case (_js) of
                                                                                             { _lhsOjsLamBody ->
                                                                                             (case (Seq.empty) of
                                                                                              { _lhsOjstats ->
                                                                                              (case (_funcImbFFIApp) of
                                                                                               { _lhsOmbFFIApp ->
                                                                                               (case (Nothing) of
                                                                                                { _lhsOmbLam ->
                                                                                                (case (Nothing) of
                                                                                                 { _lhsOmbVar ->
                                                                                                 (case (if _isTopApp' then head else _funcImkFFI) of
                                                                                                  { _lhsOmkFFI ->
                                                                                                  (case (if _isTopApp' then id else _funcIresPackWrap) of
                                                                                                   { _lhsOresPackWrap ->
                                                                                                   (case (_funcIusedModNmS `Set.union` _argIusedModNmS) of
                                                                                                    { _lhsOusedModNmS ->
                                                                                                    ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                    in  sem_CExpr_App_1)) of
             { ( sem_CExpr_1) ->
             ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) }) })
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Case_1 :: T_CExpr_1 
                   sem_CExpr_Case_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )) of
                               { __tup4 ->
                               (case (__tup4) of
                                { (_exprOgUniq,_) ->
                                (case (expr_ ) of
                                 { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                                     (case (ExprIsOther) of
                                      { _whatAbove ->
                                      (case (_whatAbove) of
                                       { _exprOwhatAbove ->
                                       (case (_lhsItopClassNm) of
                                        { _exprOtopClassNm ->
                                        (case (_lhsIpkgNm) of
                                         { _exprOpkgNm ->
                                         (case (_lhsIopts) of
                                          { _exprOopts ->
                                          (case (_lhsImoduleClassNm) of
                                           { _exprOmoduleClassNm ->
                                           (case (_lhsImbLamNm) of
                                            { _exprOmbLamNm ->
                                            (case (_lhsIlev) of
                                             { _exprOlev ->
                                             (case (True) of
                                              { _isTopTup ->
                                              (case (_isTopTup) of
                                               { _exprOisTopTup ->
                                               (case (True) of
                                                { _isTopApp ->
                                                (case (_isTopApp) of
                                                 { _exprOisTopApp ->
                                                 (case (_lhsIisStrict) of
                                                  { _exprOisStrict ->
                                                  (case (_lhsIisLamBody) of
                                                   { _exprOisLamBody ->
                                                   (case (_lhsIevalCtx) of
                                                    { _exprOevalCtx ->
                                                    (case (_lhsIdataGam) of
                                                     { _exprOdataGam ->
                                                     (case (_lhsIcvarMp) of
                                                      { _exprOcvarMp ->
                                                      (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                                       { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                           (case (_exprIgUniq) of
                                                            { _altsOgUniq ->
                                                            (case (alts_ ) of
                                                             { ( _altsIscrutinees,alts_1) ->
                                                                 (case (_lhsItopClassNm) of
                                                                  { _altsOtopClassNm ->
                                                                  (case (case _altsIscrutinees of
                                                                           (Scrutinee_Var nm : _)
                                                                             -> panic ("Core.ToJavaScript.CExpr.Case.Scrutinee_Var: " ++ show nm ++ " : not yet implemented")
                                                                           (Scrutinee_Int _ : _)
                                                                             -> ( _exprIjs
                                                                                , CVarInfo_None
                                                                                )
                                                                           (Scrutinee_Tag tag : _)
                                                                             -> ( gettag
                                                                                , CVarInfo_Local () $ panicJust "ToJavaScript.CExpr.Case scrutinee" $ J.exprMbVar _exprIjs
                                                                                )
                                                                             where (gettag)
                                                                                      = case tag of
                                                                                          CTagRec         -> ( J.Expr_Str "*** ERR GETTAG ***" )
                                                                                          CTag _ _ _ _ _  -> ( J.Expr_Sel _exprIjs (jsVar nmTag)
                                                                                                             )
                                                                           (Scrutinee_Other x : _)
                                                                             -> panic ("Core.ToJavaScript.CExpr.Case.Scrutinee_Other: " ++ x ++ " : not yet implemented")
                                                                           []
                                                                             -> panic ("Core.ToJavaScript.CExpr.Case.-")) of
                                                                   { __tup2 ->
                                                                   (case (__tup2) of
                                                                    { (_,_scrutineeCVarInfo) ->
                                                                    (case (_scrutineeCVarInfo) of
                                                                     { _altsOscrutineeCVarInfo ->
                                                                     (case (_lhsIpkgNm) of
                                                                      { _altsOpkgNm ->
                                                                      (case (_lhsIopts) of
                                                                       { _altsOopts ->
                                                                       (case (_lhsImoduleClassNm) of
                                                                        { _altsOmoduleClassNm ->
                                                                        (case (_lhsIlev) of
                                                                         { _altsOlev ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _altsOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _altsOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _altsOevalCtx ->
                                                                            (case (_lhsIdataGam) of
                                                                             { _altsOdataGam ->
                                                                             (case (_lhsIcvarMp) of
                                                                              { _altsOcvarMp ->
                                                                              (case (alts_1 _altsOcvarMp _altsOdataGam _altsOevalCtx _altsOgUniq _altsOisLamBody _altsOisStrict _altsOlev _altsOmoduleClassNm _altsOopts _altsOpkgNm _altsOscrutineeCVarInfo _altsOtopClassNm ) of
                                                                               { ( _altsIaltsJsL,_altsIgUniq) ->
                                                                                   (case (_altsIgUniq) of
                                                                                    { _dfltOgUniq ->
                                                                                    (case (dflt_ ) of
                                                                                     { ( _dfltInmArgL,_dfltIwhatBelow,dflt_1) ->
                                                                                         (case (_whatAbove) of
                                                                                          { _dfltOwhatAbove ->
                                                                                          (case (_lhsItopClassNm) of
                                                                                           { _dfltOtopClassNm ->
                                                                                           (case (_lhsIpkgNm) of
                                                                                            { _dfltOpkgNm ->
                                                                                            (case (_lhsIopts) of
                                                                                             { _dfltOopts ->
                                                                                             (case (_lhsImoduleClassNm) of
                                                                                              { _dfltOmoduleClassNm ->
                                                                                              (case (_lhsImbLamNm) of
                                                                                               { _dfltOmbLamNm ->
                                                                                               (case (_lhsIlev) of
                                                                                                { _dfltOlev ->
                                                                                                (case (_isTopTup) of
                                                                                                 { _dfltOisTopTup ->
                                                                                                 (case (_isTopApp) of
                                                                                                  { _dfltOisTopApp ->
                                                                                                  (case (_lhsIisStrict) of
                                                                                                   { _dfltOisStrict ->
                                                                                                   (case (_lhsIisLamBody) of
                                                                                                    { _dfltOisLamBody ->
                                                                                                    (case (_lhsIevalCtx) of
                                                                                                     { _dfltOevalCtx ->
                                                                                                     (case (_lhsIdataGam) of
                                                                                                      { _dfltOdataGam ->
                                                                                                      (case (_lhsIcvarMp) of
                                                                                                       { _dfltOcvarMp ->
                                                                                                       (case (dflt_1 _dfltOcvarMp _dfltOdataGam _dfltOevalCtx _dfltOgUniq _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlev _dfltOmbLamNm _dfltOmoduleClassNm _dfltOopts _dfltOpkgNm _dfltOtopClassNm _dfltOwhatAbove ) of
                                                                                                        { ( _dfltIappFunKind,_dfltIargUnpackWrapL,_dfltIgUniq,_dfltIjbinds,_dfltIjs,_dfltIjsArgFunL,_dfltIjsLamBody,_dfltIjstats,_dfltImbFFIApp,_dfltImbLam,_dfltImbVar,_dfltImkFFI,_dfltIresPackWrap,_dfltIusedModNmS) ->
                                                                                                            (case (_dfltIgUniq) of
                                                                                                             { _lhsOgUniq ->
                                                                                                             (case (_exprIjbinds `Seq.union` _dfltIjbinds) of
                                                                                                              { _lhsOjbinds ->
                                                                                                              (case (__tup4) of
                                                                                                               { (_,_lUniq) ->
                                                                                                               (case (__tup2) of
                                                                                                                { (_scrutineeTagJS,_) ->
                                                                                                                (case (let alts = case _altsIaltsJsL of
                                                                                                                                    [(_,stats,e)]
                                                                                                                                       -> (stats,e)
                                                                                                                                    as -> ( Seq.fromList $
                                                                                                                                              [ J.Stat_VarDecl swRes Nothing
                                                                                                                                              , J.Stat_Switch _scrutineeTagJS
                                                                                                                                                  [ J.Alt_Alt tag $ Seq.toList $ stats `Seq.union` Seq.fromList [jsAssign swRes e, J.Stat_Break]
                                                                                                                                                  | (tag,stats,e) <- as
                                                                                                                                                  ]
                                                                                                                                              ]
                                                                                                                                          , jsVar swRes
                                                                                                                                          )
                                                                                                                                       where swRes   = hsnJavaScriptVar False _lhsIpkgNm _lhsItopClassNm $ hsnUniqifyUID HsNameUniqifier_JSSwitchResult _lUniq nmSwitchRes
                                                                                                                       in  alts) of
                                                                                                                 { __tup3 ->
                                                                                                                 (case (__tup3) of
                                                                                                                  { (_,_js) ->
                                                                                                                  (case (_js) of
                                                                                                                   { _lhsOjs ->
                                                                                                                   (case ([_js]) of
                                                                                                                    { _lhsOjsArgFunL ->
                                                                                                                    (case (_js) of
                                                                                                                     { _lhsOjsLamBody ->
                                                                                                                     (case (__tup3) of
                                                                                                                      { (_jstatsCase,_) ->
                                                                                                                      (case (_jstatsCase) of
                                                                                                                       { _lhsOjstats ->
                                                                                                                       (case (Nothing) of
                                                                                                                        { _lhsOmbFFIApp ->
                                                                                                                        (case (Nothing) of
                                                                                                                         { _lhsOmbLam ->
                                                                                                                         (case (Nothing) of
                                                                                                                          { _lhsOmbVar ->
                                                                                                                          (case (head) of
                                                                                                                           { _lhsOmkFFI ->
                                                                                                                           (case (id) of
                                                                                                                            { _lhsOresPackWrap ->
                                                                                                                            (case (_exprIusedModNmS `Set.union` _dfltIusedModNmS) of
                                                                                                                             { _lhsOusedModNmS ->
                                                                                                                             ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Case_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (errorExpr_ ) of
      { ( _errorExprInmArgL,_errorExprIwhatBelow,errorExpr_1) ->
          (case (_errorExprIwhatBelow) of
           { _lhsOwhatBelow ->
           (case ((let sem_CExpr_CaseAltFail_1 :: T_CExpr_1 
                       sem_CExpr_CaseAltFail_1  =
                           (\ _lhsIcvarMp
                              _lhsIdataGam
                              _lhsIevalCtx
                              _lhsIgUniq
                              _lhsIisLamBody
                              _lhsIisStrict
                              _lhsIisTopApp
                              _lhsIisTopTup
                              _lhsIlev
                              _lhsImbLamNm
                              _lhsImoduleClassNm
                              _lhsIopts
                              _lhsIpkgNm
                              _lhsItopClassNm
                              _lhsIwhatAbove ->
                                (case (ExprIsOther) of
                                 { _whatAbove ->
                                 (case (_whatAbove) of
                                  { _errorExprOwhatAbove ->
                                  (case (_lhsItopClassNm) of
                                   { _errorExprOtopClassNm ->
                                   (case (_lhsIpkgNm) of
                                    { _errorExprOpkgNm ->
                                    (case (_lhsIopts) of
                                     { _errorExprOopts ->
                                     (case (_lhsImoduleClassNm) of
                                      { _errorExprOmoduleClassNm ->
                                      (case (_lhsImbLamNm) of
                                       { _errorExprOmbLamNm ->
                                       (case (_lhsIlev) of
                                        { _errorExprOlev ->
                                        (case (True) of
                                         { _isTopTup ->
                                         (case (_isTopTup) of
                                          { _errorExprOisTopTup ->
                                          (case (True) of
                                           { _isTopApp ->
                                           (case (_isTopApp) of
                                            { _errorExprOisTopApp ->
                                            (case (_lhsIisStrict) of
                                             { _errorExprOisStrict ->
                                             (case (_lhsIisLamBody) of
                                              { _errorExprOisLamBody ->
                                              (case (_lhsIgUniq) of
                                               { _errorExprOgUniq ->
                                               (case (_lhsIevalCtx) of
                                                { _errorExprOevalCtx ->
                                                (case (_lhsIdataGam) of
                                                 { _errorExprOdataGam ->
                                                 (case (_lhsIcvarMp) of
                                                  { _errorExprOcvarMp ->
                                                  (case (errorExpr_1 _errorExprOcvarMp _errorExprOdataGam _errorExprOevalCtx _errorExprOgUniq _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlev _errorExprOmbLamNm _errorExprOmoduleClassNm _errorExprOopts _errorExprOpkgNm _errorExprOtopClassNm _errorExprOwhatAbove ) of
                                                   { ( _errorExprIappFunKind,_errorExprIargUnpackWrapL,_errorExprIgUniq,_errorExprIjbinds,_errorExprIjs,_errorExprIjsArgFunL,_errorExprIjsLamBody,_errorExprIjstats,_errorExprImbFFIApp,_errorExprImbLam,_errorExprImbVar,_errorExprImkFFI,_errorExprIresPackWrap,_errorExprIusedModNmS) ->
                                                       (case (_errorExprIappFunKind) of
                                                        { _lhsOappFunKind ->
                                                        (case ([]) of
                                                         { _lhsOargUnpackWrapL ->
                                                         (case (_errorExprIgUniq) of
                                                          { _lhsOgUniq ->
                                                          (case (_errorExprIjbinds) of
                                                           { _lhsOjbinds ->
                                                           (case (_errorExprIjs) of
                                                            { _js ->
                                                            (case (_js) of
                                                             { _lhsOjs ->
                                                             (case ([_js]) of
                                                              { _lhsOjsArgFunL ->
                                                              (case (_js) of
                                                               { _lhsOjsLamBody ->
                                                               (case (Seq.empty) of
                                                                { _lhsOjstats ->
                                                                (case (Nothing) of
                                                                 { _lhsOmbFFIApp ->
                                                                 (case (_errorExprImbLam) of
                                                                  { _lhsOmbLam ->
                                                                  (case (_errorExprImbVar) of
                                                                   { _lhsOmbVar ->
                                                                   (case (head) of
                                                                    { _lhsOmkFFI ->
                                                                    (case (id) of
                                                                     { _lhsOresPackWrap ->
                                                                     (case (_errorExprIusedModNmS) of
                                                                      { _lhsOusedModNmS ->
                                                                      ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_CExpr_CaseAltFail_1)) of
            { ( sem_CExpr_1) ->
            ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Char_1 :: T_CExpr_1 
                   sem_CExpr_Char_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (jsIntConst (ord char_)) of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Char_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_CoeArg_1 :: T_CExpr_1 
                   sem_CExpr_CoeArg_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_CoeArg_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_FFI_1 :: T_CExpr_1 
                   sem_CExpr_FFI_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_FFI) of
                             { _lhsOappFunKind ->
                             (case (tyArrowArgsRes ty_) of
                              { _argTyLresTy ->
                              (case (snd _argTyLresTy) of
                               { _resTy ->
                               (case (tyAppFunMbConNm _resTy) of
                                { _resMbCon ->
                                (case (fst _argTyLresTy) of
                                 { _argTyL ->
                                 (case (map tyAppFunMbConNm _argTyL) of
                                  { _argMbConL ->
                                  (case (foreignEntExtract impEnt_) of
                                   { _foreignEntInfo ->
                                   (case (ffiJavaScriptMkCall
                                              ty_
                                              _foreignEntInfo
                                              _lhsIopts False _argMbConL _resMbCon) of
                                    { __tup5 ->
                                    (case (__tup5) of
                                     { (_mkArgsJS,_,_) ->
                                     (case (_mkArgsJS) of
                                      { _lhsOargUnpackWrapL ->
                                      (case (_lhsIgUniq) of
                                       { _lhsOgUniq ->
                                       (case (Seq.empty) of
                                        { _lhsOjbinds ->
                                        (case (__tup5) of
                                         { (_,_,_mkFFI) ->
                                         (case (__tup5) of
                                          { (_,_mkResJS,_) ->
                                          (case (if null _argMbConL
                                                 then (_mkResJS $ _mkFFI [])
                                                 else (jsVar $ mkHNm $ forextractEnt _foreignEntInfo)) of
                                           { _js ->
                                           (case (_js) of
                                            { _lhsOjs ->
                                            (case ([_js]) of
                                             { _lhsOjsArgFunL ->
                                             (case (_js) of
                                              { _lhsOjsLamBody ->
                                              (case (Seq.empty) of
                                               { _lhsOjstats ->
                                               (case (maybe Nothing lookupPrimNeedsEval $ forextractMbEnt _foreignEntInfo) of
                                                { _mbPrimNeedEval ->
                                                (case (maybe False primResNeedEval _mbPrimNeedEval) of
                                                 { _primResNeedsEval ->
                                                 (case (Just ( _resTy
                                                             , _primResNeedsEval
                                                             , callconv_
                                                             , impEnt_
                                                             , _argTyL
                                                             )) of
                                                  { _lhsOmbFFIApp ->
                                                  (case (Nothing) of
                                                   { _lhsOmbLam ->
                                                   (case (Nothing) of
                                                    { _lhsOmbVar ->
                                                    (case (_mkFFI) of
                                                     { _lhsOmkFFI ->
                                                     (case (_mkResJS) of
                                                      { _lhsOresPackWrap ->
                                                      (case (Set.empty) of
                                                       { _lhsOusedModNmS ->
                                                       ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_FFI_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Hole_1 :: T_CExpr_1 
                   sem_CExpr_Hole_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Hole_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_HoleLet_1 :: T_CExpr_1 
                   sem_CExpr_HoleLet_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _bodyOgUniq ->
                               (case (body_ ) of
                                { ( _bodyInmArgL,_bodyIwhatBelow,body_1) ->
                                    (case (ExprIsOther) of
                                     { _whatAbove ->
                                     (case (_whatAbove) of
                                      { _bodyOwhatAbove ->
                                      (case (_lhsItopClassNm) of
                                       { _bodyOtopClassNm ->
                                       (case (_lhsIpkgNm) of
                                        { _bodyOpkgNm ->
                                        (case (_lhsIopts) of
                                         { _bodyOopts ->
                                         (case (_lhsImoduleClassNm) of
                                          { _bodyOmoduleClassNm ->
                                          (case (_lhsImbLamNm) of
                                           { _bodyOmbLamNm ->
                                           (case (_lhsIlev) of
                                            { _bodyOlev ->
                                            (case (True) of
                                             { _isTopTup ->
                                             (case (_isTopTup) of
                                              { _bodyOisTopTup ->
                                              (case (True) of
                                               { _isTopApp ->
                                               (case (_isTopApp) of
                                                { _bodyOisTopApp ->
                                                (case (_lhsIisStrict) of
                                                 { _bodyOisStrict ->
                                                 (case (_lhsIisLamBody) of
                                                  { _bodyOisLamBody ->
                                                  (case (_lhsIevalCtx) of
                                                   { _bodyOevalCtx ->
                                                   (case (_lhsIdataGam) of
                                                    { _bodyOdataGam ->
                                                    (case (_lhsIcvarMp) of
                                                     { _bodyOcvarMp ->
                                                     (case (body_1 _bodyOcvarMp _bodyOdataGam _bodyOevalCtx _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOmbLamNm _bodyOmoduleClassNm _bodyOopts _bodyOpkgNm _bodyOtopClassNm _bodyOwhatAbove ) of
                                                      { ( _bodyIappFunKind,_bodyIargUnpackWrapL,_bodyIgUniq,_bodyIjbinds,_bodyIjs,_bodyIjsArgFunL,_bodyIjsLamBody,_bodyIjstats,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar,_bodyImkFFI,_bodyIresPackWrap,_bodyIusedModNmS) ->
                                                          (case (_bodyIgUniq) of
                                                           { _lhsOgUniq ->
                                                           (case (_bodyIjbinds) of
                                                            { _lhsOjbinds ->
                                                            (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                                             { _js ->
                                                             (case (_js) of
                                                              { _lhsOjs ->
                                                              (case ([_js]) of
                                                               { _lhsOjsArgFunL ->
                                                               (case (_js) of
                                                                { _lhsOjsLamBody ->
                                                                (case (Seq.empty) of
                                                                 { _lhsOjstats ->
                                                                 (case (Nothing) of
                                                                  { _lhsOmbFFIApp ->
                                                                  (case (Nothing) of
                                                                   { _lhsOmbLam ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbVar ->
                                                                    (case (head) of
                                                                     { _lhsOmkFFI ->
                                                                     (case (id) of
                                                                      { _lhsOresPackWrap ->
                                                                      (case (_bodyIusedModNmS) of
                                                                       { _lhsOusedModNmS ->
                                                                       ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_HoleLet_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_ImplsApp_1 :: T_CExpr_1 
                   sem_CExpr_ImplsApp_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _funcOgUniq ->
                               (case (func_ ) of
                                { ( _funcInmArgL,_funcIwhatBelow,func_1) ->
                                    (case (ExprIsOther) of
                                     { _whatAbove ->
                                     (case (_whatAbove) of
                                      { _funcOwhatAbove ->
                                      (case (_lhsItopClassNm) of
                                       { _funcOtopClassNm ->
                                       (case (_lhsIpkgNm) of
                                        { _funcOpkgNm ->
                                        (case (_lhsIopts) of
                                         { _funcOopts ->
                                         (case (_lhsImoduleClassNm) of
                                          { _funcOmoduleClassNm ->
                                          (case (_lhsImbLamNm) of
                                           { _funcOmbLamNm ->
                                           (case (_lhsIlev) of
                                            { _funcOlev ->
                                            (case (True) of
                                             { _isTopTup ->
                                             (case (_isTopTup) of
                                              { _funcOisTopTup ->
                                              (case (True) of
                                               { _isTopApp ->
                                               (case (_isTopApp) of
                                                { _funcOisTopApp ->
                                                (case (_lhsIisStrict) of
                                                 { _funcOisStrict ->
                                                 (case (_lhsIisLamBody) of
                                                  { _funcOisLamBody ->
                                                  (case (_lhsIevalCtx) of
                                                   { _funcOevalCtx ->
                                                   (case (_lhsIdataGam) of
                                                    { _funcOdataGam ->
                                                    (case (_lhsIcvarMp) of
                                                     { _funcOcvarMp ->
                                                     (case (func_1 _funcOcvarMp _funcOdataGam _funcOevalCtx _funcOgUniq _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlev _funcOmbLamNm _funcOmoduleClassNm _funcOopts _funcOpkgNm _funcOtopClassNm _funcOwhatAbove ) of
                                                      { ( _funcIappFunKind,_funcIargUnpackWrapL,_funcIgUniq,_funcIjbinds,_funcIjs,_funcIjsArgFunL,_funcIjsLamBody,_funcIjstats,_funcImbFFIApp,_funcImbLam,_funcImbVar,_funcImkFFI,_funcIresPackWrap,_funcIusedModNmS) ->
                                                          (case (_funcIgUniq) of
                                                           { _lhsOgUniq ->
                                                           (case (_funcIjbinds) of
                                                            { _lhsOjbinds ->
                                                            (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                                             { _js ->
                                                             (case (_js) of
                                                              { _lhsOjs ->
                                                              (case ([_js]) of
                                                               { _lhsOjsArgFunL ->
                                                               (case (_js) of
                                                                { _lhsOjsLamBody ->
                                                                (case (Seq.empty) of
                                                                 { _lhsOjstats ->
                                                                 (case (Nothing) of
                                                                  { _lhsOmbFFIApp ->
                                                                  (case (Nothing) of
                                                                   { _lhsOmbLam ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbVar ->
                                                                    (case (head) of
                                                                     { _lhsOmkFFI ->
                                                                     (case (id) of
                                                                      { _lhsOresPackWrap ->
                                                                      (case (_funcIusedModNmS) of
                                                                       { _lhsOusedModNmS ->
                                                                       ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_ImplsApp_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_ImplsLam_1 :: T_CExpr_1 
                   sem_CExpr_ImplsLam_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _bodyOgUniq ->
                               (case (body_ ) of
                                { ( _bodyInmArgL,_bodyIwhatBelow,body_1) ->
                                    (case (ExprIsOther) of
                                     { _whatAbove ->
                                     (case (_whatAbove) of
                                      { _bodyOwhatAbove ->
                                      (case (_lhsItopClassNm) of
                                       { _bodyOtopClassNm ->
                                       (case (_lhsIpkgNm) of
                                        { _bodyOpkgNm ->
                                        (case (_lhsIopts) of
                                         { _bodyOopts ->
                                         (case (_lhsImoduleClassNm) of
                                          { _bodyOmoduleClassNm ->
                                          (case (_lhsImbLamNm) of
                                           { _bodyOmbLamNm ->
                                           (case (_lhsIlev) of
                                            { _bodyOlev ->
                                            (case (True) of
                                             { _isTopTup ->
                                             (case (_isTopTup) of
                                              { _bodyOisTopTup ->
                                              (case (True) of
                                               { _isTopApp ->
                                               (case (_isTopApp) of
                                                { _bodyOisTopApp ->
                                                (case (_lhsIisStrict) of
                                                 { _bodyOisStrict ->
                                                 (case (_lhsIisLamBody) of
                                                  { _bodyOisLamBody ->
                                                  (case (_lhsIevalCtx) of
                                                   { _bodyOevalCtx ->
                                                   (case (_lhsIdataGam) of
                                                    { _bodyOdataGam ->
                                                    (case (_lhsIcvarMp) of
                                                     { _bodyOcvarMp ->
                                                     (case (body_1 _bodyOcvarMp _bodyOdataGam _bodyOevalCtx _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOmbLamNm _bodyOmoduleClassNm _bodyOopts _bodyOpkgNm _bodyOtopClassNm _bodyOwhatAbove ) of
                                                      { ( _bodyIappFunKind,_bodyIargUnpackWrapL,_bodyIgUniq,_bodyIjbinds,_bodyIjs,_bodyIjsArgFunL,_bodyIjsLamBody,_bodyIjstats,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar,_bodyImkFFI,_bodyIresPackWrap,_bodyIusedModNmS) ->
                                                          (case (_bodyIgUniq) of
                                                           { _lhsOgUniq ->
                                                           (case (_bodyIjbinds) of
                                                            { _lhsOjbinds ->
                                                            (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                                             { _js ->
                                                             (case (_js) of
                                                              { _lhsOjs ->
                                                              (case ([_js]) of
                                                               { _lhsOjsArgFunL ->
                                                               (case (_js) of
                                                                { _lhsOjsLamBody ->
                                                                (case (Seq.empty) of
                                                                 { _lhsOjstats ->
                                                                 (case (Nothing) of
                                                                  { _lhsOmbFFIApp ->
                                                                  (case (Nothing) of
                                                                   { _lhsOmbLam ->
                                                                   (case (Nothing) of
                                                                    { _lhsOmbVar ->
                                                                    (case (head) of
                                                                     { _lhsOmkFFI ->
                                                                     (case (id) of
                                                                      { _lhsOresPackWrap ->
                                                                      (case (_bodyIusedModNmS) of
                                                                       { _lhsOusedModNmS ->
                                                                       ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_ImplsLam_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsInt int_) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Int_1 :: T_CExpr_1 
                   sem_CExpr_Int_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (jsIntConst  int_) of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Int_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Integer_1 :: T_CExpr_1 
                   sem_CExpr_Integer_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Integer_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (case (bind_ ) of
     { ( _bindIbindNmL,_bindInm,bind_1) ->
         (case (_bindInm) of
          { _argNm ->
          (case (body_ ) of
           { ( _bodyInmArgL,_bodyIwhatBelow,body_1) ->
               (case (_argNm : _bodyInmArgL) of
                { _nmArgL ->
                (case (_nmArgL) of
                 { _lhsOnmArgL ->
                 (case (ExprIsLam) of
                  { _whatBelow ->
                  (case (_whatBelow) of
                   { _lhsOwhatBelow ->
                   (case ((let sem_CExpr_Lam_1 :: T_CExpr_1 
                               sem_CExpr_Lam_1  =
                                   (\ _lhsIcvarMp
                                      _lhsIdataGam
                                      _lhsIevalCtx
                                      _lhsIgUniq
                                      _lhsIisLamBody
                                      _lhsIisStrict
                                      _lhsIisTopApp
                                      _lhsIisTopTup
                                      _lhsIlev
                                      _lhsImbLamNm
                                      _lhsImoduleClassNm
                                      _lhsIopts
                                      _lhsIpkgNm
                                      _lhsItopClassNm
                                      _lhsIwhatAbove ->
                                        (case (AppFunKind_NoApp) of
                                         { _lhsOappFunKind ->
                                         (case ([]) of
                                          { _lhsOargUnpackWrapL ->
                                          (case (_lhsIgUniq) of
                                           { _bindOgUniq ->
                                           (case (_lhsItopClassNm) of
                                            { _bindOtopClassNm ->
                                            (case (_lhsIpkgNm) of
                                             { _bindOpkgNm ->
                                             (case (_lhsIopts) of
                                              { _bindOopts ->
                                              (case (_lhsImoduleClassNm) of
                                               { _bindOmoduleClassNm ->
                                               (case (_lhsIlev + 1) of
                                                { _lev ->
                                                (case (_lev) of
                                                 { _bindOlev ->
                                                 (case (acoreBindcategPlain) of
                                                  { _letBindingsCateg ->
                                                  (case (_letBindingsCateg) of
                                                   { _bindOletBindingsCateg ->
                                                   (case (_lhsIisStrict) of
                                                    { _bindOisStrict ->
                                                    (case (_lhsIisLamBody) of
                                                     { _bindOisLamBody ->
                                                     (case (False) of
                                                      { _isGlobal ->
                                                      (case (_isGlobal) of
                                                       { _bindOisGlobal ->
                                                       (case (_lhsIevalCtx) of
                                                        { _bindOevalCtx ->
                                                        (case (_lhsIdataGam) of
                                                         { _bindOdataGam ->
                                                         (case (if _lhsIwhatAbove /= ExprIsLam
                                                                then (True,fromJust _lhsImbLamNm)
                                                                else (False,(hsnUnknown,hsnUnknown))) of
                                                          { __tup7 ->
                                                          (case (__tup7) of
                                                           { (_hasFunHere,_) ->
                                                           (case (if _hasFunHere
                                                                  then jsArgsUnpack (map (hsnJavaScriptVar False _lhsIpkgNm _lhsItopClassNm)) _nmArgL
                                                                  else ([],[],[])) of
                                                            { __tup6 ->
                                                            (case (__tup6) of
                                                             { (_,_,_lamBindings) ->
                                                             (case (Map.fromList _lamBindings `Map.union` _lhsIcvarMp) of
                                                              { _cvarMp ->
                                                              (case (_cvarMp) of
                                                               { _bindOcvarMp ->
                                                               (case (bind_1 _bindOcvarMp _bindOdataGam _bindOevalCtx _bindOgUniq _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev _bindOmoduleClassNm _bindOopts _bindOpkgNm _bindOtopClassNm ) of
                                                                { ( _bindIgUniq,_bindIjbinds,_bindIusedModNmS) ->
                                                                    (case (_bindIgUniq) of
                                                                     { _bodyOgUniq ->
                                                                     (case (ExprIsLam) of
                                                                      { _whatAbove ->
                                                                      (case (_whatAbove) of
                                                                       { _bodyOwhatAbove ->
                                                                       (case (_lhsItopClassNm) of
                                                                        { _bodyOtopClassNm ->
                                                                        (case (_lhsIpkgNm) of
                                                                         { _bodyOpkgNm ->
                                                                         (case (_lhsIopts) of
                                                                          { _bodyOopts ->
                                                                          (case (_lhsImoduleClassNm) of
                                                                           { _bodyOmoduleClassNm ->
                                                                           (case (Nothing) of
                                                                            { _mbLamNm ->
                                                                            (case (_mbLamNm) of
                                                                             { _bodyOmbLamNm ->
                                                                             (case (_lev) of
                                                                              { _bodyOlev ->
                                                                              (case (True) of
                                                                               { _isTopTup ->
                                                                               (case (_isTopTup) of
                                                                                { _bodyOisTopTup ->
                                                                                (case (True) of
                                                                                 { _isTopApp ->
                                                                                 (case (_isTopApp) of
                                                                                  { _bodyOisTopApp ->
                                                                                  (case (_lhsIisStrict) of
                                                                                   { _bodyOisStrict ->
                                                                                   (case (_lhsIisLamBody) of
                                                                                    { _bodyOisLamBody ->
                                                                                    (case (_lhsIevalCtx) of
                                                                                     { _bodyOevalCtx ->
                                                                                     (case (_lhsIdataGam) of
                                                                                      { _bodyOdataGam ->
                                                                                      (case (_cvarMp) of
                                                                                       { _bodyOcvarMp ->
                                                                                       (case (body_1 _bodyOcvarMp _bodyOdataGam _bodyOevalCtx _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOmbLamNm _bodyOmoduleClassNm _bodyOopts _bodyOpkgNm _bodyOtopClassNm _bodyOwhatAbove ) of
                                                                                        { ( _bodyIappFunKind,_bodyIargUnpackWrapL,_bodyIgUniq,_bodyIjbinds,_bodyIjs,_bodyIjsArgFunL,_bodyIjsLamBody,_bodyIjstats,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar,_bodyImkFFI,_bodyIresPackWrap,_bodyIusedModNmS) ->
                                                                                            (case (_bodyIgUniq) of
                                                                                             { _lhsOgUniq ->
                                                                                             (case (_bindIjbinds `Seq.union` _bodyIjbinds) of
                                                                                              { _lhsOjbinds ->
                                                                                              (case (__tup7) of
                                                                                               { (_,__tup8) ->
                                                                                               (case (__tup8) of
                                                                                                { (_,_origLamNm) ->
                                                                                                (case (__tup8) of
                                                                                                 { (_lamNm,_) ->
                                                                                                 (case (__tup6) of
                                                                                                  { (_jsArgTyL,_,_) ->
                                                                                                  (case (if _hasFunHere
                                                                                                         then let as = [o | (o,_) <- _jsArgTyL]
                                                                                                                  v = mkHNm "_"
                                                                                                                  t1 x = if ehcOptGenTrace _lhsIopts
                                                                                                                         then Seq.unions [ jsTr (n ">" _lamNm) $ m $ [s " <- "] ++ intersperse (s ", ") (map p as)
                                                                                                                                         , x
                                                                                                                                         , Seq.singleton $ J.Stat_VarDecl v (Just _bodyIjsLamBody)
                                                                                                                                         , jsTr (n "<" _lamNm) $ m [s " -> ", s v]
                                                                                                                                         ]
                                                                                                                         else x
                                                                                                                       where m l = hlist $ intersperse (pp "+") l
                                                                                                                             s :: Show a => a -> PP_Doc
                                                                                                                             s x = pp (show x)
                                                                                                                             s' :: Show a => a -> PP_Doc
                                                                                                                             s' x = pp (show $ show x)
                                                                                                                             p x = pp x
                                                                                                                             n p x = p ++ show x
                                                                                                                  b = if ehcOptGenTrace _lhsIopts then jsVar v else _bodyIjsLamBody
                                                                                                              in  jsFun _origLamNm as $ jsBody J.Stat_Ret _bodyIjbinds (t1 _bodyIjstats) (Just b)
                                                                                                         else J.Expr_Str "*** ERR LAM ***") of
                                                                                                   { _js ->
                                                                                                   (case (_js) of
                                                                                                    { _lhsOjs ->
                                                                                                    (case ([_js]) of
                                                                                                     { _lhsOjsArgFunL ->
                                                                                                     (case (_bodyIjsLamBody) of
                                                                                                      { _jsLamBody ->
                                                                                                      (case (_jsLamBody) of
                                                                                                       { _lhsOjsLamBody ->
                                                                                                       (case (if _hasFunHere then Seq.empty else _bodyIjstats) of
                                                                                                        { _lhsOjstats ->
                                                                                                        (case (Nothing) of
                                                                                                         { _lhsOmbFFIApp ->
                                                                                                         (case (Just $ maybe [_argNm] (_argNm:) _bodyImbLam) of
                                                                                                          { _lhsOmbLam ->
                                                                                                          (case (Nothing) of
                                                                                                           { _lhsOmbVar ->
                                                                                                           (case (head) of
                                                                                                            { _lhsOmkFFI ->
                                                                                                            (case (id) of
                                                                                                             { _lhsOresPackWrap ->
                                                                                                             (case (_bindIusedModNmS `Set.union` _bodyIusedModNmS) of
                                                                                                              { _lhsOusedModNmS ->
                                                                                                              ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                           in  sem_CExpr_Lam_1)) of
                    { ( sem_CExpr_1) ->
                    ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) }) }) }) }) })
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Let_1 :: T_CExpr_1 
                   sem_CExpr_Let_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _bindsOgUniq ->
                               (case (binds_ ) of
                                { ( _bindsIbindNmL,binds_1) ->
                                    (case (_lhsItopClassNm) of
                                     { _bindsOtopClassNm ->
                                     (case (_lhsIpkgNm) of
                                      { _bindsOpkgNm ->
                                      (case (_lhsIopts) of
                                       { _bindsOopts ->
                                       (case (_lhsImoduleClassNm) of
                                        { _bindsOmoduleClassNm ->
                                        (case (categ_) of
                                         { _letBindingsCateg ->
                                         (case (_letBindingsCateg) of
                                          { _bindsOletBindingsCateg ->
                                          (case (_lhsIisLamBody) of
                                           { _bindsOisLamBody ->
                                           (case (_lhsIlev == cLevModule) of
                                            { _isGlobal ->
                                            (case (_isGlobal) of
                                             { _bindsOisGlobal ->
                                             (case (if categ_ == CBindCateg_Strict
                                                    then EvalCtx_Eval
                                                    else EvalCtx_None) of
                                              { _evalCtx ->
                                              (case (_evalCtx) of
                                               { _bindsOevalCtx ->
                                               (case (_lhsIdataGam) of
                                                { _bindsOdataGam ->
                                                (case (map (\x -> (x, hsnJavaScriptVar _isGlobal _lhsIpkgNm _lhsItopClassNm x)) _bindsIbindNmL) of
                                                 { _nmToRefAssocL ->
                                                 (case (if _isGlobal
                                                        then (0,Map.empty)
                                                        else let nLocals = length _bindsIbindNmL
                                                             in  ( nLocals
                                                                 , Map.fromList
                                                                     [ (n,CVarInfo_Local tyDefault o)
                                                                     | (n,o) <- _nmToRefAssocL
                                                                     ]
                                                                 )) of
                                                  { __tup10 ->
                                                  (case (__tup10) of
                                                   { (_,_cvarMpNew) ->
                                                   (case (_cvarMpNew `Map.union` _lhsIcvarMp) of
                                                    { _cvarMp ->
                                                    (case (_cvarMp) of
                                                     { _bindsOcvarMp ->
                                                     (case (_isGlobal || categ_ == CBindCateg_Strict) of
                                                      { _bindsOisStrict ->
                                                      (case (_lhsIlev + 1) of
                                                       { _bindsOlev ->
                                                       (case (binds_1 _bindsOcvarMp _bindsOdataGam _bindsOevalCtx _bindsOgUniq _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOletBindingsCateg _bindsOlev _bindsOmoduleClassNm _bindsOopts _bindsOpkgNm _bindsOtopClassNm ) of
                                                        { ( _bindsIgUniq,_bindsIjbinds,_bindsIusedModNmS) ->
                                                            (case (_bindsIgUniq) of
                                                             { _bodyOgUniq ->
                                                             (case (body_ ) of
                                                              { ( _bodyInmArgL,_bodyIwhatBelow,body_1) ->
                                                                  (case (ExprIsOther) of
                                                                   { _whatAbove ->
                                                                   (case (_whatAbove) of
                                                                    { _bodyOwhatAbove ->
                                                                    (case (_lhsItopClassNm) of
                                                                     { _bodyOtopClassNm ->
                                                                     (case (_lhsIpkgNm) of
                                                                      { _bodyOpkgNm ->
                                                                      (case (_lhsIopts) of
                                                                       { _bodyOopts ->
                                                                       (case (_lhsImoduleClassNm) of
                                                                        { _bodyOmoduleClassNm ->
                                                                        (case (_lhsImbLamNm) of
                                                                         { _bodyOmbLamNm ->
                                                                         (case (_lhsIlev) of
                                                                          { _bodyOlev ->
                                                                          (case (True) of
                                                                           { _isTopTup ->
                                                                           (case (_isTopTup) of
                                                                            { _bodyOisTopTup ->
                                                                            (case (True) of
                                                                             { _isTopApp ->
                                                                             (case (_isTopApp) of
                                                                              { _bodyOisTopApp ->
                                                                              (case (_lhsIisStrict) of
                                                                               { _bodyOisStrict ->
                                                                               (case (_lhsIisLamBody) of
                                                                                { _bodyOisLamBody ->
                                                                                (case (_evalCtx) of
                                                                                 { _bodyOevalCtx ->
                                                                                 (case (_lhsIdataGam) of
                                                                                  { _bodyOdataGam ->
                                                                                  (case (_cvarMp) of
                                                                                   { _bodyOcvarMp ->
                                                                                   (case (body_1 _bodyOcvarMp _bodyOdataGam _bodyOevalCtx _bodyOgUniq _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlev _bodyOmbLamNm _bodyOmoduleClassNm _bodyOopts _bodyOpkgNm _bodyOtopClassNm _bodyOwhatAbove ) of
                                                                                    { ( _bodyIappFunKind,_bodyIargUnpackWrapL,_bodyIgUniq,_bodyIjbinds,_bodyIjs,_bodyIjsArgFunL,_bodyIjsLamBody,_bodyIjstats,_bodyImbFFIApp,_bodyImbLam,_bodyImbVar,_bodyImkFFI,_bodyIresPackWrap,_bodyIusedModNmS) ->
                                                                                        (case (_bodyIgUniq) of
                                                                                         { _lhsOgUniq ->
                                                                                         (case (if _isGlobal
                                                                                                then ( _bindsIjbinds, Seq.empty )
                                                                                                else let binds = [ ((offof n),t,e) | JBind n _ t e _ <- Seq.toList _bindsIjbinds ]
                                                                                                         offof n = cvarOffset $ panicJust "ToJavaScript.CExpr.Let.js offset" $ Map.lookup n _cvarMpNew
                                                                                                         initbinds
                                                                                                               = case categ_ of
                                                                                                                   CBindCateg_Rec
                                                                                                                     ->             Seq.fromList [ J.Stat_VarDecl o $ Just $ jsCall nmInd [] | (o,_,_) <- binds ]
                                                                                                                        `Seq.union` Seq.fromList [ J.Stat_Expr $ jsCall nmIndSet [jsVar o, e] | (o,_,e) <- binds ]
                                                                                                                   _ -> Seq.fromList [ J.Stat_VarDecl o (Just e) | (o,_,e) <- binds ]
                                                                                                     in  ( Seq.empty, initbinds )) of
                                                                                          { __tup9 ->
                                                                                          (case (__tup9) of
                                                                                           { (_jbindsLet,_) ->
                                                                                           (case (_jbindsLet `Seq.union` _bodyIjbinds) of
                                                                                            { _lhsOjbinds ->
                                                                                            (case (_bodyIjs) of
                                                                                             { _js ->
                                                                                             (case (_js) of
                                                                                              { _lhsOjs ->
                                                                                              (case ([_js]) of
                                                                                               { _lhsOjsArgFunL ->
                                                                                               (case (_js) of
                                                                                                { _lhsOjsLamBody ->
                                                                                                (case (__tup9) of
                                                                                                 { (_,_jstatsLet) ->
                                                                                                 (case (_jstatsLet `Seq.union` _bodyIjstats) of
                                                                                                  { _lhsOjstats ->
                                                                                                  (case (Nothing) of
                                                                                                   { _lhsOmbFFIApp ->
                                                                                                   (case (Nothing) of
                                                                                                    { _lhsOmbLam ->
                                                                                                    (case (Nothing) of
                                                                                                     { _lhsOmbVar ->
                                                                                                     (case (head) of
                                                                                                      { _lhsOmkFFI ->
                                                                                                      (case (id) of
                                                                                                       { _lhsOresPackWrap ->
                                                                                                       (case (_bindsIusedModNmS `Set.union` _bodyIusedModNmS) of
                                                                                                        { _lhsOusedModNmS ->
                                                                                                        ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Let_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_String_1 :: T_CExpr_1 
                   sem_CExpr_String_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (J.Expr_Str  str_) of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_String_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_Tup_1 :: T_CExpr_1 
                   sem_CExpr_Tup_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_Tag tag_) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _lhsOgUniq ->
                               (case (Seq.empty) of
                                { _lhsOjbinds ->
                                (case (jsNewTupOrData _lhsIdataGam _lhsIpkgNm _lhsItopClassNm tag_ []) of
                                 { _js ->
                                 (case (_js) of
                                  { _lhsOjs ->
                                  (case ([_js]) of
                                   { _lhsOjsArgFunL ->
                                   (case (_js) of
                                    { _lhsOjsLamBody ->
                                    (case (Seq.empty) of
                                     { _lhsOjstats ->
                                     (case (Nothing) of
                                      { _lhsOmbFFIApp ->
                                      (case (Nothing) of
                                       { _lhsOmbLam ->
                                       (case (Nothing) of
                                        { _lhsOmbVar ->
                                        (case (head) of
                                         { _lhsOmkFFI ->
                                         (case (id) of
                                          { _lhsOresPackWrap ->
                                          (case (Set.empty) of
                                           { _lhsOusedModNmS ->
                                           ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_Tup_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_TupDel_1 :: T_CExpr_1 
                   sem_CExpr_TupDel_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _exprOgUniq ->
                               (case (expr_ ) of
                                { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                                    (case (ExprIsOther) of
                                     { _whatAbove ->
                                     (case (_whatAbove) of
                                      { _exprOwhatAbove ->
                                      (case (_lhsItopClassNm) of
                                       { _exprOtopClassNm ->
                                       (case (_lhsIpkgNm) of
                                        { _exprOpkgNm ->
                                        (case (_lhsIopts) of
                                         { _exprOopts ->
                                         (case (_lhsImoduleClassNm) of
                                          { _exprOmoduleClassNm ->
                                          (case (_lhsImbLamNm) of
                                           { _exprOmbLamNm ->
                                           (case (_lhsIlev) of
                                            { _exprOlev ->
                                            (case (True) of
                                             { _isTopApp ->
                                             (case (_isTopApp) of
                                              { _exprOisTopApp ->
                                              (case (_lhsIisStrict) of
                                               { _exprOisStrict ->
                                               (case (_lhsIisLamBody) of
                                                { _exprOisLamBody ->
                                                (case (_lhsIevalCtx) of
                                                 { _exprOevalCtx ->
                                                 (case (_lhsIdataGam) of
                                                  { _exprOdataGam ->
                                                  (case (_lhsIcvarMp) of
                                                   { _exprOcvarMp ->
                                                   (case (False) of
                                                    { _exprOisTopTup ->
                                                    (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                                     { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                         (case (_exprIgUniq) of
                                                          { _offsetOgUniq ->
                                                          (case (offset_ ) of
                                                           { ( _offsetInmArgL,_offsetIwhatBelow,offset_1) ->
                                                               (case (_whatAbove) of
                                                                { _offsetOwhatAbove ->
                                                                (case (_lhsItopClassNm) of
                                                                 { _offsetOtopClassNm ->
                                                                 (case (_lhsIpkgNm) of
                                                                  { _offsetOpkgNm ->
                                                                  (case (_lhsIopts) of
                                                                   { _offsetOopts ->
                                                                   (case (_lhsImoduleClassNm) of
                                                                    { _offsetOmoduleClassNm ->
                                                                    (case (_lhsImbLamNm) of
                                                                     { _offsetOmbLamNm ->
                                                                     (case (_lhsIlev) of
                                                                      { _offsetOlev ->
                                                                      (case (True) of
                                                                       { _isTopTup ->
                                                                       (case (_isTopTup) of
                                                                        { _offsetOisTopTup ->
                                                                        (case (_isTopApp) of
                                                                         { _offsetOisTopApp ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _offsetOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _offsetOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _offsetOevalCtx ->
                                                                            (case (_lhsIdataGam) of
                                                                             { _offsetOdataGam ->
                                                                             (case (_lhsIcvarMp) of
                                                                              { _offsetOcvarMp ->
                                                                              (case (offset_1 _offsetOcvarMp _offsetOdataGam _offsetOevalCtx _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOmbLamNm _offsetOmoduleClassNm _offsetOopts _offsetOpkgNm _offsetOtopClassNm _offsetOwhatAbove ) of
                                                                               { ( _offsetIappFunKind,_offsetIargUnpackWrapL,_offsetIgUniq,_offsetIjbinds,_offsetIjs,_offsetIjsArgFunL,_offsetIjsLamBody,_offsetIjstats,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar,_offsetImkFFI,_offsetIresPackWrap,_offsetIusedModNmS) ->
                                                                                   (case (_offsetIgUniq) of
                                                                                    { _lhsOgUniq ->
                                                                                    (case (_exprIjbinds `Seq.union` _offsetIjbinds) of
                                                                                     { _lhsOjbinds ->
                                                                                     (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                                                                      { _js ->
                                                                                      (case (_js) of
                                                                                       { _lhsOjs ->
                                                                                       (case ([_js]) of
                                                                                        { _lhsOjsArgFunL ->
                                                                                        (case (_js) of
                                                                                         { _lhsOjsLamBody ->
                                                                                         (case (Seq.empty) of
                                                                                          { _lhsOjstats ->
                                                                                          (case (Nothing) of
                                                                                           { _lhsOmbFFIApp ->
                                                                                           (case (Nothing) of
                                                                                            { _lhsOmbLam ->
                                                                                            (case (Nothing) of
                                                                                             { _lhsOmbVar ->
                                                                                             (case (head) of
                                                                                              { _lhsOmkFFI ->
                                                                                              (case (id) of
                                                                                               { _lhsOresPackWrap ->
                                                                                               (case (_exprIusedModNmS `Set.union` _offsetIusedModNmS) of
                                                                                                { _lhsOusedModNmS ->
                                                                                                ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_TupDel_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_TupIns_1 :: T_CExpr_1 
                   sem_CExpr_TupIns_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _exprOgUniq ->
                               (case (expr_ ) of
                                { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                                    (case (ExprIsOther) of
                                     { _whatAbove ->
                                     (case (_whatAbove) of
                                      { _exprOwhatAbove ->
                                      (case (_lhsItopClassNm) of
                                       { _exprOtopClassNm ->
                                       (case (_lhsIpkgNm) of
                                        { _exprOpkgNm ->
                                        (case (_lhsIopts) of
                                         { _exprOopts ->
                                         (case (_lhsImoduleClassNm) of
                                          { _exprOmoduleClassNm ->
                                          (case (_lhsImbLamNm) of
                                           { _exprOmbLamNm ->
                                           (case (_lhsIlev) of
                                            { _exprOlev ->
                                            (case (True) of
                                             { _isTopApp ->
                                             (case (_isTopApp) of
                                              { _exprOisTopApp ->
                                              (case (_lhsIisStrict) of
                                               { _exprOisStrict ->
                                               (case (_lhsIisLamBody) of
                                                { _exprOisLamBody ->
                                                (case (_lhsIevalCtx) of
                                                 { _exprOevalCtx ->
                                                 (case (_lhsIdataGam) of
                                                  { _exprOdataGam ->
                                                  (case (_lhsIcvarMp) of
                                                   { _exprOcvarMp ->
                                                   (case (False) of
                                                    { _exprOisTopTup ->
                                                    (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                                     { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                         (case (_exprIgUniq) of
                                                          { _offsetOgUniq ->
                                                          (case (offset_ ) of
                                                           { ( _offsetInmArgL,_offsetIwhatBelow,offset_1) ->
                                                               (case (_whatAbove) of
                                                                { _offsetOwhatAbove ->
                                                                (case (_lhsItopClassNm) of
                                                                 { _offsetOtopClassNm ->
                                                                 (case (_lhsIpkgNm) of
                                                                  { _offsetOpkgNm ->
                                                                  (case (_lhsIopts) of
                                                                   { _offsetOopts ->
                                                                   (case (_lhsImoduleClassNm) of
                                                                    { _offsetOmoduleClassNm ->
                                                                    (case (_lhsImbLamNm) of
                                                                     { _offsetOmbLamNm ->
                                                                     (case (_lhsIlev) of
                                                                      { _offsetOlev ->
                                                                      (case (True) of
                                                                       { _isTopTup ->
                                                                       (case (_isTopTup) of
                                                                        { _offsetOisTopTup ->
                                                                        (case (_isTopApp) of
                                                                         { _offsetOisTopApp ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _offsetOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _offsetOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _offsetOevalCtx ->
                                                                            (case (_lhsIdataGam) of
                                                                             { _offsetOdataGam ->
                                                                             (case (_lhsIcvarMp) of
                                                                              { _offsetOcvarMp ->
                                                                              (case (offset_1 _offsetOcvarMp _offsetOdataGam _offsetOevalCtx _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOmbLamNm _offsetOmoduleClassNm _offsetOopts _offsetOpkgNm _offsetOtopClassNm _offsetOwhatAbove ) of
                                                                               { ( _offsetIappFunKind,_offsetIargUnpackWrapL,_offsetIgUniq,_offsetIjbinds,_offsetIjs,_offsetIjsArgFunL,_offsetIjsLamBody,_offsetIjstats,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar,_offsetImkFFI,_offsetIresPackWrap,_offsetIusedModNmS) ->
                                                                                   (case (_offsetIgUniq) of
                                                                                    { _fldExprOgUniq ->
                                                                                    (case (fldExpr_ ) of
                                                                                     { ( _fldExprInmArgL,_fldExprIwhatBelow,fldExpr_1) ->
                                                                                         (case (_whatAbove) of
                                                                                          { _fldExprOwhatAbove ->
                                                                                          (case (_lhsItopClassNm) of
                                                                                           { _fldExprOtopClassNm ->
                                                                                           (case (_lhsIpkgNm) of
                                                                                            { _fldExprOpkgNm ->
                                                                                            (case (_lhsIopts) of
                                                                                             { _fldExprOopts ->
                                                                                             (case (_lhsImoduleClassNm) of
                                                                                              { _fldExprOmoduleClassNm ->
                                                                                              (case (_lhsImbLamNm) of
                                                                                               { _fldExprOmbLamNm ->
                                                                                               (case (_lhsIlev) of
                                                                                                { _fldExprOlev ->
                                                                                                (case (_isTopTup) of
                                                                                                 { _fldExprOisTopTup ->
                                                                                                 (case (_isTopApp) of
                                                                                                  { _fldExprOisTopApp ->
                                                                                                  (case (_lhsIisStrict) of
                                                                                                   { _fldExprOisStrict ->
                                                                                                   (case (_lhsIisLamBody) of
                                                                                                    { _fldExprOisLamBody ->
                                                                                                    (case (_lhsIevalCtx) of
                                                                                                     { _fldExprOevalCtx ->
                                                                                                     (case (_lhsIdataGam) of
                                                                                                      { _fldExprOdataGam ->
                                                                                                      (case (_lhsIcvarMp) of
                                                                                                       { _fldExprOcvarMp ->
                                                                                                       (case (fldExpr_1 _fldExprOcvarMp _fldExprOdataGam _fldExprOevalCtx _fldExprOgUniq _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlev _fldExprOmbLamNm _fldExprOmoduleClassNm _fldExprOopts _fldExprOpkgNm _fldExprOtopClassNm _fldExprOwhatAbove ) of
                                                                                                        { ( _fldExprIappFunKind,_fldExprIargUnpackWrapL,_fldExprIgUniq,_fldExprIjbinds,_fldExprIjs,_fldExprIjsArgFunL,_fldExprIjsLamBody,_fldExprIjstats,_fldExprImbFFIApp,_fldExprImbLam,_fldExprImbVar,_fldExprImkFFI,_fldExprIresPackWrap,_fldExprIusedModNmS) ->
                                                                                                            (case (_fldExprIgUniq) of
                                                                                                             { _lhsOgUniq ->
                                                                                                             (case (_exprIjbinds `Seq.union` _offsetIjbinds `Seq.union` _fldExprIjbinds) of
                                                                                                              { _lhsOjbinds ->
                                                                                                              (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                                                                                               { _js ->
                                                                                                               (case (_js) of
                                                                                                                { _lhsOjs ->
                                                                                                                (case ([_js]) of
                                                                                                                 { _lhsOjsArgFunL ->
                                                                                                                 (case (_js) of
                                                                                                                  { _lhsOjsLamBody ->
                                                                                                                  (case (Seq.empty) of
                                                                                                                   { _lhsOjstats ->
                                                                                                                   (case (Nothing) of
                                                                                                                    { _lhsOmbFFIApp ->
                                                                                                                    (case (Nothing) of
                                                                                                                     { _lhsOmbLam ->
                                                                                                                     (case (Nothing) of
                                                                                                                      { _lhsOmbVar ->
                                                                                                                      (case (head) of
                                                                                                                       { _lhsOmkFFI ->
                                                                                                                       (case (id) of
                                                                                                                        { _lhsOresPackWrap ->
                                                                                                                        (case (_exprIusedModNmS `Set.union` _offsetIusedModNmS `Set.union` _fldExprIusedModNmS) of
                                                                                                                         { _lhsOusedModNmS ->
                                                                                                                         ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_TupIns_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (ExprIsOther) of
      { _whatBelow ->
      (case (_whatBelow) of
       { _lhsOwhatBelow ->
       (case ((let sem_CExpr_TupUpd_1 :: T_CExpr_1 
                   sem_CExpr_TupUpd_1  =
                       (\ _lhsIcvarMp
                          _lhsIdataGam
                          _lhsIevalCtx
                          _lhsIgUniq
                          _lhsIisLamBody
                          _lhsIisStrict
                          _lhsIisTopApp
                          _lhsIisTopTup
                          _lhsIlev
                          _lhsImbLamNm
                          _lhsImoduleClassNm
                          _lhsIopts
                          _lhsIpkgNm
                          _lhsItopClassNm
                          _lhsIwhatAbove ->
                            (case (AppFunKind_NoApp) of
                             { _lhsOappFunKind ->
                             (case ([]) of
                              { _lhsOargUnpackWrapL ->
                              (case (_lhsIgUniq) of
                               { _exprOgUniq ->
                               (case (expr_ ) of
                                { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
                                    (case (ExprIsOther) of
                                     { _whatAbove ->
                                     (case (_whatAbove) of
                                      { _exprOwhatAbove ->
                                      (case (_lhsItopClassNm) of
                                       { _exprOtopClassNm ->
                                       (case (_lhsIpkgNm) of
                                        { _exprOpkgNm ->
                                        (case (_lhsIopts) of
                                         { _exprOopts ->
                                         (case (_lhsImoduleClassNm) of
                                          { _exprOmoduleClassNm ->
                                          (case (_lhsImbLamNm) of
                                           { _exprOmbLamNm ->
                                           (case (_lhsIlev) of
                                            { _exprOlev ->
                                            (case (True) of
                                             { _isTopApp ->
                                             (case (_isTopApp) of
                                              { _exprOisTopApp ->
                                              (case (_lhsIisStrict) of
                                               { _exprOisStrict ->
                                               (case (_lhsIisLamBody) of
                                                { _exprOisLamBody ->
                                                (case (_lhsIevalCtx) of
                                                 { _exprOevalCtx ->
                                                 (case (_lhsIdataGam) of
                                                  { _exprOdataGam ->
                                                  (case (_lhsIcvarMp) of
                                                   { _exprOcvarMp ->
                                                   (case (False) of
                                                    { _exprOisTopTup ->
                                                    (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                                     { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                                         (case (_exprIgUniq) of
                                                          { _offsetOgUniq ->
                                                          (case (offset_ ) of
                                                           { ( _offsetInmArgL,_offsetIwhatBelow,offset_1) ->
                                                               (case (_whatAbove) of
                                                                { _offsetOwhatAbove ->
                                                                (case (_lhsItopClassNm) of
                                                                 { _offsetOtopClassNm ->
                                                                 (case (_lhsIpkgNm) of
                                                                  { _offsetOpkgNm ->
                                                                  (case (_lhsIopts) of
                                                                   { _offsetOopts ->
                                                                   (case (_lhsImoduleClassNm) of
                                                                    { _offsetOmoduleClassNm ->
                                                                    (case (_lhsImbLamNm) of
                                                                     { _offsetOmbLamNm ->
                                                                     (case (_lhsIlev) of
                                                                      { _offsetOlev ->
                                                                      (case (True) of
                                                                       { _isTopTup ->
                                                                       (case (_isTopTup) of
                                                                        { _offsetOisTopTup ->
                                                                        (case (_isTopApp) of
                                                                         { _offsetOisTopApp ->
                                                                         (case (_lhsIisStrict) of
                                                                          { _offsetOisStrict ->
                                                                          (case (_lhsIisLamBody) of
                                                                           { _offsetOisLamBody ->
                                                                           (case (_lhsIevalCtx) of
                                                                            { _offsetOevalCtx ->
                                                                            (case (_lhsIdataGam) of
                                                                             { _offsetOdataGam ->
                                                                             (case (_lhsIcvarMp) of
                                                                              { _offsetOcvarMp ->
                                                                              (case (offset_1 _offsetOcvarMp _offsetOdataGam _offsetOevalCtx _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOmbLamNm _offsetOmoduleClassNm _offsetOopts _offsetOpkgNm _offsetOtopClassNm _offsetOwhatAbove ) of
                                                                               { ( _offsetIappFunKind,_offsetIargUnpackWrapL,_offsetIgUniq,_offsetIjbinds,_offsetIjs,_offsetIjsArgFunL,_offsetIjsLamBody,_offsetIjstats,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar,_offsetImkFFI,_offsetIresPackWrap,_offsetIusedModNmS) ->
                                                                                   (case (_offsetIgUniq) of
                                                                                    { _fldExprOgUniq ->
                                                                                    (case (fldExpr_ ) of
                                                                                     { ( _fldExprInmArgL,_fldExprIwhatBelow,fldExpr_1) ->
                                                                                         (case (_whatAbove) of
                                                                                          { _fldExprOwhatAbove ->
                                                                                          (case (_lhsItopClassNm) of
                                                                                           { _fldExprOtopClassNm ->
                                                                                           (case (_lhsIpkgNm) of
                                                                                            { _fldExprOpkgNm ->
                                                                                            (case (_lhsIopts) of
                                                                                             { _fldExprOopts ->
                                                                                             (case (_lhsImoduleClassNm) of
                                                                                              { _fldExprOmoduleClassNm ->
                                                                                              (case (_lhsImbLamNm) of
                                                                                               { _fldExprOmbLamNm ->
                                                                                               (case (_lhsIlev) of
                                                                                                { _fldExprOlev ->
                                                                                                (case (_isTopTup) of
                                                                                                 { _fldExprOisTopTup ->
                                                                                                 (case (_isTopApp) of
                                                                                                  { _fldExprOisTopApp ->
                                                                                                  (case (_lhsIisStrict) of
                                                                                                   { _fldExprOisStrict ->
                                                                                                   (case (_lhsIisLamBody) of
                                                                                                    { _fldExprOisLamBody ->
                                                                                                    (case (_lhsIevalCtx) of
                                                                                                     { _fldExprOevalCtx ->
                                                                                                     (case (_lhsIdataGam) of
                                                                                                      { _fldExprOdataGam ->
                                                                                                      (case (_lhsIcvarMp) of
                                                                                                       { _fldExprOcvarMp ->
                                                                                                       (case (fldExpr_1 _fldExprOcvarMp _fldExprOdataGam _fldExprOevalCtx _fldExprOgUniq _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlev _fldExprOmbLamNm _fldExprOmoduleClassNm _fldExprOopts _fldExprOpkgNm _fldExprOtopClassNm _fldExprOwhatAbove ) of
                                                                                                        { ( _fldExprIappFunKind,_fldExprIargUnpackWrapL,_fldExprIgUniq,_fldExprIjbinds,_fldExprIjs,_fldExprIjsArgFunL,_fldExprIjsLamBody,_fldExprIjstats,_fldExprImbFFIApp,_fldExprImbLam,_fldExprImbVar,_fldExprImkFFI,_fldExprIresPackWrap,_fldExprIusedModNmS) ->
                                                                                                            (case (_fldExprIgUniq) of
                                                                                                             { _lhsOgUniq ->
                                                                                                             (case (_exprIjbinds `Seq.union` _offsetIjbinds `Seq.union` _fldExprIjbinds) of
                                                                                                              { _lhsOjbinds ->
                                                                                                              (case (J.Expr_Str "*** TODO: CExpr | loc.js ***") of
                                                                                                               { _js ->
                                                                                                               (case (_js) of
                                                                                                                { _lhsOjs ->
                                                                                                                (case ([_js]) of
                                                                                                                 { _lhsOjsArgFunL ->
                                                                                                                 (case (_js) of
                                                                                                                  { _lhsOjsLamBody ->
                                                                                                                  (case (Seq.empty) of
                                                                                                                   { _lhsOjstats ->
                                                                                                                   (case (Nothing) of
                                                                                                                    { _lhsOmbFFIApp ->
                                                                                                                    (case (Nothing) of
                                                                                                                     { _lhsOmbLam ->
                                                                                                                     (case (Nothing) of
                                                                                                                      { _lhsOmbVar ->
                                                                                                                      (case (head) of
                                                                                                                       { _lhsOmkFFI ->
                                                                                                                       (case (id) of
                                                                                                                        { _lhsOresPackWrap ->
                                                                                                                        (case (_exprIusedModNmS `Set.union` _offsetIusedModNmS `Set.union` _fldExprIusedModNmS) of
                                                                                                                         { _lhsOusedModNmS ->
                                                                                                                         ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
               in  sem_CExpr_TupUpd_1)) of
        { ( sem_CExpr_1) ->
        ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) })
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (case ([]) of
     { _lhsOnmArgL ->
     (case (acbrefNm ref_) of
      { _nm ->
      (case (ExprIsVar _nm) of
       { _whatBelow ->
       (case (_whatBelow) of
        { _lhsOwhatBelow ->
        (case ((let sem_CExpr_Var_1 :: T_CExpr_1 
                    sem_CExpr_Var_1  =
                        (\ _lhsIcvarMp
                           _lhsIdataGam
                           _lhsIevalCtx
                           _lhsIgUniq
                           _lhsIisLamBody
                           _lhsIisStrict
                           _lhsIisTopApp
                           _lhsIisTopTup
                           _lhsIlev
                           _lhsImbLamNm
                           _lhsImoduleClassNm
                           _lhsIopts
                           _lhsIpkgNm
                           _lhsItopClassNm
                           _lhsIwhatAbove ->
                             (case (AppFunKind_Fun ref_) of
                              { _lhsOappFunKind ->
                              (case ([]) of
                               { _lhsOargUnpackWrapL ->
                               (case (_lhsIgUniq) of
                                { _lhsOgUniq ->
                                (case (Seq.empty) of
                                 { _lhsOjbinds ->
                                 (case (hsnJavaScriptVar True _lhsIpkgNm _lhsItopClassNm _nm) of
                                  { _varnm ->
                                  (case (Map.findWithDefault (cvarGlob tyDefault _lhsImoduleClassNm _nm _varnm) _nm _lhsIcvarMp) of
                                   { _cvi ->
                                   (case (jvRef _lhsIcvarMp _cvi) of
                                    { _js ->
                                    (case (_js) of
                                     { _lhsOjs ->
                                     (case ([_js]) of
                                      { _lhsOjsArgFunL ->
                                      (case (_js) of
                                       { _lhsOjsLamBody ->
                                       (case (Seq.empty) of
                                        { _lhsOjstats ->
                                        (case (Nothing) of
                                         { _lhsOmbFFIApp ->
                                         (case (Nothing) of
                                          { _lhsOmbLam ->
                                          (case (Just _nm) of
                                           { _mbVar ->
                                           (case (_mbVar) of
                                            { _lhsOmbVar ->
                                            (case (head) of
                                             { _lhsOmkFFI ->
                                             (case (id) of
                                              { _lhsOresPackWrap ->
                                              (case (Set.empty) of
                                               { _lhsOusedModNmS ->
                                               ( _lhsOappFunKind,_lhsOargUnpackWrapL,_lhsOgUniq,_lhsOjbinds,_lhsOjs,_lhsOjsArgFunL,_lhsOjsLamBody,_lhsOjstats,_lhsOmbFFIApp,_lhsOmbLam,_lhsOmbVar,_lhsOmkFFI,_lhsOresPackWrap,_lhsOusedModNmS) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                in  sem_CExpr_Var_1)) of
         { ( sem_CExpr_1) ->
         ( _lhsOnmArgL,_lhsOwhatBelow,sem_CExpr_1) }) }) }) }) })
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
      alternative Debug:
         child info           : {String}
      alternative Ty:
         child ty             : {Ty}
-}
-- cata
sem_CExprAnn :: CExprAnn  ->
                T_CExprAnn 
sem_CExprAnn (CExprAnn_Coe _coe )  =
    (sem_CExprAnn_Coe _coe )
sem_CExprAnn (CExprAnn_Debug _info )  =
    (sem_CExprAnn_Debug _info )
sem_CExprAnn (CExprAnn_Ty _ty )  =
    (sem_CExprAnn_Ty _ty )
-- semantic domain
type T_CExprAnn  = CVarMp ->
                   DataGam ->
                   UID ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   HsName ->
                   HsName ->
                   ( UID)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
   alternatives:
      alternative Apply0:
      alternative Function0:
      alternative Function1:
      alternative Plain:
-}
-- cata
sem_CMetaBind :: CMetaBind  ->
                 T_CMetaBind 
sem_CMetaBind (CMetaBind_Apply0 )  =
    (sem_CMetaBind_Apply0 )
sem_CMetaBind (CMetaBind_Function0 )  =
    (sem_CMetaBind_Function0 )
sem_CMetaBind (CMetaBind_Function1 )  =
    (sem_CMetaBind_Function1 )
sem_CMetaBind (CMetaBind_Plain )  =
    (sem_CMetaBind_Plain )
-- semantic domain
type T_CMetaBind  = CVarMp ->
                    DataGam ->
                    UID ->
                    Int ->
                    HsName ->
                    EHCOpts ->
                    HsName ->
                    HsName ->
                    ( UID)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
   alternatives:
      alternative Dict:
      alternative DictClass:
         child tracks         : {[Track]}
      alternative DictInstance:
         child tracks         : {[Track]}
      alternative Track:
         child track          : {Track}
      alternative Val:
-}
-- cata
sem_CMetaVal :: CMetaVal  ->
                T_CMetaVal 
sem_CMetaVal (CMetaVal_Dict )  =
    (sem_CMetaVal_Dict )
sem_CMetaVal (CMetaVal_DictClass _tracks )  =
    (sem_CMetaVal_DictClass _tracks )
sem_CMetaVal (CMetaVal_DictInstance _tracks )  =
    (sem_CMetaVal_DictInstance _tracks )
sem_CMetaVal (CMetaVal_Track _track )  =
    (sem_CMetaVal_Track _track )
sem_CMetaVal (CMetaVal_Val )  =
    (sem_CMetaVal_Val )
-- semantic domain
type T_CMetaVal  = CVarMp ->
                   DataGam ->
                   UID ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   HsName ->
                   HsName ->
                   ( UID)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = CVarMp ->
                 DataGam ->
                 UID ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 HsName ->
                 HsName ->
                 ( UID)
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _x1OgUniq ->
          (case (_lhsItopClassNm) of
           { _x1OtopClassNm ->
           (case (_lhsIpkgNm) of
            { _x1OpkgNm ->
            (case (_lhsIopts) of
             { _x1Oopts ->
             (case (_lhsImoduleClassNm) of
              { _x1OmoduleClassNm ->
              (case (_lhsIlev) of
               { _x1Olev ->
               (case (_lhsIdataGam) of
                { _x1OdataGam ->
                (case (_lhsIcvarMp) of
                 { _x1OcvarMp ->
                 (case (x1_ _x1OcvarMp _x1OdataGam _x1OgUniq _x1Olev _x1OmoduleClassNm _x1Oopts _x1OpkgNm _x1OtopClassNm ) of
                  { ( _x1IgUniq) ->
                      (case (_x1IgUniq) of
                       { _x2OgUniq ->
                       (case (_lhsItopClassNm) of
                        { _x2OtopClassNm ->
                        (case (_lhsIpkgNm) of
                         { _x2OpkgNm ->
                         (case (_lhsIopts) of
                          { _x2Oopts ->
                          (case (_lhsImoduleClassNm) of
                           { _x2OmoduleClassNm ->
                           (case (_lhsIlev) of
                            { _x2Olev ->
                            (case (_lhsIdataGam) of
                             { _x2OdataGam ->
                             (case (_lhsIcvarMp) of
                              { _x2OcvarMp ->
                              (case (x2_ _x2OcvarMp _x2OdataGam _x2OgUniq _x2Olev _x2OmoduleClassNm _x2Oopts _x2OpkgNm _x2OtopClassNm ) of
                               { ( _x2IgUniq) ->
                                   (case (_x2IgUniq) of
                                    { _lhsOgUniq ->
                                    ( _lhsOgUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         js                   : J.JavaScriptModule
         moduleClassNm        : HsName
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local whatAbove   : {WhatExpr}
            local topClassNm  : _
            local pkgNm       : _
            local moduleClassNm : _
            local jsModTraceStats : _
            local jsModInitStats : _
            local jmodStatsMain : _
            local jmodStats   : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = CVarMp ->
                  DataGam ->
                  UID ->
                  Int ->
                  EHCOpts ->
                  HsName ->
                  HsName ->
                  ( UID,(J.JavaScriptModule),HsName)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _exprOgUniq ->
          (case (expr_ ) of
           { ( _exprInmArgL,_exprIwhatBelow,expr_1) ->
               (case (ExprIsOther) of
                { _whatAbove ->
                (case (_whatAbove) of
                 { _exprOwhatAbove ->
                 (case (moduleNm_) of
                  { _topClassNm ->
                  (case (_topClassNm) of
                   { _exprOtopClassNm ->
                   (case (moduleNm_) of
                    { _pkgNm ->
                    (case (_pkgNm) of
                     { _exprOpkgNm ->
                     (case (_lhsIopts) of
                      { _exprOopts ->
                      (case (hsnSetQual moduleNm_ $ hsnQualified moduleNm_) of
                       { _moduleClassNm ->
                       (case (_moduleClassNm) of
                        { _exprOmoduleClassNm ->
                        (case (_lhsIlev) of
                         { _exprOlev ->
                         (case (_lhsIdataGam) of
                          { _exprOdataGam ->
                          (case (_lhsIcvarMp) of
                           { _exprOcvarMp ->
                           (case (EvalCtx_Eval) of
                            { _exprOevalCtx ->
                            (case (False) of
                             { _exprOisLamBody ->
                             (case (True) of
                              { _exprOisStrict ->
                              (case (True) of
                               { _exprOisTopTup ->
                               (case (True) of
                                { _exprOisTopApp ->
                                (case (Nothing) of
                                 { _exprOmbLamNm ->
                                 (case (expr_1 _exprOcvarMp _exprOdataGam _exprOevalCtx _exprOgUniq _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlev _exprOmbLamNm _exprOmoduleClassNm _exprOopts _exprOpkgNm _exprOtopClassNm _exprOwhatAbove ) of
                                  { ( _exprIappFunKind,_exprIargUnpackWrapL,_exprIgUniq,_exprIjbinds,_exprIjs,_exprIjsArgFunL,_exprIjsLamBody,_exprIjstats,_exprImbFFIApp,_exprImbLam,_exprImbVar,_exprImkFFI,_exprIresPackWrap,_exprIusedModNmS) ->
                                      (case (_exprIgUniq) of
                                       { _lhsOgUniq ->
                                       (case ([]) of
                                        { _jsModTraceStats ->
                                        (case (let prefixes = nub . catMaybes . map hsnQualifier
                                                   mk  n = J.Expr_If (jsVar n) (jsVar n) (J.Expr_Obj [])
                                                   mkd n e | isJust q  = jsAssign n e
                                                           | otherwise = J.Stat_VarDecl n (Just e)
                                                     where q = hsnQualifier n
                                               in  map (\n -> mkd n $ mk n) $ concat $ reverse $ takeWhile (not.null) $ iterate prefixes $ Set.toList _exprIusedModNmS) of
                                         { _jsModInitStats ->
                                         (case ([ J.Stat_Expr $ jsEvl $
                                                  (\m -> jsApp m [J.Expr_Arr []]) $
                                                  _exprIjs
                                                ]) of
                                          { _jmodStatsMain ->
                                          (case (jsBody J.Stat_Expr _exprIjbinds _exprIjstats Nothing) of
                                           { _jmodStats ->
                                           (case (J.JavaScriptModule_Mod (_jsModTraceStats ++ _jsModInitStats ++ _jmodStats) _jmodStatsMain) of
                                            { _lhsOjs ->
                                            (case (_moduleClassNm) of
                                             { _lhsOmoduleClassNm ->
                                             ( _lhsOgUniq,_lhsOjs,_lhsOmoduleClassNm) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         scrutinees           : [Scrutinee]
   visit 1:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         fldNmL               : [HsName]
         offsetBinds          : [(HsName,CVarInfo,J.Expr)]
         patCVarMp            : CVarMp
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
      alternative Char:
         child char           : {Char}
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
      alternative Int:
         child int            : {Int}
      alternative Var:
         child pnm            : {HsName}
-}
-- cata
sem_CPat :: CPat  ->
            T_CPat 
sem_CPat (CPat_BoolExpr _cexpr )  =
    (sem_CPat_BoolExpr _cexpr )
sem_CPat (CPat_Char _char )  =
    (sem_CPat_Char _char )
sem_CPat (CPat_Con _tag _rest _binds )  =
    (sem_CPat_Con _tag (sem_CPatRest _rest ) (sem_CPatFldL _binds ) )
sem_CPat (CPat_Int _int )  =
    (sem_CPat_Int _int )
sem_CPat (CPat_Var _pnm )  =
    (sem_CPat_Var _pnm )
-- semantic domain
type T_CPat  = ( ([Scrutinee]),T_CPat_1 )
type T_CPat_1  = CVarMp ->
                 DataGam ->
                 UID ->
                 Int ->
                 HsName ->
                 EHCOpts ->
                 HsName ->
                 CVarInfo ->
                 HsName ->
                 ( ([HsName]),UID,([(HsName,CVarInfo,J.Expr)]),CVarMp)
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (case ([Scrutinee_Other "other"]) of
     { _lhsOscrutinees ->
     (case ((let sem_CPat_BoolExpr_1 :: T_CPat_1 
                 sem_CPat_BoolExpr_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIgUniq
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsIscrutineeCVarInfo
                        _lhsItopClassNm ->
                          (case ([]) of
                           { _lhsOfldNmL ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case ([]) of
                             { _lhsOoffsetBinds ->
                             (case (Map.empty) of
                              { _lhsOpatCVarMp ->
                              ( _lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }))
             in  sem_CPat_BoolExpr_1)) of
      { ( sem_CPat_1) ->
      ( _lhsOscrutinees,sem_CPat_1) }) })
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (case ([Scrutinee_Other "char"]) of
     { _lhsOscrutinees ->
     (case ((let sem_CPat_Char_1 :: T_CPat_1 
                 sem_CPat_Char_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIgUniq
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsIscrutineeCVarInfo
                        _lhsItopClassNm ->
                          (case ([]) of
                           { _lhsOfldNmL ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case ([]) of
                             { _lhsOoffsetBinds ->
                             (case (Map.empty) of
                              { _lhsOpatCVarMp ->
                              ( _lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }))
             in  sem_CPat_Char_1)) of
      { ( sem_CPat_1) ->
      ( _lhsOscrutinees,sem_CPat_1) }) })
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (case ([Scrutinee_Tag tag_]) of
     { _lhsOscrutinees ->
     (case ((let sem_CPat_Con_1 :: T_CPat_1 
                 sem_CPat_Con_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIgUniq
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsIscrutineeCVarInfo
                        _lhsItopClassNm ->
                          (case (_lhsItopClassNm) of
                           { _bindsOtopClassNm ->
                           (case (_lhsIscrutineeCVarInfo) of
                            { _bindsOscrutineeCVarInfo ->
                            (case (_lhsIpkgNm) of
                             { _bindsOpkgNm ->
                             (case (_lhsIopts) of
                              { _bindsOopts ->
                              (case (_lhsImoduleClassNm) of
                               { _bindsOmoduleClassNm ->
                               (case (_lhsIlev) of
                                { _bindsOlev ->
                                (case (_lhsIgUniq) of
                                 { _restOgUniq ->
                                 (case (_lhsItopClassNm) of
                                  { _restOtopClassNm ->
                                  (case (_lhsIscrutineeCVarInfo) of
                                   { _restOscrutineeCVarInfo ->
                                   (case (_lhsIpkgNm) of
                                    { _restOpkgNm ->
                                    (case (_lhsIopts) of
                                     { _restOopts ->
                                     (case (_lhsImoduleClassNm) of
                                      { _restOmoduleClassNm ->
                                      (case (_lhsIlev) of
                                       { _restOlev ->
                                       (case (_lhsIdataGam) of
                                        { _restOdataGam ->
                                        (case (_lhsIcvarMp) of
                                         { _restOcvarMp ->
                                         (case (rest_ _restOcvarMp _restOdataGam _restOgUniq _restOlev _restOmoduleClassNm _restOopts _restOpkgNm _restOscrutineeCVarInfo _restOtopClassNm ) of
                                          { ( _restIgUniq,_restIoffsetBinds,_restIpatCVarMp) ->
                                              (case (_restIgUniq) of
                                               { _bindsOgUniq ->
                                               (case (_lhsIdataGam) of
                                                { _bindsOdataGam ->
                                                (case (_lhsIcvarMp) of
                                                 { _bindsOcvarMp ->
                                                 (case (tag_) of
                                                  { _bindsOctag ->
                                                  (case (jsDataFldNames _lhsIdataGam tag_) of
                                                   { _bindsOdataFldNmL ->
                                                   (case (binds_ _bindsOctag _bindsOcvarMp _bindsOdataFldNmL _bindsOdataGam _bindsOgUniq _bindsOlev _bindsOmoduleClassNm _bindsOopts _bindsOpkgNm _bindsOscrutineeCVarInfo _bindsOtopClassNm ) of
                                                    { ( _bindsIdataFldNmL,_bindsIfldNmL,_bindsIgUniq,_bindsIoffsetBinds,_bindsIpatCVarMp) ->
                                                        (case (_bindsIfldNmL) of
                                                         { _lhsOfldNmL ->
                                                         (case (_bindsIgUniq) of
                                                          { _lhsOgUniq ->
                                                          (case (_restIoffsetBinds ++ _bindsIoffsetBinds) of
                                                           { _lhsOoffsetBinds ->
                                                           (case (_restIpatCVarMp `Map.union` _bindsIpatCVarMp) of
                                                            { _lhsOpatCVarMp ->
                                                            ( _lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
             in  sem_CPat_Con_1)) of
      { ( sem_CPat_1) ->
      ( _lhsOscrutinees,sem_CPat_1) }) })
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (case ([Scrutinee_Int int_]) of
     { _lhsOscrutinees ->
     (case ((let sem_CPat_Int_1 :: T_CPat_1 
                 sem_CPat_Int_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIgUniq
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsIscrutineeCVarInfo
                        _lhsItopClassNm ->
                          (case ([]) of
                           { _lhsOfldNmL ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case ([]) of
                             { _lhsOoffsetBinds ->
                             (case (Map.empty) of
                              { _lhsOpatCVarMp ->
                              ( _lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }))
             in  sem_CPat_Int_1)) of
      { ( sem_CPat_1) ->
      ( _lhsOscrutinees,sem_CPat_1) }) })
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (case ([Scrutinee_Var pnm_]) of
     { _lhsOscrutinees ->
     (case ((let sem_CPat_Var_1 :: T_CPat_1 
                 sem_CPat_Var_1  =
                     (\ _lhsIcvarMp
                        _lhsIdataGam
                        _lhsIgUniq
                        _lhsIlev
                        _lhsImoduleClassNm
                        _lhsIopts
                        _lhsIpkgNm
                        _lhsIscrutineeCVarInfo
                        _lhsItopClassNm ->
                          (case ([]) of
                           { _lhsOfldNmL ->
                           (case (_lhsIgUniq) of
                            { _lhsOgUniq ->
                            (case ([]) of
                             { _lhsOoffsetBinds ->
                             (case (Map.empty) of
                              { _lhsOpatCVarMp ->
                              ( _lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }))
             in  sem_CPat_Var_1)) of
      { ( sem_CPat_1) ->
      ( _lhsOscrutinees,sem_CPat_1) }) })
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ctag                 : CTag
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attributes:
         dataFldNmL           : [HsName]
         gUniq                : UID
      synthesized attributes:
         fldNmL               : [HsName]
         offsetBinds          : [(HsName,CVarInfo,J.Expr)]
         patCVarMp            : CVarMp
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local _tup11      : _
            local fldNm       : _
            local whatAbove   : {WhatExpr}
            local objFldNm    : _
            local _tup12      : _
            local offsetBinds : _
            local cviField    : _
            local patCVarMp   : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = CTag ->
                  CVarMp ->
                  ([HsName]) ->
                  DataGam ->
                  UID ->
                  Int ->
                  HsName ->
                  EHCOpts ->
                  HsName ->
                  CVarInfo ->
                  HsName ->
                  ( ([HsName]),([HsName]),UID,([(HsName,CVarInfo,J.Expr)]),CVarMp)
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIctag
       _lhsIcvarMp
       _lhsIdataFldNmL
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (hdAndTl' (panic "ToJavaScript.CPatFld.Fld.dataFldNmL") _lhsIdataFldNmL) of
          { __tup11 ->
          (case (__tup11) of
           { (_,_lhsOdataFldNmL) ->
           (case (bind_ ) of
            { ( _bindIbindNmL,_bindInm,bind_1) ->
                (case (_bindInm) of
                 { _fldNm ->
                 (case ([_fldNm]) of
                  { _lhsOfldNmL ->
                  (case (_lhsIgUniq) of
                   { _offsetOgUniq ->
                   (case (offset_ ) of
                    { ( _offsetInmArgL,_offsetIwhatBelow,offset_1) ->
                        (case (ExprIsOther) of
                         { _whatAbove ->
                         (case (_whatAbove) of
                          { _offsetOwhatAbove ->
                          (case (_lhsItopClassNm) of
                           { _offsetOtopClassNm ->
                           (case (_lhsIpkgNm) of
                            { _offsetOpkgNm ->
                            (case (_lhsIopts) of
                             { _offsetOopts ->
                             (case (_lhsImoduleClassNm) of
                              { _offsetOmoduleClassNm ->
                              (case (_lhsIlev) of
                               { _offsetOlev ->
                               (case (_lhsIdataGam) of
                                { _offsetOdataGam ->
                                (case (_lhsIcvarMp) of
                                 { _offsetOcvarMp ->
                                 (case (EvalCtx_Eval) of
                                  { _offsetOevalCtx ->
                                  (case (False) of
                                   { _offsetOisLamBody ->
                                   (case (True) of
                                    { _offsetOisStrict ->
                                    (case (True) of
                                     { _offsetOisTopTup ->
                                     (case (True) of
                                      { _offsetOisTopApp ->
                                      (case (Nothing) of
                                       { _offsetOmbLamNm ->
                                       (case (offset_1 _offsetOcvarMp _offsetOdataGam _offsetOevalCtx _offsetOgUniq _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlev _offsetOmbLamNm _offsetOmoduleClassNm _offsetOopts _offsetOpkgNm _offsetOtopClassNm _offsetOwhatAbove ) of
                                        { ( _offsetIappFunKind,_offsetIargUnpackWrapL,_offsetIgUniq,_offsetIjbinds,_offsetIjs,_offsetIjsArgFunL,_offsetIjsLamBody,_offsetIjstats,_offsetImbFFIApp,_offsetImbLam,_offsetImbVar,_offsetImkFFI,_offsetIresPackWrap,_offsetIusedModNmS) ->
                                            (case (_offsetIgUniq) of
                                             { _bindOgUniq ->
                                             (case (_lhsItopClassNm) of
                                              { _bindOtopClassNm ->
                                              (case (_lhsIpkgNm) of
                                               { _bindOpkgNm ->
                                               (case (_lhsIopts) of
                                                { _bindOopts ->
                                                (case (_lhsImoduleClassNm) of
                                                 { _bindOmoduleClassNm ->
                                                 (case (_lhsIlev) of
                                                  { _bindOlev ->
                                                  (case (_lhsIdataGam) of
                                                   { _bindOdataGam ->
                                                   (case (_lhsIcvarMp) of
                                                    { _bindOcvarMp ->
                                                    (case (EvalCtx_None) of
                                                     { _bindOevalCtx ->
                                                     (case (False) of
                                                      { _bindOisLamBody ->
                                                      (case (False) of
                                                       { _bindOisStrict ->
                                                       (case (acoreBindcategPlain) of
                                                        { _bindOletBindingsCateg ->
                                                        (case (False) of
                                                         { _bindOisGlobal ->
                                                         (case (bind_1 _bindOcvarMp _bindOdataGam _bindOevalCtx _bindOgUniq _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev _bindOmoduleClassNm _bindOopts _bindOpkgNm _bindOtopClassNm ) of
                                                          { ( _bindIgUniq,_bindIjbinds,_bindIusedModNmS) ->
                                                              (case (_bindIgUniq) of
                                                               { _fldAnnsOgUniq ->
                                                               (case (_lhsItopClassNm) of
                                                                { _fldAnnsOtopClassNm ->
                                                                (case (_lhsIscrutineeCVarInfo) of
                                                                 { _fldAnnsOscrutineeCVarInfo ->
                                                                 (case (_lhsIpkgNm) of
                                                                  { _fldAnnsOpkgNm ->
                                                                  (case (_lhsIopts) of
                                                                   { _fldAnnsOopts ->
                                                                   (case (_lhsImoduleClassNm) of
                                                                    { _fldAnnsOmoduleClassNm ->
                                                                    (case (_lhsIlev) of
                                                                     { _fldAnnsOlev ->
                                                                     (case (_lhsIdataGam) of
                                                                      { _fldAnnsOdataGam ->
                                                                      (case (_lhsIdataFldNmL) of
                                                                       { _fldAnnsOdataFldNmL ->
                                                                       (case (_lhsIcvarMp) of
                                                                        { _fldAnnsOcvarMp ->
                                                                        (case (_lhsIctag) of
                                                                         { _fldAnnsOctag ->
                                                                         (case (fldAnns_ _fldAnnsOctag _fldAnnsOcvarMp _fldAnnsOdataFldNmL _fldAnnsOdataGam _fldAnnsOgUniq _fldAnnsOlev _fldAnnsOmoduleClassNm _fldAnnsOopts _fldAnnsOpkgNm _fldAnnsOscrutineeCVarInfo _fldAnnsOtopClassNm ) of
                                                                          { ( _fldAnnsIdataFldNmL,_fldAnnsIgUniq,_fldAnnsIoffsetBinds,_fldAnnsIpatCVarMp) ->
                                                                              (case (_fldAnnsIgUniq) of
                                                                               { _lhsOgUniq ->
                                                                               (case (__tup11) of
                                                                                { (_objFldNm,_) ->
                                                                                (case (case _lhsIctag of
                                                                                         CTagRec
                                                                                           -> case _offsetIwhatBelow of
                                                                                                ExprIsInt i -> (mkf $ Left  i,[])
                                                                                                ExprIsVar n -> (mkf $ Right n,[])
                                                                                           where mkf o = CVarInfo_TupFld tyDefault _lhsIscrutineeCVarInfo o
                                                                                         CTag _ cn _ _ _
                                                                                           -> case _offsetIwhatBelow of
                                                                                                ExprIsInt i -> (CVarInfo_DataFld tyDefault _lhsIscrutineeCVarInfo hsnUnknown (show _objFldNm),[])
                                                                                                _           -> panic "Core.ToJavaScript.CPatFld.Fld.cviField") of
                                                                                 { __tup12 ->
                                                                                 (case (__tup12) of
                                                                                  { (_,_offsetBinds) ->
                                                                                  (case (_offsetBinds) of
                                                                                   { _lhsOoffsetBinds ->
                                                                                   (case (__tup12) of
                                                                                    { (_cviField,_) ->
                                                                                    (case (Map.singleton _fldNm _cviField) of
                                                                                     { _patCVarMp ->
                                                                                     (case (_patCVarMp) of
                                                                                      { _lhsOpatCVarMp ->
                                                                                      ( _lhsOdataFldNmL,_lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ctag                 : CTag
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attributes:
         dataFldNmL           : [HsName]
         gUniq                : UID
      synthesized attributes:
         fldNmL               : [HsName]
         offsetBinds          : [(HsName,CVarInfo,J.Expr)]
         patCVarMp            : CVarMp
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
      alternative Nil:
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = CTag ->
                   CVarMp ->
                   ([HsName]) ->
                   DataGam ->
                   UID ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   HsName ->
                   CVarInfo ->
                   HsName ->
                   ( ([HsName]),([HsName]),UID,([(HsName,CVarInfo,J.Expr)]),CVarMp)
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIctag
       _lhsIcvarMp
       _lhsIdataFldNmL
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIdataFldNmL) of
          { _hdOdataFldNmL ->
          (case (_lhsItopClassNm) of
           { _hdOtopClassNm ->
           (case (_lhsIscrutineeCVarInfo) of
            { _hdOscrutineeCVarInfo ->
            (case (_lhsIpkgNm) of
             { _hdOpkgNm ->
             (case (_lhsIopts) of
              { _hdOopts ->
              (case (_lhsImoduleClassNm) of
               { _hdOmoduleClassNm ->
               (case (_lhsIlev) of
                { _hdOlev ->
                (case (_lhsIgUniq) of
                 { _hdOgUniq ->
                 (case (_lhsIdataGam) of
                  { _hdOdataGam ->
                  (case (_lhsIcvarMp) of
                   { _hdOcvarMp ->
                   (case (_lhsIctag) of
                    { _hdOctag ->
                    (case (hd_ _hdOctag _hdOcvarMp _hdOdataFldNmL _hdOdataGam _hdOgUniq _hdOlev _hdOmoduleClassNm _hdOopts _hdOpkgNm _hdOscrutineeCVarInfo _hdOtopClassNm ) of
                     { ( _hdIdataFldNmL,_hdIfldNmL,_hdIgUniq,_hdIoffsetBinds,_hdIpatCVarMp) ->
                         (case (_hdIdataFldNmL) of
                          { _tlOdataFldNmL ->
                          (case (_lhsItopClassNm) of
                           { _tlOtopClassNm ->
                           (case (_lhsIscrutineeCVarInfo) of
                            { _tlOscrutineeCVarInfo ->
                            (case (_lhsIpkgNm) of
                             { _tlOpkgNm ->
                             (case (_lhsIopts) of
                              { _tlOopts ->
                              (case (_lhsImoduleClassNm) of
                               { _tlOmoduleClassNm ->
                               (case (_lhsIlev) of
                                { _tlOlev ->
                                (case (_hdIgUniq) of
                                 { _tlOgUniq ->
                                 (case (_lhsIdataGam) of
                                  { _tlOdataGam ->
                                  (case (_lhsIcvarMp) of
                                   { _tlOcvarMp ->
                                   (case (_lhsIctag) of
                                    { _tlOctag ->
                                    (case (tl_ _tlOctag _tlOcvarMp _tlOdataFldNmL _tlOdataGam _tlOgUniq _tlOlev _tlOmoduleClassNm _tlOopts _tlOpkgNm _tlOscrutineeCVarInfo _tlOtopClassNm ) of
                                     { ( _tlIdataFldNmL,_tlIfldNmL,_tlIgUniq,_tlIoffsetBinds,_tlIpatCVarMp) ->
                                         (case (_tlIdataFldNmL) of
                                          { _lhsOdataFldNmL ->
                                          (case (_hdIfldNmL ++ _tlIfldNmL) of
                                           { _lhsOfldNmL ->
                                           (case (_tlIgUniq) of
                                            { _lhsOgUniq ->
                                            (case (_hdIoffsetBinds ++ _tlIoffsetBinds) of
                                             { _lhsOoffsetBinds ->
                                             (case (_hdIpatCVarMp `Map.union` _tlIpatCVarMp) of
                                              { _lhsOpatCVarMp ->
                                              ( _lhsOdataFldNmL,_lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIctag
       _lhsIcvarMp
       _lhsIdataFldNmL
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIdataFldNmL) of
          { _lhsOdataFldNmL ->
          (case ([]) of
           { _lhsOfldNmL ->
           (case (_lhsIgUniq) of
            { _lhsOgUniq ->
            (case ([]) of
             { _lhsOoffsetBinds ->
             (case (Map.empty) of
              { _lhsOpatCVarMp ->
              ( _lhsOdataFldNmL,_lhsOfldNmL,_lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         lev                  : Int
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         scrutineeCVarInfo    : CVarInfo
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         offsetBinds          : [(HsName,CVarInfo,J.Expr)]
         patCVarMp            : CVarMp
   alternatives:
      alternative Empty:
      alternative Var:
         child nm             : {HsName}
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = CVarMp ->
                   DataGam ->
                   UID ->
                   Int ->
                   HsName ->
                   EHCOpts ->
                   HsName ->
                   CVarInfo ->
                   HsName ->
                   ( UID,([(HsName,CVarInfo,J.Expr)]),CVarMp)
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ([]) of
           { _lhsOoffsetBinds ->
           (case (Map.empty) of
            { _lhsOpatCVarMp ->
            ( _lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIgUniq
       _lhsIlev
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsIscrutineeCVarInfo
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          (case ([]) of
           { _lhsOoffsetBinds ->
           (case (Map.empty) of
            { _lhsOpatCVarMp ->
            ( _lhsOgUniq,_lhsOoffsetBinds,_lhsOpatCVarMp) }) }) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dataGam              : DataGam
         opts                 : EHCOpts
      synthesized attributes:
         js                   : J.JavaScriptModule
         moduleClassNm        : HsName
   alternatives:
      alternative AGItf:
         child module         : CModule 
         visit 0:
            local gUniq       : _
            local cvarMp      : _
            local topClassNm  : _
            local pkgNm       : _
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = DataGam ->
                    EHCOpts ->
                    ( (J.JavaScriptModule),HsName)
data Inh_CodeAGItf  = Inh_CodeAGItf {dataGam_Inh_CodeAGItf :: !(DataGam),opts_Inh_CodeAGItf :: !(EHCOpts)}
data Syn_CodeAGItf  = Syn_CodeAGItf {js_Syn_CodeAGItf :: !((J.JavaScriptModule)),moduleClassNm_Syn_CodeAGItf :: !(HsName)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIdataGam _lhsIopts )  =
    (let ( _lhsOjs,_lhsOmoduleClassNm) = sem _lhsIdataGam _lhsIopts 
     in  (Syn_CodeAGItf _lhsOjs _lhsOmoduleClassNm ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIdataGam
       _lhsIopts ->
         (case (_lhsIopts) of
          { _moduleOopts ->
          (case (uidStart) of
           { _gUniq ->
           (case (_gUniq) of
            { _moduleOgUniq ->
            (case (_lhsIdataGam) of
             { _moduleOdataGam ->
             (case (Map.empty) of
              { _cvarMp ->
              (case (_cvarMp) of
               { _moduleOcvarMp ->
               (case (cLevModule) of
                { _moduleOlev ->
                (case (hsnUnknown) of
                 { _topClassNm ->
                 (case (_topClassNm) of
                  { _moduleOtopClassNm ->
                  (case (hsnUnknown) of
                   { _pkgNm ->
                   (case (_pkgNm) of
                    { _moduleOpkgNm ->
                    (case (module_ _moduleOcvarMp _moduleOdataGam _moduleOgUniq _moduleOlev _moduleOopts _moduleOpkgNm _moduleOtopClassNm ) of
                     { ( _moduleIgUniq,_moduleIjs,_moduleImoduleClassNm) ->
                         (case (_moduleIjs) of
                          { _lhsOjs ->
                          (case (_moduleImoduleClassNm) of
                           { _lhsOmoduleClassNm ->
                           ( _lhsOjs,_lhsOmoduleClassNm) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarMp               : CVarMp
         dataGam              : DataGam
         evalCtx              : EvalCtx
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         mbLamNm              : Maybe (HsName,HsName)
         moduleClassNm        : HsName
         opts                 : EHCOpts
         pkgNm                : HsName
         topClassNm           : HsName
      chained attribute:
         gUniq                : UID
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
      alternative Nothing:
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = CVarMp ->
                  DataGam ->
                  EvalCtx ->
                  UID ->
                  Bool ->
                  Bool ->
                  Int ->
                  (Maybe (HsName,HsName)) ->
                  HsName ->
                  EHCOpts ->
                  HsName ->
                  HsName ->
                  ( UID)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _justOgUniq ->
          (case (just_ ) of
           { ( _justInmArgL,_justIwhatBelow,just_1) ->
               (case (ExprIsOther) of
                { _whatAbove ->
                (case (_whatAbove) of
                 { _justOwhatAbove ->
                 (case (_lhsItopClassNm) of
                  { _justOtopClassNm ->
                  (case (_lhsIpkgNm) of
                   { _justOpkgNm ->
                   (case (_lhsIopts) of
                    { _justOopts ->
                    (case (_lhsImoduleClassNm) of
                     { _justOmoduleClassNm ->
                     (case (_lhsImbLamNm) of
                      { _justOmbLamNm ->
                      (case (_lhsIlev) of
                       { _justOlev ->
                       (case (_lhsIisStrict) of
                        { _justOisStrict ->
                        (case (_lhsIisLamBody) of
                         { _justOisLamBody ->
                         (case (_lhsIevalCtx) of
                          { _justOevalCtx ->
                          (case (_lhsIdataGam) of
                           { _justOdataGam ->
                           (case (_lhsIcvarMp) of
                            { _justOcvarMp ->
                            (case (True) of
                             { _justOisTopTup ->
                             (case (True) of
                              { _justOisTopApp ->
                              (case (just_1 _justOcvarMp _justOdataGam _justOevalCtx _justOgUniq _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlev _justOmbLamNm _justOmoduleClassNm _justOopts _justOpkgNm _justOtopClassNm _justOwhatAbove ) of
                               { ( _justIappFunKind,_justIargUnpackWrapL,_justIgUniq,_justIjbinds,_justIjs,_justIjsArgFunL,_justIjsLamBody,_justIjstats,_justImbFFIApp,_justImbLam,_justImbVar,_justImkFFI,_justIresPackWrap,_justIusedModNmS) ->
                                   (case (_justIgUniq) of
                                    { _lhsOgUniq ->
                                    ( _lhsOgUniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIcvarMp
       _lhsIdataGam
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsImbLamNm
       _lhsImoduleClassNm
       _lhsIopts
       _lhsIpkgNm
       _lhsItopClassNm ->
         (case (_lhsIgUniq) of
          { _lhsOgUniq ->
          ( _lhsOgUniq) }))