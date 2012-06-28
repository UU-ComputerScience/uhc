

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/HS/ModImpExp.ag)
module EH101.HS.ModImpExp(Inh_AGItf (..), Syn_AGItf (..), sem_AGItf, wrap_AGItf) where

import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts
import EH101.HS
import EH101.Gam.Full
import EH101.NameAspect
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import qualified EH.Util.Rel as Rel
import EH101.Module
import EH101.Base.Target
import qualified EH101.Base.Pragma as Pragma














mkInstDefsRel :: Bool -> HsName -> Maybe HsName -> IdOcc -> IdOcc -> (ModEntRel,ModEntRel)
mkInstDefsRel useimplicitly instancename maybeinstancename idOccDef idOccDefVal
  = ( (if useimplicitly then i else Rel.empty)
      `Rel.union`
      maybe (Rel.empty) (const v) maybeinstancename
    , v
    )
  where v = Rel.singleton instancename (ModEnt IdOcc_Val idOccDefVal Set.empty emptyRange)
        i = Rel.singleton instancename (ModEnt IdOcc_Inst idOccDef Set.empty emptyRange)

-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gUniq                : UID
         moduleNm             : HsName
         opts                 : EHCOpts
      synthesized attributes:
         fileHeaderPragmas    : Set.Set Pragma.Pragma
         mainValExists        : Bool
         mod                  : Mod
         modImpNmS            : Set.Set HsName
         realModuleNm         : HsName
   alternatives:
      alternative AGItf:
         child module         : Module 
         visit 0:
            local topInstanceNmL : _
            local nmLev       : _
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _module )  =
    (sem_AGItf_AGItf (sem_Module _module ) )
-- semantic domain
type T_AGItf  = UID ->
                HsName ->
                EHCOpts ->
                ( (Set.Set Pragma.Pragma),Bool,Mod,(Set.Set HsName),HsName)
data Inh_AGItf  = Inh_AGItf {gUniq_Inh_AGItf :: !(UID),moduleNm_Inh_AGItf :: !(HsName),opts_Inh_AGItf :: !(EHCOpts)}
data Syn_AGItf  = Syn_AGItf {fileHeaderPragmas_Syn_AGItf :: !((Set.Set Pragma.Pragma)),mainValExists_Syn_AGItf :: !(Bool),mod_Syn_AGItf :: !(Mod),modImpNmS_Syn_AGItf :: !((Set.Set HsName)),realModuleNm_Syn_AGItf :: !(HsName)}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsIgUniq _lhsImoduleNm _lhsIopts )  =
    (let ( _lhsOfileHeaderPragmas,_lhsOmainValExists,_lhsOmod,_lhsOmodImpNmS,_lhsOrealModuleNm) | True = sem _lhsIgUniq _lhsImoduleNm _lhsIopts 
     in  (Syn_AGItf _lhsOfileHeaderPragmas _lhsOmainValExists _lhsOmod _lhsOmodImpNmS _lhsOrealModuleNm ))
sem_AGItf_AGItf :: T_Module  ->
                   T_AGItf 
sem_AGItf_AGItf module_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsIopts ->
         (let _lhsOfileHeaderPragmas :: (Set.Set Pragma.Pragma)
              _lhsOmainValExists :: Bool
              _lhsOmod :: Mod
              _lhsOmodImpNmS :: (Set.Set HsName)
              _lhsOrealModuleNm :: HsName
              _moduleOgUniq :: UID
              _moduleOmoduleNm :: HsName
              _moduleOnmLev :: NmLev
              _moduleOopts :: EHCOpts
              _moduleOtopInstanceNmL :: ([HsName])
              _moduleIfileHeaderPragmas :: (Set.Set Pragma.Pragma)
              _moduleIgUniq :: UID
              _moduleImainValExists :: Bool
              _moduleImod :: Mod
              _moduleImodImpNmS :: (Set.Set HsName)
              _moduleIrealModuleNm :: HsName
              _moduleItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 51, column 9)
              _topInstanceNmL =
                  []
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 13, column 9)
              _nmLev =
                  nmLevModule
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 2, column 43)
              _lhsOfileHeaderPragmas =
                  _moduleIfileHeaderPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _moduleImainValExists
              -- copy rule (up)
              _lhsOmod =
                  _moduleImod
              -- copy rule (up)
              _lhsOmodImpNmS =
                  _moduleImodImpNmS
              -- copy rule (up)
              _lhsOrealModuleNm =
                  _moduleIrealModuleNm
              -- copy rule (down)
              _moduleOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _moduleOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _moduleOnmLev =
                  _nmLev
              -- copy rule (down)
              _moduleOopts =
                  _lhsIopts
              -- copy rule (from local)
              _moduleOtopInstanceNmL =
                  _topInstanceNmL
              ( _moduleIfileHeaderPragmas,_moduleIgUniq,_moduleImainValExists,_moduleImod,_moduleImodImpNmS,_moduleIrealModuleNm,_moduleItopInstanceNmL) | True =
                  module_ _moduleOgUniq _moduleOmoduleNm _moduleOnmLev _moduleOopts _moduleOtopInstanceNmL 
          in  ( _lhsOfileHeaderPragmas,_lhsOmainValExists,_lhsOmod,_lhsOmodImpNmS,_lhsOrealModuleNm)))
-- Alternative -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Alternative:
         child range          : {Range}
         child pattern        : Pattern 
         child righthandside  : RightHandSide 
         visit 0:
            local nmLev       : _
      alternative Empty:
         child range          : {Range}
-}
-- cata
sem_Alternative :: Alternative  ->
                   T_Alternative 
sem_Alternative (Alternative_Alternative _range _pattern _righthandside )  =
    (sem_Alternative_Alternative _range (sem_Pattern _pattern ) (sem_RightHandSide _righthandside ) )
sem_Alternative (Alternative_Empty _range )  =
    (sem_Alternative_Empty _range )
-- semantic domain
type T_Alternative  = UID ->
                      HsName ->
                      NmLev ->
                      EHCOpts ->
                      ([HsName]) ->
                      ( UID,([HsName]))
sem_Alternative_Alternative :: Range ->
                               T_Pattern  ->
                               T_RightHandSide  ->
                               T_Alternative 
sem_Alternative_Alternative range_ pattern_ righthandside_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _righthandsideOgUniq :: UID
              _righthandsideOmoduleNm :: HsName
              _righthandsideOnmLev :: NmLev
              _righthandsideOopts :: EHCOpts
              _righthandsideOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _righthandsideIgUniq :: UID
              _righthandsideItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 73, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _righthandsideIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _righthandsideItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _patternOnmLev =
                  _nmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _righthandsideOgUniq =
                  _patternIgUniq
              -- copy rule (down)
              _righthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _righthandsideOnmLev =
                  _nmLev
              -- copy rule (down)
              _righthandsideOopts =
                  _lhsIopts
              -- copy rule (chain)
              _righthandsideOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _righthandsideIgUniq,_righthandsideItopInstanceNmL) | True =
                  righthandside_ _righthandsideOgUniq _righthandsideOmoduleNm _righthandsideOnmLev _righthandsideOopts _righthandsideOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Alternative_Empty :: Range ->
                         T_Alternative 
sem_Alternative_Empty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Alternatives ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : Alternative 
         child tl             : Alternatives 
      alternative Nil:
-}
-- cata
sem_Alternatives :: Alternatives  ->
                    T_Alternatives 
sem_Alternatives list  =
    (Prelude.foldr sem_Alternatives_Cons sem_Alternatives_Nil (Prelude.map sem_Alternative list) )
-- semantic domain
type T_Alternatives  = UID ->
                       HsName ->
                       NmLev ->
                       EHCOpts ->
                       ([HsName]) ->
                       ( UID,([HsName]))
sem_Alternatives_Cons :: T_Alternative  ->
                         T_Alternatives  ->
                         T_Alternatives 
sem_Alternatives_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Alternatives_Nil :: T_Alternatives 
sem_Alternatives_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Body --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         mainValExists        : Bool
         modDefsRel           : ModEntRel
         modHideDefsRel       : ModEntRel
         modImpL              : [ModImp]
   alternatives:
      alternative Body:
         child range          : {Range}
         child importdeclarations : ImportDeclarations 
         child declarations   : Declarations 
         visit 0:
            local refmainname : _
            local idOccDefMain : _
-}
-- cata
sem_Body :: Body  ->
            T_Body 
sem_Body (Body_Body _range _importdeclarations _declarations )  =
    (sem_Body_Body _range (sem_ImportDeclarations _importdeclarations ) (sem_Declarations _declarations ) )
-- semantic domain
type T_Body  = UID ->
               HsName ->
               NmLev ->
               EHCOpts ->
               ([HsName]) ->
               ( UID,Bool,ModEntRel,ModEntRel,([ModImp]),([HsName]))
sem_Body_Body :: Range ->
                 T_ImportDeclarations  ->
                 T_Declarations  ->
                 T_Body 
sem_Body_Body range_ importdeclarations_ declarations_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOmodImpL :: ([ModImp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _importdeclarationsOgUniq :: UID
              _importdeclarationsOmoduleNm :: HsName
              _importdeclarationsOnmLev :: NmLev
              _importdeclarationsOopts :: EHCOpts
              _importdeclarationsOtopInstanceNmL :: ([HsName])
              _declarationsOgUniq :: UID
              _declarationsOmoduleNm :: HsName
              _declarationsOnmLev :: NmLev
              _declarationsOopts :: EHCOpts
              _declarationsOtopInstanceNmL :: ([HsName])
              _importdeclarationsIgUniq :: UID
              _importdeclarationsImodImpL :: ([ModImp])
              _importdeclarationsItopInstanceNmL :: ([HsName])
              _declarationsIgUniq :: UID
              _declarationsIgathPragmas :: (Set.Set Pragma.Pragma)
              _declarationsIidOccDefs :: ([IdOcc])
              _declarationsImainValExists :: Bool
              _declarationsImodDefsRel :: ModEntRel
              _declarationsImodHideDefsRel :: ModEntRel
              _declarationsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 8, column 9)
              _refmainname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm hsnMain
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 188, column 9)
              _idOccDefMain =
                  IdOcc hsnMain IdOcc_Val
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _declarationsImainValExists
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  _declarationsImodDefsRel
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _declarationsImodHideDefsRel
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 35, column 46)
              _lhsOmodImpL =
                  _importdeclarationsImodImpL
              -- copy rule (up)
              _lhsOgUniq =
                  _declarationsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _declarationsItopInstanceNmL
              -- copy rule (down)
              _importdeclarationsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _importdeclarationsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _importdeclarationsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _importdeclarationsOopts =
                  _lhsIopts
              -- copy rule (down)
              _importdeclarationsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _declarationsOgUniq =
                  _importdeclarationsIgUniq
              -- copy rule (down)
              _declarationsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _declarationsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _declarationsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _declarationsOtopInstanceNmL =
                  _importdeclarationsItopInstanceNmL
              ( _importdeclarationsIgUniq,_importdeclarationsImodImpL,_importdeclarationsItopInstanceNmL) | True =
                  importdeclarations_ _importdeclarationsOgUniq _importdeclarationsOmoduleNm _importdeclarationsOnmLev _importdeclarationsOopts _importdeclarationsOtopInstanceNmL 
              ( _declarationsIgUniq,_declarationsIgathPragmas,_declarationsIidOccDefs,_declarationsImainValExists,_declarationsImodDefsRel,_declarationsImodHideDefsRel,_declarationsItopInstanceNmL) | True =
                  declarations_ _declarationsOgUniq _declarationsOmoduleNm _declarationsOnmLev _declarationsOopts _declarationsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOmodImpL,_lhsOtopInstanceNmL)))
-- Constructor -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         conAndFldrefnames    : AssocL Name Names
         conrefnames          : Names
   alternatives:
      alternative Constructor:
         child range          : {Range}
         child constructor    : {Name}
         child types          : Types 
         visit 0:
            local nmLev       : _
            local conrefname  : _
      alternative Contexted:
         child range          : {Range}
         child context        : ContextItems 
         child constructor    : Constructor 
      alternative GADTFunction:
         child range          : {Range}
         child constructor    : {Name}
         child type           : Type 
         visit 0:
            local nmLev       : _
            local conrefname  : _
      alternative Infix:
         child range          : {Range}
         child leftType       : Type 
         child constructorOperator : {Name}
         child rightType      : Type 
         visit 0:
            local nmLev       : _
            local conrefname  : _
      alternative Record:
         child range          : {Range}
         child constructor    : {Name}
         child fieldDeclarations : FieldDeclarations 
         visit 0:
            local conrefname  : _
            local conAndFldrefnames : _
-}
-- cata
sem_Constructor :: Constructor  ->
                   T_Constructor 
sem_Constructor (Constructor_Constructor _range _constructor _types )  =
    (sem_Constructor_Constructor _range _constructor (sem_Types _types ) )
sem_Constructor (Constructor_Contexted _range _context _constructor )  =
    (sem_Constructor_Contexted _range (sem_ContextItems _context ) (sem_Constructor _constructor ) )
sem_Constructor (Constructor_GADTFunction _range _constructor _type )  =
    (sem_Constructor_GADTFunction _range _constructor (sem_Type _type ) )
sem_Constructor (Constructor_Infix _range _leftType _constructorOperator _rightType )  =
    (sem_Constructor_Infix _range (sem_Type _leftType ) _constructorOperator (sem_Type _rightType ) )
sem_Constructor (Constructor_Record _range _constructor _fieldDeclarations )  =
    (sem_Constructor_Record _range _constructor (sem_FieldDeclarations _fieldDeclarations ) )
-- semantic domain
type T_Constructor  = UID ->
                      HsName ->
                      NmLev ->
                      ([HsName]) ->
                      ( (AssocL Name Names),Names,UID,([HsName]))
sem_Constructor_Constructor :: Range ->
                               Name ->
                               T_Types  ->
                               T_Constructor 
sem_Constructor_Constructor range_ constructor_ types_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconrefnames :: Names
              _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typesOgUniq :: UID
              _typesOnmLev :: NmLev
              _typesOtopInstanceNmL :: ([HsName])
              _typesIgUniq :: UID
              _typesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 38, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 64, column 9)
              _conrefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm constructor_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 71, column 9)
              _lhsOconrefnames =
                  [_conrefname]
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  []
              -- copy rule (up)
              _lhsOgUniq =
                  _typesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typesItopInstanceNmL
              -- copy rule (down)
              _typesOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _typesOnmLev =
                  _nmLev
              -- copy rule (down)
              _typesOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typesIgUniq,_typesItopInstanceNmL) | True =
                  types_ _typesOgUniq _typesOnmLev _typesOtopInstanceNmL 
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Constructor_Contexted :: Range ->
                             T_ContextItems  ->
                             T_Constructor  ->
                             T_Constructor 
sem_Constructor_Contexted range_ context_ constructor_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOconrefnames :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _constructorOgUniq :: UID
              _constructorOmoduleNm :: HsName
              _constructorOnmLev :: NmLev
              _constructorOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _constructorIconAndFldrefnames :: (AssocL Name Names)
              _constructorIconrefnames :: Names
              _constructorIgUniq :: UID
              _constructorItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  _constructorIconAndFldrefnames
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 60, column 39)
              _lhsOconrefnames =
                  _constructorIconrefnames
              -- copy rule (up)
              _lhsOgUniq =
                  _constructorIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _constructorItopInstanceNmL
              -- copy rule (down)
              _contextOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _contextOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _constructorOgUniq =
                  _contextIgUniq
              -- copy rule (down)
              _constructorOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _constructorOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _constructorOtopInstanceNmL =
                  _contextItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _constructorIconAndFldrefnames,_constructorIconrefnames,_constructorIgUniq,_constructorItopInstanceNmL) | True =
                  constructor_ _constructorOgUniq _constructorOmoduleNm _constructorOnmLev _constructorOtopInstanceNmL 
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Constructor_GADTFunction :: Range ->
                                Name ->
                                T_Type  ->
                                T_Constructor 
sem_Constructor_GADTFunction range_ constructor_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconrefnames :: Names
              _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 38, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 64, column 9)
              _conrefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm constructor_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 71, column 9)
              _lhsOconrefnames =
                  [_conrefname]
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  []
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Constructor_Infix :: Range ->
                         T_Type  ->
                         Name ->
                         T_Type  ->
                         T_Constructor 
sem_Constructor_Infix range_ leftType_ constructorOperator_ rightType_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconrefnames :: Names
              _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftTypeOgUniq :: UID
              _leftTypeOnmLev :: NmLev
              _leftTypeOtopInstanceNmL :: ([HsName])
              _rightTypeOgUniq :: UID
              _rightTypeOnmLev :: NmLev
              _rightTypeOtopInstanceNmL :: ([HsName])
              _leftTypeIconNm :: Name
              _leftTypeIgUniq :: UID
              _leftTypeItopInstanceNmL :: ([HsName])
              _rightTypeIconNm :: Name
              _rightTypeIgUniq :: UID
              _rightTypeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 38, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 66, column 9)
              _conrefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm constructorOperator_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 73, column 9)
              _lhsOconrefnames =
                  [_conrefname]
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  []
              -- copy rule (up)
              _lhsOgUniq =
                  _rightTypeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightTypeItopInstanceNmL
              -- copy rule (down)
              _leftTypeOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _leftTypeOnmLev =
                  _nmLev
              -- copy rule (down)
              _leftTypeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rightTypeOgUniq =
                  _leftTypeIgUniq
              -- copy rule (from local)
              _rightTypeOnmLev =
                  _nmLev
              -- copy rule (chain)
              _rightTypeOtopInstanceNmL =
                  _leftTypeItopInstanceNmL
              ( _leftTypeIconNm,_leftTypeIgUniq,_leftTypeItopInstanceNmL) | True =
                  leftType_ _leftTypeOgUniq _leftTypeOnmLev _leftTypeOtopInstanceNmL 
              ( _rightTypeIconNm,_rightTypeIgUniq,_rightTypeItopInstanceNmL) | True =
                  rightType_ _rightTypeOgUniq _rightTypeOnmLev _rightTypeOtopInstanceNmL 
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Constructor_Record :: Range ->
                          Name ->
                          T_FieldDeclarations  ->
                          T_Constructor 
sem_Constructor_Record range_ constructor_ fieldDeclarations_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconrefnames :: Names
              _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _fieldDeclarationsOgUniq :: UID
              _fieldDeclarationsOmoduleNm :: HsName
              _fieldDeclarationsOnmLev :: NmLev
              _fieldDeclarationsOtopInstanceNmL :: ([HsName])
              _fieldDeclarationsIfldrefnames :: Names
              _fieldDeclarationsIgUniq :: UID
              _fieldDeclarationsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 64, column 9)
              _conrefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm constructor_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 76, column 9)
              _lhsOconrefnames =
                  [_conrefname]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 87, column 9)
              _conAndFldrefnames =
                  [(_conrefname,_fieldDeclarationsIfldrefnames)]
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  _conAndFldrefnames
              -- copy rule (up)
              _lhsOgUniq =
                  _fieldDeclarationsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _fieldDeclarationsItopInstanceNmL
              -- copy rule (down)
              _fieldDeclarationsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _fieldDeclarationsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _fieldDeclarationsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _fieldDeclarationsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _fieldDeclarationsIfldrefnames,_fieldDeclarationsIgUniq,_fieldDeclarationsItopInstanceNmL) | True =
                  fieldDeclarations_ _fieldDeclarationsOgUniq _fieldDeclarationsOmoduleNm _fieldDeclarationsOnmLev _fieldDeclarationsOtopInstanceNmL 
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
-- Constructors ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         conAndFldrefnames    : AssocL Name Names
         conrefnames          : Names
   alternatives:
      alternative Cons:
         child hd             : Constructor 
         child tl             : Constructors 
      alternative Nil:
-}
-- cata
sem_Constructors :: Constructors  ->
                    T_Constructors 
sem_Constructors list  =
    (Prelude.foldr sem_Constructors_Cons sem_Constructors_Nil (Prelude.map sem_Constructor list) )
-- semantic domain
type T_Constructors  = UID ->
                       HsName ->
                       NmLev ->
                       ([HsName]) ->
                       ( (AssocL Name Names),Names,UID,([HsName]))
sem_Constructors_Cons :: T_Constructor  ->
                         T_Constructors  ->
                         T_Constructors 
sem_Constructors_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOconrefnames :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIconAndFldrefnames :: (AssocL Name Names)
              _hdIconrefnames :: Names
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIconAndFldrefnames :: (AssocL Name Names)
              _tlIconrefnames :: Names
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  _hdIconAndFldrefnames ++ _tlIconAndFldrefnames
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 60, column 39)
              _lhsOconrefnames =
                  _hdIconrefnames ++ _tlIconrefnames
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIconAndFldrefnames,_hdIconrefnames,_hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIconAndFldrefnames,_tlIconrefnames,_tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Constructors_Nil :: T_Constructors 
sem_Constructors_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconAndFldrefnames :: (AssocL Name Names)
              _lhsOconrefnames :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 79, column 45)
              _lhsOconAndFldrefnames =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 60, column 39)
              _lhsOconrefnames =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconAndFldrefnames,_lhsOconrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
-- ContextItem -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Arrow:
         child range          : {Range}
         child argument       : ContextItem 
         child result         : ContextItem 
      alternative Class:
         child range          : {Range}
         child name           : {Name}
         child types          : Types 
      alternative Equal:
         child range          : {Range}
         child type1          : Type 
         child type2          : Type 
      alternative Forall:
         child range          : {Range}
         child typevariables  : {Names}
         child context        : ContextItem 
         visit 0:
            local nmLev       : _
      alternative Implicits:
         child range          : {Range}
      alternative NoImplicits:
         child range          : {Range}
      alternative RowLacksLabel:
         child range          : {Range}
         child rowvariable    : {Name}
         child name           : {Name}
-}
-- cata
sem_ContextItem :: ContextItem  ->
                   T_ContextItem 
sem_ContextItem (ContextItem_Arrow _range _argument _result )  =
    (sem_ContextItem_Arrow _range (sem_ContextItem _argument ) (sem_ContextItem _result ) )
sem_ContextItem (ContextItem_Class _range _name _types )  =
    (sem_ContextItem_Class _range _name (sem_Types _types ) )
sem_ContextItem (ContextItem_Equal _range _type1 _type2 )  =
    (sem_ContextItem_Equal _range (sem_Type _type1 ) (sem_Type _type2 ) )
sem_ContextItem (ContextItem_Forall _range _typevariables _context )  =
    (sem_ContextItem_Forall _range _typevariables (sem_ContextItem _context ) )
sem_ContextItem (ContextItem_Implicits _range )  =
    (sem_ContextItem_Implicits _range )
sem_ContextItem (ContextItem_NoImplicits _range )  =
    (sem_ContextItem_NoImplicits _range )
sem_ContextItem (ContextItem_RowLacksLabel _range _rowvariable _name )  =
    (sem_ContextItem_RowLacksLabel _range _rowvariable _name )
-- semantic domain
type T_ContextItem  = UID ->
                      NmLev ->
                      ([HsName]) ->
                      ( UID,([HsName]))
sem_ContextItem_Arrow :: Range ->
                         T_ContextItem  ->
                         T_ContextItem  ->
                         T_ContextItem 
sem_ContextItem_Arrow range_ argument_ result_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _argumentOgUniq :: UID
              _argumentOnmLev :: NmLev
              _argumentOtopInstanceNmL :: ([HsName])
              _resultOgUniq :: UID
              _resultOnmLev :: NmLev
              _resultOtopInstanceNmL :: ([HsName])
              _argumentIgUniq :: UID
              _argumentItopInstanceNmL :: ([HsName])
              _resultIgUniq :: UID
              _resultItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _resultIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _resultItopInstanceNmL
              -- copy rule (down)
              _argumentOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _argumentOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _argumentOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _resultOgUniq =
                  _argumentIgUniq
              -- copy rule (down)
              _resultOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _resultOtopInstanceNmL =
                  _argumentItopInstanceNmL
              ( _argumentIgUniq,_argumentItopInstanceNmL) | True =
                  argument_ _argumentOgUniq _argumentOnmLev _argumentOtopInstanceNmL 
              ( _resultIgUniq,_resultItopInstanceNmL) | True =
                  result_ _resultOgUniq _resultOnmLev _resultOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItem_Class :: Range ->
                         Name ->
                         T_Types  ->
                         T_ContextItem 
sem_ContextItem_Class range_ name_ types_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typesOgUniq :: UID
              _typesOnmLev :: NmLev
              _typesOtopInstanceNmL :: ([HsName])
              _typesIgUniq :: UID
              _typesItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _typesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typesItopInstanceNmL
              -- copy rule (down)
              _typesOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typesOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typesOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typesIgUniq,_typesItopInstanceNmL) | True =
                  types_ _typesOgUniq _typesOnmLev _typesOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItem_Equal :: Range ->
                         T_Type  ->
                         T_Type  ->
                         T_ContextItem 
sem_ContextItem_Equal range_ type1_ type2_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _type1OgUniq :: UID
              _type1OnmLev :: NmLev
              _type1OtopInstanceNmL :: ([HsName])
              _type2OgUniq :: UID
              _type2OnmLev :: NmLev
              _type2OtopInstanceNmL :: ([HsName])
              _type1IconNm :: Name
              _type1IgUniq :: UID
              _type1ItopInstanceNmL :: ([HsName])
              _type2IconNm :: Name
              _type2IgUniq :: UID
              _type2ItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _type2IgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _type2ItopInstanceNmL
              -- copy rule (down)
              _type1OgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _type1OnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _type1OtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _type2OgUniq =
                  _type1IgUniq
              -- copy rule (down)
              _type2OnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _type2OtopInstanceNmL =
                  _type1ItopInstanceNmL
              ( _type1IconNm,_type1IgUniq,_type1ItopInstanceNmL) | True =
                  type1_ _type1OgUniq _type1OnmLev _type1OtopInstanceNmL 
              ( _type2IconNm,_type2IgUniq,_type2ItopInstanceNmL) | True =
                  type2_ _type2OgUniq _type2OnmLev _type2OtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItem_Forall :: Range ->
                          Names ->
                          T_ContextItem  ->
                          T_ContextItem 
sem_ContextItem_Forall range_ typevariables_ context_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 46, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _contextIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (down)
              _contextOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _contextOnmLev =
                  _nmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItem_Implicits :: Range ->
                             T_ContextItem 
sem_ContextItem_Implicits range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItem_NoImplicits :: Range ->
                               T_ContextItem 
sem_ContextItem_NoImplicits range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItem_RowLacksLabel :: Range ->
                                 Name ->
                                 Name ->
                                 T_ContextItem 
sem_ContextItem_RowLacksLabel range_ rowvariable_ name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- ContextItems ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : ContextItem 
         child tl             : ContextItems 
      alternative Nil:
-}
-- cata
sem_ContextItems :: ContextItems  ->
                    T_ContextItems 
sem_ContextItems list  =
    (Prelude.foldr sem_ContextItems_Cons sem_ContextItems_Nil (Prelude.map sem_ContextItem list) )
-- semantic domain
type T_ContextItems  = UID ->
                       NmLev ->
                       ([HsName]) ->
                       ( UID,([HsName]))
sem_ContextItems_Cons :: T_ContextItem  ->
                         T_ContextItems  ->
                         T_ContextItems 
sem_ContextItems_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextItems_Nil :: T_ContextItems 
sem_ContextItems_Nil  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- ContextedExpression -----------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Contexted:
         child range          : {Range}
         child expression     : Expression 
         child context        : ContextItem 
-}
-- cata
sem_ContextedExpression :: ContextedExpression  ->
                           T_ContextedExpression 
sem_ContextedExpression (ContextedExpression_Contexted _range _expression _context )  =
    (sem_ContextedExpression_Contexted _range (sem_Expression _expression ) (sem_ContextItem _context ) )
-- semantic domain
type T_ContextedExpression  = UID ->
                              HsName ->
                              NmLev ->
                              EHCOpts ->
                              ([HsName]) ->
                              ( UID,([HsName]))
sem_ContextedExpression_Contexted :: Range ->
                                     T_Expression  ->
                                     T_ContextItem  ->
                                     T_ContextedExpression 
sem_ContextedExpression_Contexted range_ expression_ context_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _contextIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _contextOgUniq =
                  _expressionIgUniq
              -- copy rule (down)
              _contextOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _contextOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- ContextedExpressions ----------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : ContextedExpression 
         child tl             : ContextedExpressions 
      alternative Nil:
-}
-- cata
sem_ContextedExpressions :: ContextedExpressions  ->
                            T_ContextedExpressions 
sem_ContextedExpressions list  =
    (Prelude.foldr sem_ContextedExpressions_Cons sem_ContextedExpressions_Nil (Prelude.map sem_ContextedExpression list) )
-- semantic domain
type T_ContextedExpressions  = UID ->
                               HsName ->
                               NmLev ->
                               EHCOpts ->
                               ([HsName]) ->
                               ( UID,([HsName]))
sem_ContextedExpressions_Cons :: T_ContextedExpression  ->
                                 T_ContextedExpressions  ->
                                 T_ContextedExpressions 
sem_ContextedExpressions_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_ContextedExpressions_Nil :: T_ContextedExpressions 
sem_ContextedExpressions_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- ContextedPattern --------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Contexted:
         child range          : {Range}
         child pattern        : Pattern 
         child context        : ContextItem 
-}
-- cata
sem_ContextedPattern :: ContextedPattern  ->
                        T_ContextedPattern 
sem_ContextedPattern (ContextedPattern_Contexted _range _pattern _context )  =
    (sem_ContextedPattern_Contexted _range (sem_Pattern _pattern ) (sem_ContextItem _context ) )
-- semantic domain
type T_ContextedPattern  = UID ->
                           HsName ->
                           NmLev ->
                           ([HsName]) ->
                           ( UID,([IdOcc]),([HsName]))
sem_ContextedPattern_Contexted :: Range ->
                                  T_Pattern  ->
                                  T_ContextItem  ->
                                  T_ContextedPattern 
sem_ContextedPattern_Contexted range_ pattern_ context_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _contextIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _contextOgUniq =
                  _patternIgUniq
              -- copy rule (down)
              _contextOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _contextOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- ContextedPatterns -------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Cons:
         child hd             : ContextedPattern 
         child tl             : ContextedPatterns 
      alternative Nil:
-}
-- cata
sem_ContextedPatterns :: ContextedPatterns  ->
                         T_ContextedPatterns 
sem_ContextedPatterns list  =
    (Prelude.foldr sem_ContextedPatterns_Cons sem_ContextedPatterns_Nil (Prelude.map sem_ContextedPattern list) )
-- semantic domain
type T_ContextedPatterns  = UID ->
                            HsName ->
                            NmLev ->
                            ([HsName]) ->
                            ( UID,([IdOcc]),([HsName]))
sem_ContextedPatterns_Cons :: T_ContextedPattern  ->
                              T_ContextedPatterns  ->
                              T_ContextedPatterns 
sem_ContextedPatterns_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIidOccDefs :: ([IdOcc])
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIidOccDefs :: ([IdOcc])
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIidOccDefs,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIidOccDefs,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
sem_ContextedPatterns_Nil :: T_ContextedPatterns 
sem_ContextedPatterns_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- Declaration -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
         modDefsRel           : ModEntRel
         modHideDefsRel       : ModEntRel
   alternatives:
      alternative Class:
         child range          : {Range}
         child context        : ContextItems 
         child typelefthandside : TypeLeftHandSide 
         child dependencies   : FunctionalDependencies 
         child where          : MaybeDeclarations 
         visit 0:
            local mbPrevInstancename : _
            local generDerivInfoL : _
            local nmLev       : _
            local instancerefname : _
            local classrefname : _
            local extranames  : _
            local idOccDef    : _
            local idOccDefVal : _
            local idOccDefsSig : _
            local idOccDefs   : _
            local instancename : _
            local modDefsRel  : _
            local _tup1       : _
            local modHideDefsRel : _
            local modEntsSub  : _
            local _tup2       : {(UID,UID)}
            local lUniq       : {UID}
      alternative Data:
         child range          : {Range}
         child context        : ContextItems 
         child typelefthandside : TypeLeftHandSide 
         child constructors   : Constructors 
         child derivings      : Derivings 
         visit 0:
            local nmLev       : _
            local typerefname : _
            local idOccDef    : _
            local idOccDefsCon : _
            local idOccDefsFld : _
            local generMaxRemArity : _
            local idOccDefsGener : _
            local idOccDefs   : _
            local modDefsRel  : _
            local modEntsSub  : _
      alternative Default:
         child range          : {Range}
         child name           : {MaybeName}
         child types          : Types 
         visit 0:
            local nmLev       : _
            local idOccDef    : _
            local idOccDefs   : _
            local defaultname : _
            local _tup3       : {(UID,UID)}
            local lUniq       : {UID}
      alternative Empty:
         child range          : {Range}
      alternative Fixity:
         child range          : {Range}
         child fixity         : {Fixity}
         child priority       : {Maybe Int}
         child operators      : {Names}
         visit 0:
            local oprefnames  : _
      alternative ForeignExport:
         child range          : {Range}
         child callconvention : {FFIWay}
         child exportname     : {Maybe String}
         child name           : {Name}
         child type           : Type 
         visit 0:
            local exportrefname : _
            local idOccDef    : _
            local idOccDefs   : _
      alternative ForeignImport:
         child range          : {Range}
         child callconvention : {FFIWay}
         child safety         : {Maybe String}
         child importname     : {Maybe String}
         child name           : {Name}
         child type           : Type 
         visit 0:
            local refname     : _
            local idOccDef    : _
            local idOccDefs   : _
      alternative FunctionBindings:
         child range          : {Range}
         child bindings       : FunctionBindings 
         visit 0:
            local idOccDefs   : _
      alternative FusionConversion:
         child range          : {Range}
         child absname        : {Name}
         child conname        : {Name}
         visit 0:
            local idOccDef    : _
            local idOccDefs   : _
            local _tup4       : {(UID,UID)}
            local lUniq       : {UID}
      alternative FusionDeclaration:
         child range          : {Range}
         child fusename       : {Name}
         visit 0:
            local idOccDef    : _
            local idOccDefs   : _
            local _tup5       : {(UID,UID)}
            local lUniq       : {UID}
      alternative GADT:
         child range          : {Range}
         child context        : ContextItems 
         child typelefthandside : TypeLeftHandSide 
         child constructors   : Constructors 
         child derivings      : Derivings 
         visit 0:
            local nmLev       : _
            local typerefname : _
            local idOccDef    : _
            local idOccDefsCon : _
            local idOccDefsFld : _
            local generMaxRemArity : _
            local idOccDefsGener : _
            local idOccDefs   : _
            local modDefsRel  : _
            local modEntsSub  : _
      alternative Instance:
         child range          : {Range}
         child instVariant    : {InstVariant}
         child maybeinstancename : {MaybeName}
         child useimplicitly  : {Bool}
         child context        : ContextItems 
         child type           : Type 
         child where          : MaybeDeclarations 
         visit 0:
            local mbPrevInstancename : _
            local nmLev       : _
            local instancerefname : _
            local idOccDef    : _
            local idOccDefVal : _
            local idOccDefs   : _
            local instancename : _
            local _tup6       : {(ModEntRel,ModEntRel)}
            local _tup7       : {(UID,UID)}
            local lUniq       : {UID}
      alternative InstanceUseImplicitly:
         child range          : {Range}
         child expression     : Expression 
         child name           : {Name}
         child types          : Types 
         visit 0:
            local mbPrevInstancename : _
            local nmLev       : _
            local instancerefname : _
            local idOccDef    : _
            local idOccDefs   : _
            local instancename : _
            local _tup8       : {(UID,UID)}
            local lUniq       : {UID}
      alternative KindSignature:
         child range          : {Range}
         child names          : {Names}
         child kind           : Kind 
         visit 0:
            local nmLev       : _
            local refnames    : _
            local idOccDefs   : _
      alternative Module:
         child range          : {Range}
         child name           : {MaybeName}
         child exports        : MaybeExports 
      alternative Newtype:
         child range          : {Range}
         child context        : ContextItems 
         child typelefthandside : TypeLeftHandSide 
         child constructors   : Constructor 
         child derivings      : Derivings 
         visit 0:
            local nmLev       : _
            local typerefname : _
            local idOccDef    : _
            local idOccDefsCon : _
            local idOccDefsFld : _
            local generMaxRemArity : _
            local idOccDefsGener : _
            local idOccDefs   : _
            local modDefsRel  : _
            local modEntsSub  : _
      alternative PatternBinding:
         child range          : {Range}
         child pattern        : Pattern 
         child righthandside  : RightHandSide 
         visit 0:
            local idOccDef    : _
            local idOccDefsPat : _
            local idOccDefs   : _
            local _tup9       : {(UID,UID)}
            local lUniq       : {UID}
      alternative Pragma:
         child range          : {Range}
         child pragma         : Pragma 
      alternative Type:
         child range          : {Range}
         child typelefthandside : TypeLeftHandSide 
         child type           : Type 
         visit 0:
            local nmLev       : _
            local typerefname : _
            local idOccDef    : _
            local idOccDefs   : _
      alternative TypeSignature:
         child range          : {Range}
         child names          : {Names}
         child type           : Type 
         visit 0:
            local nmLev       : _
            local refnames    : _
            local idOccDefs   : _
-}
-- cata
sem_Declaration :: Declaration  ->
                   T_Declaration 
sem_Declaration (Declaration_Class _range _context _typelefthandside _dependencies _where )  =
    (sem_Declaration_Class _range (sem_ContextItems _context ) (sem_TypeLeftHandSide _typelefthandside ) (sem_FunctionalDependencies _dependencies ) (sem_MaybeDeclarations _where ) )
sem_Declaration (Declaration_Data _range _context _typelefthandside _constructors _derivings )  =
    (sem_Declaration_Data _range (sem_ContextItems _context ) (sem_TypeLeftHandSide _typelefthandside ) (sem_Constructors _constructors ) (sem_Derivings _derivings ) )
sem_Declaration (Declaration_Default _range _name _types )  =
    (sem_Declaration_Default _range _name (sem_Types _types ) )
sem_Declaration (Declaration_Empty _range )  =
    (sem_Declaration_Empty _range )
sem_Declaration (Declaration_Fixity _range _fixity _priority _operators )  =
    (sem_Declaration_Fixity _range _fixity _priority _operators )
sem_Declaration (Declaration_ForeignExport _range _callconvention _exportname _name _type )  =
    (sem_Declaration_ForeignExport _range _callconvention _exportname _name (sem_Type _type ) )
sem_Declaration (Declaration_ForeignImport _range _callconvention _safety _importname _name _type )  =
    (sem_Declaration_ForeignImport _range _callconvention _safety _importname _name (sem_Type _type ) )
sem_Declaration (Declaration_FunctionBindings _range _bindings )  =
    (sem_Declaration_FunctionBindings _range (sem_FunctionBindings _bindings ) )
sem_Declaration (Declaration_FusionConversion _range _absname _conname )  =
    (sem_Declaration_FusionConversion _range _absname _conname )
sem_Declaration (Declaration_FusionDeclaration _range _fusename )  =
    (sem_Declaration_FusionDeclaration _range _fusename )
sem_Declaration (Declaration_GADT _range _context _typelefthandside _constructors _derivings )  =
    (sem_Declaration_GADT _range (sem_ContextItems _context ) (sem_TypeLeftHandSide _typelefthandside ) (sem_Constructors _constructors ) (sem_Derivings _derivings ) )
sem_Declaration (Declaration_Instance _range _instVariant _maybeinstancename _useimplicitly _context _type _where )  =
    (sem_Declaration_Instance _range _instVariant _maybeinstancename _useimplicitly (sem_ContextItems _context ) (sem_Type _type ) (sem_MaybeDeclarations _where ) )
sem_Declaration (Declaration_InstanceUseImplicitly _range _expression _name _types )  =
    (sem_Declaration_InstanceUseImplicitly _range (sem_Expression _expression ) _name (sem_Types _types ) )
sem_Declaration (Declaration_KindSignature _range _names _kind )  =
    (sem_Declaration_KindSignature _range _names (sem_Kind _kind ) )
sem_Declaration (Declaration_Module _range _name _exports )  =
    (sem_Declaration_Module _range _name (sem_MaybeExports _exports ) )
sem_Declaration (Declaration_Newtype _range _context _typelefthandside _constructors _derivings )  =
    (sem_Declaration_Newtype _range (sem_ContextItems _context ) (sem_TypeLeftHandSide _typelefthandside ) (sem_Constructor _constructors ) (sem_Derivings _derivings ) )
sem_Declaration (Declaration_PatternBinding _range _pattern _righthandside )  =
    (sem_Declaration_PatternBinding _range (sem_Pattern _pattern ) (sem_RightHandSide _righthandside ) )
sem_Declaration (Declaration_Pragma _range _pragma )  =
    (sem_Declaration_Pragma _range (sem_Pragma _pragma ) )
sem_Declaration (Declaration_Type _range _typelefthandside _type )  =
    (sem_Declaration_Type _range (sem_TypeLeftHandSide _typelefthandside ) (sem_Type _type ) )
sem_Declaration (Declaration_TypeSignature _range _names _type )  =
    (sem_Declaration_TypeSignature _range _names (sem_Type _type ) )
-- semantic domain
type T_Declaration  = UID ->
                      HsName ->
                      NmLev ->
                      EHCOpts ->
                      ([HsName]) ->
                      ( UID,(Set.Set Pragma.Pragma),([IdOcc]),Bool,ModEntRel,ModEntRel,([HsName]))
sem_Declaration_Class :: Range ->
                         T_ContextItems  ->
                         T_TypeLeftHandSide  ->
                         T_FunctionalDependencies  ->
                         T_MaybeDeclarations  ->
                         T_Declaration 
sem_Declaration_Class range_ context_ typelefthandside_ dependencies_ where_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOtopInstanceNmL :: ([HsName])
              _lhsOmodDefsRel :: ModEntRel
              __tup2 :: ((UID,UID))
              _contextOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _typelefthandsideOgUniq :: UID
              _typelefthandsideOmoduleNm :: HsName
              _typelefthandsideOnmLev :: NmLev
              _typelefthandsideOtopInstanceNmL :: ([HsName])
              _dependenciesOgUniq :: UID
              _dependenciesOmoduleNm :: HsName
              _dependenciesOnmLev :: NmLev
              _dependenciesOopts :: EHCOpts
              _dependenciesOtopInstanceNmL :: ([HsName])
              _whereOgUniq :: UID
              _whereOmoduleNm :: HsName
              _whereOnmLev :: NmLev
              _whereOopts :: EHCOpts
              _whereOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _typelefthandsideIgUniq :: UID
              _typelefthandsideIidOccDefs :: ([IdOcc])
              _typelefthandsideIname :: Name
              _typelefthandsideIrange :: Range
              _typelefthandsideItopInstanceNmL :: ([HsName])
              _typelefthandsideItypevariables :: Names
              _dependenciesIgUniq :: UID
              _dependenciesIgathPragmas :: (Set.Set Pragma.Pragma)
              _dependenciesIidOccDefs :: ([IdOcc])
              _dependenciesItopInstanceNmL :: ([HsName])
              _whereIgUniq :: UID
              _whereIgathPragmas :: (Set.Set Pragma.Pragma)
              _whereIidOccDefs :: ([IdOcc])
              _whereImainValExists :: Bool
              _whereItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 46, column 9)
              _lhsOtopInstanceNmL =
                  _instancename : _lhsItopInstanceNmL
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 61, column 9)
              _mbPrevInstancename =
                  Nothing
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 86, column 9)
              _generDerivInfoL =
                  []
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 24, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 22, column 9)
              _instancerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm
                  $ maybe _instancename id _mbPrevInstancename
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 26, column 9)
              _classrefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm _typelefthandsideIname
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 96, column 17)
              _extranames =
                  []
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 139, column 9)
              _idOccDef =
                  IdOcc _classrefname IdOcc_Class
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 139, column 9)
              _idOccDefVal =
                  IdOcc _instancerefname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 139, column 9)
              _idOccDefsSig =
                  _whereIidOccDefs
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 139, column 9)
              _idOccDefs =
                  [_idOccDef] ++ _idOccDefsSig
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 236, column 9)
              _instancename =
                  hsnUniqifyUID HsNameUniqifier_Class _lUniq (mkHNm _typelefthandsideIname)
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 131, column 9)
              _modDefsRel =
                  Rel.singleton
                    _typelefthandsideIname
                    (ModEnt IdOcc_Class _idOccDef (Set.fromList _modEntsSub) range_)
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 134, column 29)
              __tup1 =
                  mkInstDefsRel False _instancename Nothing _idOccDef _idOccDefVal
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 134, column 29)
              (_,_modHideDefsRel) =
                  __tup1
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 131, column 9)
              _modEntsSub =
                  [ ModEnt IdOcc_Val o Set.empty range_ | o <- _idOccDefsSig ]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 138, column 9)
              _lhsOmodDefsRel =
                  _modDefsRel
                  `Rel.union`
                  Rel.fromList [ (hsnQualified $ ioccNm $ mentIdOcc e,e) | e <- _modEntsSub ]
              -- -- generated by the unique rule mechanism.
              __tup2 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_contextOgUniq,_) =
                  __tup2
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup2
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _dependenciesIgathPragmas `Set.union` _whereIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _whereImainValExists
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _modHideDefsRel
              -- copy rule (up)
              _lhsOgUniq =
                  _whereIgUniq
              -- copy rule (from local)
              _contextOnmLev =
                  _nmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typelefthandsideOgUniq =
                  _contextIgUniq
              -- copy rule (down)
              _typelefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _typelefthandsideOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typelefthandsideOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (chain)
              _dependenciesOgUniq =
                  _typelefthandsideIgUniq
              -- copy rule (down)
              _dependenciesOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _dependenciesOnmLev =
                  _nmLev
              -- copy rule (down)
              _dependenciesOopts =
                  _lhsIopts
              -- copy rule (chain)
              _dependenciesOtopInstanceNmL =
                  _typelefthandsideItopInstanceNmL
              -- copy rule (chain)
              _whereOgUniq =
                  _dependenciesIgUniq
              -- copy rule (down)
              _whereOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _whereOnmLev =
                  _nmLev
              -- copy rule (down)
              _whereOopts =
                  _lhsIopts
              -- copy rule (chain)
              _whereOtopInstanceNmL =
                  _dependenciesItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _typelefthandsideIgUniq,_typelefthandsideIidOccDefs,_typelefthandsideIname,_typelefthandsideIrange,_typelefthandsideItopInstanceNmL,_typelefthandsideItypevariables) | True =
                  typelefthandside_ _typelefthandsideOgUniq _typelefthandsideOmoduleNm _typelefthandsideOnmLev _typelefthandsideOtopInstanceNmL 
              ( _dependenciesIgUniq,_dependenciesIgathPragmas,_dependenciesIidOccDefs,_dependenciesItopInstanceNmL) | True =
                  dependencies_ _dependenciesOgUniq _dependenciesOmoduleNm _dependenciesOnmLev _dependenciesOopts _dependenciesOtopInstanceNmL 
              ( _whereIgUniq,_whereIgathPragmas,_whereIidOccDefs,_whereImainValExists,_whereItopInstanceNmL) | True =
                  where_ _whereOgUniq _whereOmoduleNm _whereOnmLev _whereOopts _whereOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Data :: Range ->
                        T_ContextItems  ->
                        T_TypeLeftHandSide  ->
                        T_Constructors  ->
                        T_Derivings  ->
                        T_Declaration 
sem_Declaration_Data range_ context_ typelefthandside_ constructors_ derivings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _typelefthandsideOgUniq :: UID
              _typelefthandsideOmoduleNm :: HsName
              _typelefthandsideOnmLev :: NmLev
              _typelefthandsideOtopInstanceNmL :: ([HsName])
              _constructorsOgUniq :: UID
              _constructorsOmoduleNm :: HsName
              _constructorsOnmLev :: NmLev
              _constructorsOtopInstanceNmL :: ([HsName])
              _derivingsOgUniq :: UID
              _derivingsOmoduleNm :: HsName
              _derivingsOnmLev :: NmLev
              _derivingsOopts :: EHCOpts
              _derivingsOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _typelefthandsideIgUniq :: UID
              _typelefthandsideIidOccDefs :: ([IdOcc])
              _typelefthandsideIname :: Name
              _typelefthandsideIrange :: Range
              _typelefthandsideItopInstanceNmL :: ([HsName])
              _typelefthandsideItypevariables :: Names
              _constructorsIconAndFldrefnames :: (AssocL Name Names)
              _constructorsIconrefnames :: Names
              _constructorsIgUniq :: UID
              _constructorsItopInstanceNmL :: ([HsName])
              _derivingsIgUniq :: UID
              _derivingsIgathPragmas :: (Set.Set Pragma.Pragma)
              _derivingsIidOccDefs :: ([IdOcc])
              _derivingsImodDefsRel :: ModEntRel
              _derivingsImodHideDefsRel :: ModEntRel
              _derivingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 20, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 18, column 9)
              _typerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm _typelefthandsideIname
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDef =
                  IdOcc _typerefname IdOcc_Type
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsCon =
                  [ IdOcc n IdOcc_Val | n <- _constructorsIconrefnames ]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsFld =
                  concat [ [IdOcc n IdOcc_Val, IdOcc n IdOcc_Fld] | (_,ns) <- _constructorsIconAndFldrefnames, n <- ns ]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _generMaxRemArity =
                  length _typelefthandsideItypevariables `min` 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsGener =
                  if ehcOptGenGenerics _lhsIopts
                  then [ IdOcc (hsnNm2GenerReprSyn arity _typerefname) IdOcc_Type | arity <- [0 .. _generMaxRemArity] ]
                  else []
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefs =
                  [_idOccDef]
                  ++ _idOccDefsCon
                  ++ _idOccDefsFld
                  ++ _idOccDefsGener
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 121, column 9)
              _modDefsRel =
                  Rel.singleton
                    _typelefthandsideIname
                    (ModEnt IdOcc_Data _idOccDef (Set.fromList _modEntsSub) range_)
                  `Rel.union`
                  _derivingsImodDefsRel
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 121, column 9)
              _modEntsSub =
                  [ ModEnt (ioccKind o) o Set.empty range_
                  | o <- _idOccDefsCon ++ _idOccDefsFld
                         ++ _idOccDefsGener
                  ]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 138, column 9)
              _lhsOmodDefsRel =
                  _modDefsRel
                  `Rel.union`
                  Rel.fromList [ (hsnQualified $ ioccNm $ mentIdOcc e,e) | e <- _modEntsSub ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _derivingsIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _derivingsImodHideDefsRel
              -- copy rule (up)
              _lhsOgUniq =
                  _derivingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _derivingsItopInstanceNmL
              -- copy rule (down)
              _contextOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _contextOnmLev =
                  _nmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typelefthandsideOgUniq =
                  _contextIgUniq
              -- copy rule (down)
              _typelefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _typelefthandsideOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typelefthandsideOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (chain)
              _constructorsOgUniq =
                  _typelefthandsideIgUniq
              -- copy rule (down)
              _constructorsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _constructorsOnmLev =
                  _nmLev
              -- copy rule (chain)
              _constructorsOtopInstanceNmL =
                  _typelefthandsideItopInstanceNmL
              -- copy rule (chain)
              _derivingsOgUniq =
                  _constructorsIgUniq
              -- copy rule (down)
              _derivingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _derivingsOnmLev =
                  _nmLev
              -- copy rule (down)
              _derivingsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _derivingsOtopInstanceNmL =
                  _constructorsItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _typelefthandsideIgUniq,_typelefthandsideIidOccDefs,_typelefthandsideIname,_typelefthandsideIrange,_typelefthandsideItopInstanceNmL,_typelefthandsideItypevariables) | True =
                  typelefthandside_ _typelefthandsideOgUniq _typelefthandsideOmoduleNm _typelefthandsideOnmLev _typelefthandsideOtopInstanceNmL 
              ( _constructorsIconAndFldrefnames,_constructorsIconrefnames,_constructorsIgUniq,_constructorsItopInstanceNmL) | True =
                  constructors_ _constructorsOgUniq _constructorsOmoduleNm _constructorsOnmLev _constructorsOtopInstanceNmL 
              ( _derivingsIgUniq,_derivingsIgathPragmas,_derivingsIidOccDefs,_derivingsImodDefsRel,_derivingsImodHideDefsRel,_derivingsItopInstanceNmL) | True =
                  derivings_ _derivingsOgUniq _derivingsOmoduleNm _derivingsOnmLev _derivingsOopts _derivingsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Default :: Range ->
                           MaybeName ->
                           T_Types  ->
                           T_Declaration 
sem_Declaration_Default range_ name_ types_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let __tup3 :: ((UID,UID))
              _typesOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typesOnmLev :: NmLev
              _typesOtopInstanceNmL :: ([HsName])
              _typesIgUniq :: UID
              _typesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 24, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 151, column 9)
              _idOccDef =
                  IdOcc _defaultname IdOcc_Dflt
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 151, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 240, column 9)
              _defaultname =
                  mkHNm _lUniq
              -- -- generated by the unique rule mechanism.
              __tup3 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_typesOgUniq,_) =
                  __tup3
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup3
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _typesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typesItopInstanceNmL
              -- copy rule (from local)
              _typesOnmLev =
                  _nmLev
              -- copy rule (down)
              _typesOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typesIgUniq,_typesItopInstanceNmL) | True =
                  types_ _typesOgUniq _typesOnmLev _typesOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Empty :: Range ->
                         T_Declaration 
sem_Declaration_Empty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Fixity :: Range ->
                          Fixity ->
                          (Maybe Int) ->
                          Names ->
                          T_Declaration 
sem_Declaration_Fixity range_ fixity_ priority_ operators_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 12, column 9)
              _oprefnames =
                  map (hsnSetLevQual _lhsInmLev _lhsImoduleNm) operators_
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_ForeignExport :: Range ->
                                 FFIWay ->
                                 (Maybe String) ->
                                 Name ->
                                 T_Type  ->
                                 T_Declaration 
sem_Declaration_ForeignExport range_ callconvention_ exportname_ name_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 32, column 9)
              _exportrefname =
                  hsnUniqify HsNameUniqifier_FFE name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 160, column 9)
              _idOccDef =
                  IdOcc _exportrefname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 160, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_ForeignImport :: Range ->
                                 FFIWay ->
                                 (Maybe String) ->
                                 (Maybe String) ->
                                 Name ->
                                 T_Type  ->
                                 T_Declaration 
sem_Declaration_ForeignImport range_ callconvention_ safety_ importname_ name_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 20, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 157, column 9)
              _idOccDef =
                  IdOcc _refname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 157, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 117, column 9)
              _lhsOmodDefsRel =
                  Rel.fromList [ (hsnQualified (ioccNm o),ModEnt IdOcc_Val o Set.empty range_) | o <- _idOccDefs ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_FunctionBindings :: Range ->
                                    T_FunctionBindings  ->
                                    T_Declaration 
sem_Declaration_FunctionBindings range_ bindings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _bindingsOgUniq :: UID
              _bindingsOmoduleNm :: HsName
              _bindingsOnmLev :: NmLev
              _bindingsOopts :: EHCOpts
              _bindingsOtopInstanceNmL :: ([HsName])
              _bindingsIgUniq :: UID
              _bindingsIidOccDefs :: ([IdOcc])
              _bindingsImainValExists :: Bool
              _bindingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 115, column 9)
              _idOccDefs =
                  _bindingsIidOccDefs
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 117, column 9)
              _lhsOmodDefsRel =
                  Rel.fromList [ (hsnQualified (ioccNm o),ModEnt IdOcc_Val o Set.empty range_) | o <- _idOccDefs ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _bindingsImainValExists
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _bindingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _bindingsItopInstanceNmL
              -- copy rule (down)
              _bindingsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bindingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _bindingsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _bindingsOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindingsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _bindingsIgUniq,_bindingsIidOccDefs,_bindingsImainValExists,_bindingsItopInstanceNmL) | True =
                  bindings_ _bindingsOgUniq _bindingsOmoduleNm _bindingsOnmLev _bindingsOopts _bindingsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_FusionConversion :: Range ->
                                    Name ->
                                    Name ->
                                    T_Declaration 
sem_Declaration_FusionConversion range_ absname_ conname_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let __tup4 :: ((UID,UID))
              _lhsOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 163, column 9)
              _idOccDef =
                  IdOcc (mkHNm _lUniq) IdOcc_Fusion
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 163, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- -- generated by the unique rule mechanism.
              __tup4 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_lhsOgUniq,_) =
                  __tup4
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup4
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_FusionDeclaration :: Range ->
                                     Name ->
                                     T_Declaration 
sem_Declaration_FusionDeclaration range_ fusename_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let __tup5 :: ((UID,UID))
              _lhsOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 163, column 9)
              _idOccDef =
                  IdOcc (mkHNm _lUniq) IdOcc_Fusion
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 163, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- -- generated by the unique rule mechanism.
              __tup5 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_lhsOgUniq,_) =
                  __tup5
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup5
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_GADT :: Range ->
                        T_ContextItems  ->
                        T_TypeLeftHandSide  ->
                        T_Constructors  ->
                        T_Derivings  ->
                        T_Declaration 
sem_Declaration_GADT range_ context_ typelefthandside_ constructors_ derivings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _typelefthandsideOgUniq :: UID
              _typelefthandsideOmoduleNm :: HsName
              _typelefthandsideOnmLev :: NmLev
              _typelefthandsideOtopInstanceNmL :: ([HsName])
              _constructorsOgUniq :: UID
              _constructorsOmoduleNm :: HsName
              _constructorsOnmLev :: NmLev
              _constructorsOtopInstanceNmL :: ([HsName])
              _derivingsOgUniq :: UID
              _derivingsOmoduleNm :: HsName
              _derivingsOnmLev :: NmLev
              _derivingsOopts :: EHCOpts
              _derivingsOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _typelefthandsideIgUniq :: UID
              _typelefthandsideIidOccDefs :: ([IdOcc])
              _typelefthandsideIname :: Name
              _typelefthandsideIrange :: Range
              _typelefthandsideItopInstanceNmL :: ([HsName])
              _typelefthandsideItypevariables :: Names
              _constructorsIconAndFldrefnames :: (AssocL Name Names)
              _constructorsIconrefnames :: Names
              _constructorsIgUniq :: UID
              _constructorsItopInstanceNmL :: ([HsName])
              _derivingsIgUniq :: UID
              _derivingsIgathPragmas :: (Set.Set Pragma.Pragma)
              _derivingsIidOccDefs :: ([IdOcc])
              _derivingsImodDefsRel :: ModEntRel
              _derivingsImodHideDefsRel :: ModEntRel
              _derivingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 20, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 18, column 9)
              _typerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm _typelefthandsideIname
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDef =
                  IdOcc _typerefname IdOcc_Type
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsCon =
                  [ IdOcc n IdOcc_Val | n <- _constructorsIconrefnames ]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsFld =
                  concat [ [IdOcc n IdOcc_Val, IdOcc n IdOcc_Fld] | (_,ns) <- _constructorsIconAndFldrefnames, n <- ns ]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _generMaxRemArity =
                  length _typelefthandsideItypevariables `min` 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsGener =
                  if ehcOptGenGenerics _lhsIopts
                  then [ IdOcc (hsnNm2GenerReprSyn arity _typerefname) IdOcc_Type | arity <- [0 .. _generMaxRemArity] ]
                  else []
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefs =
                  [_idOccDef]
                  ++ _idOccDefsCon
                  ++ _idOccDefsFld
                  ++ _idOccDefsGener
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 121, column 9)
              _modDefsRel =
                  Rel.singleton
                    _typelefthandsideIname
                    (ModEnt IdOcc_Data _idOccDef (Set.fromList _modEntsSub) range_)
                  `Rel.union`
                  _derivingsImodDefsRel
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 121, column 9)
              _modEntsSub =
                  [ ModEnt (ioccKind o) o Set.empty range_
                  | o <- _idOccDefsCon ++ _idOccDefsFld
                         ++ _idOccDefsGener
                  ]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 138, column 9)
              _lhsOmodDefsRel =
                  _modDefsRel
                  `Rel.union`
                  Rel.fromList [ (hsnQualified $ ioccNm $ mentIdOcc e,e) | e <- _modEntsSub ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _derivingsIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _derivingsImodHideDefsRel
              -- copy rule (up)
              _lhsOgUniq =
                  _derivingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _derivingsItopInstanceNmL
              -- copy rule (down)
              _contextOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _contextOnmLev =
                  _nmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typelefthandsideOgUniq =
                  _contextIgUniq
              -- copy rule (down)
              _typelefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _typelefthandsideOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typelefthandsideOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (chain)
              _constructorsOgUniq =
                  _typelefthandsideIgUniq
              -- copy rule (down)
              _constructorsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _constructorsOnmLev =
                  _nmLev
              -- copy rule (chain)
              _constructorsOtopInstanceNmL =
                  _typelefthandsideItopInstanceNmL
              -- copy rule (chain)
              _derivingsOgUniq =
                  _constructorsIgUniq
              -- copy rule (down)
              _derivingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _derivingsOnmLev =
                  _nmLev
              -- copy rule (down)
              _derivingsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _derivingsOtopInstanceNmL =
                  _constructorsItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _typelefthandsideIgUniq,_typelefthandsideIidOccDefs,_typelefthandsideIname,_typelefthandsideIrange,_typelefthandsideItopInstanceNmL,_typelefthandsideItypevariables) | True =
                  typelefthandside_ _typelefthandsideOgUniq _typelefthandsideOmoduleNm _typelefthandsideOnmLev _typelefthandsideOtopInstanceNmL 
              ( _constructorsIconAndFldrefnames,_constructorsIconrefnames,_constructorsIgUniq,_constructorsItopInstanceNmL) | True =
                  constructors_ _constructorsOgUniq _constructorsOmoduleNm _constructorsOnmLev _constructorsOtopInstanceNmL 
              ( _derivingsIgUniq,_derivingsIgathPragmas,_derivingsIidOccDefs,_derivingsImodDefsRel,_derivingsImodHideDefsRel,_derivingsItopInstanceNmL) | True =
                  derivings_ _derivingsOgUniq _derivingsOmoduleNm _derivingsOnmLev _derivingsOopts _derivingsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Instance :: Range ->
                            InstVariant ->
                            MaybeName ->
                            Bool ->
                            T_ContextItems  ->
                            T_Type  ->
                            T_MaybeDeclarations  ->
                            T_Declaration 
sem_Declaration_Instance range_ instVariant_ maybeinstancename_ useimplicitly_ context_ type_ where_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOtopInstanceNmL :: ([HsName])
              __tup6 :: ((ModEntRel,ModEntRel))
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              __tup7 :: ((UID,UID))
              _contextOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _whereOgUniq :: UID
              _whereOmoduleNm :: HsName
              _whereOnmLev :: NmLev
              _whereOopts :: EHCOpts
              _whereOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              _whereIgUniq :: UID
              _whereIgathPragmas :: (Set.Set Pragma.Pragma)
              _whereIidOccDefs :: ([IdOcc])
              _whereImainValExists :: Bool
              _whereItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 46, column 9)
              _lhsOtopInstanceNmL =
                  _instancename : _lhsItopInstanceNmL
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 61, column 9)
              _mbPrevInstancename =
                  Nothing
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 26, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 22, column 9)
              _instancerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm
                  $ maybe _instancename id _mbPrevInstancename
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 144, column 9)
              _idOccDef =
                  IdOcc _instancerefname IdOcc_Inst
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 144, column 9)
              _idOccDefVal =
                  IdOcc _instancerefname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 144, column 9)
              _idOccDefs =
                  [_idOccDef,_idOccDefVal]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 234, column 9)
              _instancename =
                  maybe (hsnUniqifyUID HsNameUniqifier_ClassDict _lUniq (_typeIconNm)) id maybeinstancename_
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 142, column 29)
              __tup6 =
                  mkInstDefsRel useimplicitly_ _instancename maybeinstancename_ _idOccDef _idOccDefVal
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 142, column 29)
              (_lhsOmodDefsRel,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 142, column 29)
              (_,_lhsOmodHideDefsRel) =
                  __tup6
              -- -- generated by the unique rule mechanism.
              __tup7 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_contextOgUniq,_) =
                  __tup7
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup7
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _whereIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _whereImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _whereIgUniq
              -- copy rule (from local)
              _contextOnmLev =
                  _nmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _contextIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (chain)
              _whereOgUniq =
                  _typeIgUniq
              -- copy rule (down)
              _whereOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _whereOnmLev =
                  _nmLev
              -- copy rule (down)
              _whereOopts =
                  _lhsIopts
              -- copy rule (chain)
              _whereOtopInstanceNmL =
                  _typeItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
              ( _whereIgUniq,_whereIgathPragmas,_whereIidOccDefs,_whereImainValExists,_whereItopInstanceNmL) | True =
                  where_ _whereOgUniq _whereOmoduleNm _whereOnmLev _whereOopts _whereOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_InstanceUseImplicitly :: Range ->
                                         T_Expression  ->
                                         Name ->
                                         T_Types  ->
                                         T_Declaration 
sem_Declaration_InstanceUseImplicitly range_ expression_ name_ types_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOtopInstanceNmL :: ([HsName])
              _lhsOmodDefsRel :: ModEntRel
              __tup8 :: ((UID,UID))
              _expressionOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _typesOgUniq :: UID
              _typesOnmLev :: NmLev
              _typesOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _typesIgUniq :: UID
              _typesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 46, column 9)
              _lhsOtopInstanceNmL =
                  _instancename : _lhsItopInstanceNmL
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 61, column 9)
              _mbPrevInstancename =
                  Nothing
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 24, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 22, column 9)
              _instancerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm
                  $ maybe _instancename id _mbPrevInstancename
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 148, column 9)
              _idOccDef =
                  IdOcc _instancerefname IdOcc_Inst
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 148, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 238, column 9)
              _instancename =
                  mkHNm _lUniq
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 145, column 9)
              _lhsOmodDefsRel =
                  Rel.singleton _instancename (ModEnt IdOcc_Inst _idOccDef Set.empty range_)
              -- -- generated by the unique rule mechanism.
              __tup8 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_expressionOgUniq,_) =
                  __tup8
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup8
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _typesIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _expressionOnmLev =
                  _nmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typesOgUniq =
                  _expressionIgUniq
              -- copy rule (from local)
              _typesOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typesOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _typesIgUniq,_typesItopInstanceNmL) | True =
                  types_ _typesOgUniq _typesOnmLev _typesOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_KindSignature :: Range ->
                                 Names ->
                                 T_Kind  ->
                                 T_Declaration 
sem_Declaration_KindSignature range_ names_ kind_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _kindOgUniq :: UID
              _kindOnmLev :: NmLev
              _kindOtopInstanceNmL :: ([HsName])
              _kindIgUniq :: UID
              _kindItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 22, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 16, column 9)
              _refnames =
                  map (hsnSetLevQual _lhsInmLev _lhsImoduleNm) names_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 137, column 9)
              _idOccDefs =
                  [ IdOcc n IdOcc_Type | n <- _refnames ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _kindIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _kindItopInstanceNmL
              -- copy rule (down)
              _kindOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _kindOnmLev =
                  _nmLev
              -- copy rule (down)
              _kindOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _kindIgUniq,_kindItopInstanceNmL) | True =
                  kind_ _kindOgUniq _kindOnmLev _kindOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Module :: Range ->
                          MaybeName ->
                          T_MaybeExports  ->
                          T_Declaration 
sem_Declaration_Module range_ name_ exports_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _exportsOgUniq :: UID
              _exportsOmoduleNm :: HsName
              _exportsOnmLev :: NmLev
              _exportsOopts :: EHCOpts
              _exportsOtopInstanceNmL :: ([HsName])
              _exportsIgUniq :: UID
              _exportsImodExpsMb :: (Maybe [ModExp])
              _exportsItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _exportsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _exportsItopInstanceNmL
              -- copy rule (down)
              _exportsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exportsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _exportsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _exportsOopts =
                  _lhsIopts
              -- copy rule (down)
              _exportsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _exportsIgUniq,_exportsImodExpsMb,_exportsItopInstanceNmL) | True =
                  exports_ _exportsOgUniq _exportsOmoduleNm _exportsOnmLev _exportsOopts _exportsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Newtype :: Range ->
                           T_ContextItems  ->
                           T_TypeLeftHandSide  ->
                           T_Constructor  ->
                           T_Derivings  ->
                           T_Declaration 
sem_Declaration_Newtype range_ context_ typelefthandside_ constructors_ derivings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _typelefthandsideOgUniq :: UID
              _typelefthandsideOmoduleNm :: HsName
              _typelefthandsideOnmLev :: NmLev
              _typelefthandsideOtopInstanceNmL :: ([HsName])
              _constructorsOgUniq :: UID
              _constructorsOmoduleNm :: HsName
              _constructorsOnmLev :: NmLev
              _constructorsOtopInstanceNmL :: ([HsName])
              _derivingsOgUniq :: UID
              _derivingsOmoduleNm :: HsName
              _derivingsOnmLev :: NmLev
              _derivingsOopts :: EHCOpts
              _derivingsOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _typelefthandsideIgUniq :: UID
              _typelefthandsideIidOccDefs :: ([IdOcc])
              _typelefthandsideIname :: Name
              _typelefthandsideIrange :: Range
              _typelefthandsideItopInstanceNmL :: ([HsName])
              _typelefthandsideItypevariables :: Names
              _constructorsIconAndFldrefnames :: (AssocL Name Names)
              _constructorsIconrefnames :: Names
              _constructorsIgUniq :: UID
              _constructorsItopInstanceNmL :: ([HsName])
              _derivingsIgUniq :: UID
              _derivingsIgathPragmas :: (Set.Set Pragma.Pragma)
              _derivingsIidOccDefs :: ([IdOcc])
              _derivingsImodDefsRel :: ModEntRel
              _derivingsImodHideDefsRel :: ModEntRel
              _derivingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 20, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 18, column 9)
              _typerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm _typelefthandsideIname
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDef =
                  IdOcc _typerefname IdOcc_Type
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsCon =
                  [ IdOcc n IdOcc_Val | n <- _constructorsIconrefnames ]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsFld =
                  concat [ [IdOcc n IdOcc_Val, IdOcc n IdOcc_Fld] | (_,ns) <- _constructorsIconAndFldrefnames, n <- ns ]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _generMaxRemArity =
                  length _typelefthandsideItypevariables `min` 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefsGener =
                  if ehcOptGenGenerics _lhsIopts
                  then [ IdOcc (hsnNm2GenerReprSyn arity _typerefname) IdOcc_Type | arity <- [0 .. _generMaxRemArity] ]
                  else []
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 123, column 9)
              _idOccDefs =
                  [_idOccDef]
                  ++ _idOccDefsCon
                  ++ _idOccDefsFld
                  ++ _idOccDefsGener
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 121, column 9)
              _modDefsRel =
                  Rel.singleton
                    _typelefthandsideIname
                    (ModEnt IdOcc_Data _idOccDef (Set.fromList _modEntsSub) range_)
                  `Rel.union`
                  _derivingsImodDefsRel
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 121, column 9)
              _modEntsSub =
                  [ ModEnt (ioccKind o) o Set.empty range_
                  | o <- _idOccDefsCon ++ _idOccDefsFld
                         ++ _idOccDefsGener
                  ]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 138, column 9)
              _lhsOmodDefsRel =
                  _modDefsRel
                  `Rel.union`
                  Rel.fromList [ (hsnQualified $ ioccNm $ mentIdOcc e,e) | e <- _modEntsSub ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _derivingsIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _derivingsImodHideDefsRel
              -- copy rule (up)
              _lhsOgUniq =
                  _derivingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _derivingsItopInstanceNmL
              -- copy rule (down)
              _contextOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _contextOnmLev =
                  _nmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typelefthandsideOgUniq =
                  _contextIgUniq
              -- copy rule (down)
              _typelefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _typelefthandsideOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typelefthandsideOtopInstanceNmL =
                  _contextItopInstanceNmL
              -- copy rule (chain)
              _constructorsOgUniq =
                  _typelefthandsideIgUniq
              -- copy rule (down)
              _constructorsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _constructorsOnmLev =
                  _nmLev
              -- copy rule (chain)
              _constructorsOtopInstanceNmL =
                  _typelefthandsideItopInstanceNmL
              -- copy rule (chain)
              _derivingsOgUniq =
                  _constructorsIgUniq
              -- copy rule (down)
              _derivingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _derivingsOnmLev =
                  _nmLev
              -- copy rule (down)
              _derivingsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _derivingsOtopInstanceNmL =
                  _constructorsItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _typelefthandsideIgUniq,_typelefthandsideIidOccDefs,_typelefthandsideIname,_typelefthandsideIrange,_typelefthandsideItopInstanceNmL,_typelefthandsideItypevariables) | True =
                  typelefthandside_ _typelefthandsideOgUniq _typelefthandsideOmoduleNm _typelefthandsideOnmLev _typelefthandsideOtopInstanceNmL 
              ( _constructorsIconAndFldrefnames,_constructorsIconrefnames,_constructorsIgUniq,_constructorsItopInstanceNmL) | True =
                  constructors_ _constructorsOgUniq _constructorsOmoduleNm _constructorsOnmLev _constructorsOtopInstanceNmL 
              ( _derivingsIgUniq,_derivingsIgathPragmas,_derivingsIidOccDefs,_derivingsImodDefsRel,_derivingsImodHideDefsRel,_derivingsItopInstanceNmL) | True =
                  derivings_ _derivingsOgUniq _derivingsOmoduleNm _derivingsOnmLev _derivingsOopts _derivingsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_PatternBinding :: Range ->
                                  T_Pattern  ->
                                  T_RightHandSide  ->
                                  T_Declaration 
sem_Declaration_PatternBinding range_ pattern_ righthandside_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              __tup9 :: ((UID,UID))
              _patternOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _righthandsideOgUniq :: UID
              _righthandsideOmoduleNm :: HsName
              _righthandsideOnmLev :: NmLev
              _righthandsideOopts :: EHCOpts
              _righthandsideOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _righthandsideIgUniq :: UID
              _righthandsideItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 117, column 9)
              _idOccDef =
                  IdOcc (maybe (mkHNm _lUniq) id _patternImbTopRefname) IdOcc_Pat
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 117, column 9)
              _idOccDefsPat =
                  [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 117, column 9)
              _idOccDefs =
                  _patternIidOccDefs
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 117, column 9)
              _lhsOmodDefsRel =
                  Rel.fromList [ (hsnQualified (ioccNm o),ModEnt IdOcc_Val o Set.empty range_) | o <- _idOccDefs ]
              -- -- generated by the unique rule mechanism.
              __tup9 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_patternOgUniq,_) =
                  __tup9
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup9
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _righthandsideIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _righthandsideItopInstanceNmL
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _righthandsideOgUniq =
                  _patternIgUniq
              -- copy rule (down)
              _righthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _righthandsideOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _righthandsideOopts =
                  _lhsIopts
              -- copy rule (chain)
              _righthandsideOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _righthandsideIgUniq,_righthandsideItopInstanceNmL) | True =
                  righthandside_ _righthandsideOgUniq _righthandsideOmoduleNm _righthandsideOnmLev _righthandsideOopts _righthandsideOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Pragma :: Range ->
                          T_Pragma  ->
                          T_Declaration 
sem_Declaration_Pragma range_ pragma_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _pragmaOgUniq :: UID
              _pragmaOnmLev :: NmLev
              _pragmaOtopInstanceNmL :: ([HsName])
              _pragmaIgUniq :: UID
              _pragmaIgathPragmas :: (Set.Set Pragma.Pragma)
              _pragmaItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _pragmaIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _pragmaIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _pragmaItopInstanceNmL
              -- copy rule (down)
              _pragmaOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _pragmaOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _pragmaOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _pragmaIgUniq,_pragmaIgathPragmas,_pragmaItopInstanceNmL) | True =
                  pragma_ _pragmaOgUniq _pragmaOnmLev _pragmaOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_Type :: Range ->
                        T_TypeLeftHandSide  ->
                        T_Type  ->
                        T_Declaration 
sem_Declaration_Type range_ typelefthandside_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typelefthandsideOgUniq :: UID
              _typelefthandsideOmoduleNm :: HsName
              _typelefthandsideOnmLev :: NmLev
              _typelefthandsideOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typelefthandsideIgUniq :: UID
              _typelefthandsideIidOccDefs :: ([IdOcc])
              _typelefthandsideIname :: Name
              _typelefthandsideIrange :: Range
              _typelefthandsideItopInstanceNmL :: ([HsName])
              _typelefthandsideItypevariables :: Names
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 29, column 9)
              _nmLev =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 28, column 9)
              _typerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm _typelefthandsideIname
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 154, column 9)
              _idOccDef =
                  IdOcc _typerefname IdOcc_Type
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 154, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 119, column 9)
              _lhsOmodDefsRel =
                  Rel.fromList [ (hsnQualified (ioccNm o),ModEnt IdOcc_Type o Set.empty range_) | o <- _idOccDefs ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typelefthandsideOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typelefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _typelefthandsideOnmLev =
                  _nmLev
              -- copy rule (down)
              _typelefthandsideOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _typelefthandsideIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _typelefthandsideItopInstanceNmL
              ( _typelefthandsideIgUniq,_typelefthandsideIidOccDefs,_typelefthandsideIname,_typelefthandsideIrange,_typelefthandsideItopInstanceNmL,_typelefthandsideItypevariables) | True =
                  typelefthandside_ _typelefthandsideOgUniq _typelefthandsideOmoduleNm _typelefthandsideOnmLev _typelefthandsideOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declaration_TypeSignature :: Range ->
                                 Names ->
                                 T_Type  ->
                                 T_Declaration 
sem_Declaration_TypeSignature range_ names_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodDefsRel :: ModEntRel
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 17, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 14, column 9)
              _refnames =
                  map (hsnSetLevQual _lhsInmLev _lhsImoduleNm) names_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 113, column 9)
              _idOccDefs =
                  [ IdOcc n IdOcc_Val | n <- _refnames ]
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 117, column 9)
              _lhsOmodDefsRel =
                  Rel.fromList [ (hsnQualified (ioccNm o),ModEnt IdOcc_Val o Set.empty range_) | o <- _idOccDefs ]
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
-- Declarations ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
         modDefsRel           : ModEntRel
         modHideDefsRel       : ModEntRel
   alternatives:
      alternative Cons:
         child hd             : Declaration 
         child tl             : Declarations 
      alternative Nil:
-}
-- cata
sem_Declarations :: Declarations  ->
                    T_Declarations 
sem_Declarations list  =
    (Prelude.foldr sem_Declarations_Cons sem_Declarations_Nil (Prelude.map sem_Declaration list) )
-- semantic domain
type T_Declarations  = UID ->
                       HsName ->
                       NmLev ->
                       EHCOpts ->
                       ([HsName]) ->
                       ( UID,(Set.Set Pragma.Pragma),([IdOcc]),Bool,ModEntRel,ModEntRel,([HsName]))
sem_Declarations_Cons :: T_Declaration  ->
                         T_Declarations  ->
                         T_Declarations 
sem_Declarations_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIgathPragmas :: (Set.Set Pragma.Pragma)
              _hdIidOccDefs :: ([IdOcc])
              _hdImainValExists :: Bool
              _hdImodDefsRel :: ModEntRel
              _hdImodHideDefsRel :: ModEntRel
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIgathPragmas :: (Set.Set Pragma.Pragma)
              _tlIidOccDefs :: ([IdOcc])
              _tlImainValExists :: Bool
              _tlImodDefsRel :: ModEntRel
              _tlImodHideDefsRel :: ModEntRel
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _hdIgathPragmas `Set.union` _tlIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _hdImainValExists || _tlImainValExists
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  _hdImodDefsRel `Rel.union` _tlImodDefsRel
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _hdImodHideDefsRel `Rel.union` _tlImodHideDefsRel
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIgathPragmas,_hdIidOccDefs,_hdImainValExists,_hdImodDefsRel,_hdImodHideDefsRel,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIgathPragmas,_tlIidOccDefs,_tlImainValExists,_tlImodDefsRel,_tlImodHideDefsRel,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Declarations_Nil :: T_Declarations 
sem_Declarations_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
-- Deriving ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
         modDefsRel           : ModEntRel
         modHideDefsRel       : ModEntRel
   alternatives:
      alternative Deriving:
         child range          : {Range}
         child maybeinstancename : {MaybeName}
         child useimplicitly  : {Bool}
         child name           : {Name}
         visit 0:
            local mbPrevInstancename : _
            local instancerefname : _
            local idOccDef    : _
            local idOccDefVal : _
            local idOccDefs   : _
            local instancename : _
            local _tup10      : {(ModEntRel,ModEntRel)}
            local _tup11      : {(UID,UID)}
            local lUniq       : {UID}
-}
-- cata
sem_Deriving :: Deriving  ->
                T_Deriving 
sem_Deriving (Deriving_Deriving _range _maybeinstancename _useimplicitly _name )  =
    (sem_Deriving_Deriving _range _maybeinstancename _useimplicitly _name )
-- semantic domain
type T_Deriving  = UID ->
                   HsName ->
                   NmLev ->
                   EHCOpts ->
                   ([HsName]) ->
                   ( UID,(Set.Set Pragma.Pragma),([IdOcc]),ModEntRel,ModEntRel,([HsName]))
sem_Deriving_Deriving :: Range ->
                         MaybeName ->
                         Bool ->
                         Name ->
                         T_Deriving 
sem_Deriving_Deriving range_ maybeinstancename_ useimplicitly_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOtopInstanceNmL :: ([HsName])
              __tup10 :: ((ModEntRel,ModEntRel))
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              __tup11 :: ((UID,UID))
              _lhsOgUniq :: UID
              _lUniq :: UID
              _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 56, column 9)
              _lhsOtopInstanceNmL =
                  _instancename : _lhsItopInstanceNmL
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 66, column 9)
              _mbPrevInstancename =
                  Nothing
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 37, column 9)
              _instancerefname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm
                  $ maybe _instancename id _mbPrevInstancename
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 192, column 9)
              _idOccDef =
                  IdOcc _instancerefname IdOcc_Inst
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 192, column 9)
              _idOccDefVal =
                  IdOcc _instancerefname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 192, column 9)
              _idOccDefs =
                  [_idOccDef,_idOccDefVal]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 248, column 9)
              _instancename =
                  maybe (mkHNm _lUniq) id maybeinstancename_
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 149, column 29)
              __tup10 =
                  mkInstDefsRel useimplicitly_ _instancename maybeinstancename_ _idOccDef _idOccDefVal
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 149, column 29)
              (_lhsOmodDefsRel,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 149, column 29)
              (_,_lhsOmodHideDefsRel) =
                  __tup10
              -- -- generated by the unique rule mechanism.
              __tup11 =
                  let __cont = _lhsIgUniq in seq __cont ( case nextUnique __cont of { (__cont, lUniq) -> (__cont, lUniq)} )
              -- -- generated by the unique rule mechanism.
              (_lhsOgUniq,_) =
                  __tup11
              -- -- generated by the unique rule mechanism.
              (_,_lUniq) =
                  __tup11
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
-- Derivings ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
         modDefsRel           : ModEntRel
         modHideDefsRel       : ModEntRel
   alternatives:
      alternative Cons:
         child hd             : Deriving 
         child tl             : Derivings 
      alternative Nil:
-}
-- cata
sem_Derivings :: Derivings  ->
                 T_Derivings 
sem_Derivings list  =
    (Prelude.foldr sem_Derivings_Cons sem_Derivings_Nil (Prelude.map sem_Deriving list) )
-- semantic domain
type T_Derivings  = UID ->
                    HsName ->
                    NmLev ->
                    EHCOpts ->
                    ([HsName]) ->
                    ( UID,(Set.Set Pragma.Pragma),([IdOcc]),ModEntRel,ModEntRel,([HsName]))
sem_Derivings_Cons :: T_Deriving  ->
                      T_Derivings  ->
                      T_Derivings 
sem_Derivings_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIgathPragmas :: (Set.Set Pragma.Pragma)
              _hdIidOccDefs :: ([IdOcc])
              _hdImodDefsRel :: ModEntRel
              _hdImodHideDefsRel :: ModEntRel
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIgathPragmas :: (Set.Set Pragma.Pragma)
              _tlIidOccDefs :: ([IdOcc])
              _tlImodDefsRel :: ModEntRel
              _tlImodHideDefsRel :: ModEntRel
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _hdIgathPragmas `Set.union` _tlIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  _hdImodDefsRel `Rel.union` _tlImodDefsRel
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  _hdImodHideDefsRel `Rel.union` _tlImodHideDefsRel
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIgathPragmas,_hdIidOccDefs,_hdImodDefsRel,_hdImodHideDefsRel,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIgathPragmas,_tlIidOccDefs,_tlImodDefsRel,_tlImodHideDefsRel,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
sem_Derivings_Nil :: T_Derivings 
sem_Derivings_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmodDefsRel :: ModEntRel
              _lhsOmodHideDefsRel :: ModEntRel
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 111, column 26)
              _lhsOmodDefsRel =
                  Rel.empty
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 112, column 26)
              _lhsOmodHideDefsRel =
                  Rel.empty
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmodDefsRel,_lhsOmodHideDefsRel,_lhsOtopInstanceNmL)))
-- Export ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modExp               : ModExp
   alternatives:
      alternative Module:
         child range          : {Range}
         child name           : {Name}
      alternative TypeOrClass:
         child range          : {Range}
         child name           : {Name}
         child names          : {MaybeNames}
      alternative TypeOrClassComplete:
         child range          : {Range}
         child name           : {Name}
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
-}
-- cata
sem_Export :: Export  ->
              T_Export 
sem_Export (Export_Module _range _name )  =
    (sem_Export_Module _range _name )
sem_Export (Export_TypeOrClass _range _name _names )  =
    (sem_Export_TypeOrClass _range _name _names )
sem_Export (Export_TypeOrClassComplete _range _name )  =
    (sem_Export_TypeOrClassComplete _range _name )
sem_Export (Export_Variable _range _name )  =
    (sem_Export_Variable _range _name )
-- semantic domain
type T_Export  = UID ->
                 HsName ->
                 NmLev ->
                 EHCOpts ->
                 ([HsName]) ->
                 ( UID,ModExp,([HsName]))
sem_Export_Module :: Range ->
                     Name ->
                     T_Export 
sem_Export_Module range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExp :: ModExp
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 78, column 9)
              _lhsOmodExp =
                  ModExpMod name_
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodExp,_lhsOtopInstanceNmL)))
sem_Export_TypeOrClass :: Range ->
                          Name ->
                          MaybeNames ->
                          T_Export 
sem_Export_TypeOrClass range_ name_ names_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExp :: ModExp
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 72, column 9)
              _lhsOmodExp =
                  case names_ of
                    Just ns -> ModExpEnt (ModEntSpec name_ range_ (Just (ModEntSubs ns)))
                    Nothing -> ModExpEnt (ModEntSpec name_ range_ Nothing)
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodExp,_lhsOtopInstanceNmL)))
sem_Export_TypeOrClassComplete :: Range ->
                                  Name ->
                                  T_Export 
sem_Export_TypeOrClassComplete range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExp :: ModExp
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 76, column 9)
              _lhsOmodExp =
                  ModExpEnt (ModEntSpec name_ range_ (Just ModEntSubAll))
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodExp,_lhsOtopInstanceNmL)))
sem_Export_Variable :: Range ->
                       Name ->
                       T_Export 
sem_Export_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExp :: ModExp
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 70, column 9)
              _lhsOmodExp =
                  ModExpEnt (ModEntSpec name_ range_ Nothing)
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodExp,_lhsOtopInstanceNmL)))
-- Exports -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modExpL              : [ModExp]
   alternatives:
      alternative Cons:
         child hd             : Export 
         child tl             : Exports 
      alternative Nil:
-}
-- cata
sem_Exports :: Exports  ->
               T_Exports 
sem_Exports list  =
    (Prelude.foldr sem_Exports_Cons sem_Exports_Nil (Prelude.map sem_Export list) )
-- semantic domain
type T_Exports  = UID ->
                  HsName ->
                  NmLev ->
                  EHCOpts ->
                  ([HsName]) ->
                  ( UID,([ModExp]),([HsName]))
sem_Exports_Cons :: T_Export  ->
                    T_Exports  ->
                    T_Exports 
sem_Exports_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExpL :: ([ModExp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdImodExp :: ModExp
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlImodExpL :: ([ModExp])
              _tlItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 84, column 9)
              _lhsOmodExpL =
                  _hdImodExp : _tlImodExpL
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdImodExp,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlImodExpL,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOmodExpL,_lhsOtopInstanceNmL)))
sem_Exports_Nil :: T_Exports 
sem_Exports_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExpL :: ([ModExp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 86, column 9)
              _lhsOmodExpL =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodExpL,_lhsOtopInstanceNmL)))
-- Expression --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         conNm                : Name
   alternatives:
      alternative Annotate:
         child range          : {Range}
         child annotation     : ExpressionAnnotation 
         child expression     : Expression 
      alternative Case:
         child range          : {Range}
         child expression     : Expression 
         child alternatives   : Alternatives 
      alternative Comprehension:
         child range          : {Range}
         child expression     : Expression 
         child qualifiers     : Qualifiers 
      alternative Constructor:
         child range          : {Range}
         child name           : {Name}
      alternative Do:
         child range          : {Range}
         child statements     : Statements 
      alternative Enum:
         child range          : {Range}
         child from           : Expression 
         child then           : MaybeExpression 
         child to             : MaybeExpression 
      alternative If:
         child range          : {Range}
         child guardExpression : Expression 
         child thenExpression : Expression 
         child elseExpression : Expression 
      alternative ImplicitApplication:
         child range          : {Range}
         child function       : Expression 
         child arguments      : ContextedExpressions 
      alternative ImplicitLambda:
         child range          : {Range}
         child patterns       : ContextedPatterns 
         child expression     : Expression 
      alternative ImpredicativeApplication:
         child range          : {Range}
         child function       : Expression 
         child arguments      : Expressions 
      alternative InfixApplication:
         child range          : {Range}
         child leftExpression : Expression 
         child operator       : Expression 
         child rightExpression : Expression 
      alternative InfixApplicationChainTop:
         child range          : {Range}
         child expression     : Expression 
      alternative Lambda:
         child range          : {Range}
         child patterns       : Patterns 
         child expression     : Expression 
         visit 0:
            local nmLev       : _
      alternative Let:
         child range          : {Range}
         child isStrict       : {Bool}
         child declarations   : Declarations 
         child expression     : Expression 
         visit 0:
            local nmLev       : _
      alternative List:
         child range          : {Range}
         child expressions    : Expressions 
      alternative Literal:
         child range          : {Range}
         child literal        : Literal 
      alternative Negate:
         child range          : {Range}
         child expression     : Expression 
      alternative NormalApplication:
         child range          : {Range}
         child function       : Expression 
         child arguments      : Expressions 
      alternative Parenthesized:
         child range          : {Range}
         child expression     : Expression 
      alternative RecordConstruction:
         child range          : {Range}
         child name           : {Name}
         child recordExpressionBindings : RecordExpressionBindings 
      alternative RecordUpdate:
         child range          : {Range}
         child expression     : Expression 
         child recordExpressionBindings : RecordExpressionBindings 
      alternative RowRecordEmpty:
         child range          : {Range}
      alternative RowRecordSelect:
         child range          : {Range}
         child expression     : Expression 
         child name           : {Name}
      alternative RowRecordUpdate:
         child range          : {Range}
         child expression     : Expression 
         child rowRecordExpressionUpdates : RowRecordExpressionUpdates 
      alternative SectionApplication:
         child range          : {Range}
         child leftExpression : MaybeExpression 
         child operator       : Expression 
         child rightExpression : MaybeExpression 
      alternative Tuple:
         child range          : {Range}
         child expressions    : Expressions 
      alternative TupleConstructor:
         child range          : {Range}
         child arity          : {Int}
      alternative Typed:
         child range          : {Range}
         child expression     : Expression 
         child type           : Type 
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
-}
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (Expression_Annotate _range _annotation _expression )  =
    (sem_Expression_Annotate _range (sem_ExpressionAnnotation _annotation ) (sem_Expression _expression ) )
sem_Expression (Expression_Case _range _expression _alternatives )  =
    (sem_Expression_Case _range (sem_Expression _expression ) (sem_Alternatives _alternatives ) )
sem_Expression (Expression_Comprehension _range _expression _qualifiers )  =
    (sem_Expression_Comprehension _range (sem_Expression _expression ) (sem_Qualifiers _qualifiers ) )
sem_Expression (Expression_Constructor _range _name )  =
    (sem_Expression_Constructor _range _name )
sem_Expression (Expression_Do _range _statements )  =
    (sem_Expression_Do _range (sem_Statements _statements ) )
sem_Expression (Expression_Enum _range _from _then _to )  =
    (sem_Expression_Enum _range (sem_Expression _from ) (sem_MaybeExpression _then ) (sem_MaybeExpression _to ) )
sem_Expression (Expression_If _range _guardExpression _thenExpression _elseExpression )  =
    (sem_Expression_If _range (sem_Expression _guardExpression ) (sem_Expression _thenExpression ) (sem_Expression _elseExpression ) )
sem_Expression (Expression_ImplicitApplication _range _function _arguments )  =
    (sem_Expression_ImplicitApplication _range (sem_Expression _function ) (sem_ContextedExpressions _arguments ) )
sem_Expression (Expression_ImplicitLambda _range _patterns _expression )  =
    (sem_Expression_ImplicitLambda _range (sem_ContextedPatterns _patterns ) (sem_Expression _expression ) )
sem_Expression (Expression_ImpredicativeApplication _range _function _arguments )  =
    (sem_Expression_ImpredicativeApplication _range (sem_Expression _function ) (sem_Expressions _arguments ) )
sem_Expression (Expression_InfixApplication _range _leftExpression _operator _rightExpression )  =
    (sem_Expression_InfixApplication _range (sem_Expression _leftExpression ) (sem_Expression _operator ) (sem_Expression _rightExpression ) )
sem_Expression (Expression_InfixApplicationChainTop _range _expression )  =
    (sem_Expression_InfixApplicationChainTop _range (sem_Expression _expression ) )
sem_Expression (Expression_Lambda _range _patterns _expression )  =
    (sem_Expression_Lambda _range (sem_Patterns _patterns ) (sem_Expression _expression ) )
sem_Expression (Expression_Let _range _isStrict _declarations _expression )  =
    (sem_Expression_Let _range _isStrict (sem_Declarations _declarations ) (sem_Expression _expression ) )
sem_Expression (Expression_List _range _expressions )  =
    (sem_Expression_List _range (sem_Expressions _expressions ) )
sem_Expression (Expression_Literal _range _literal )  =
    (sem_Expression_Literal _range (sem_Literal _literal ) )
sem_Expression (Expression_Negate _range _expression )  =
    (sem_Expression_Negate _range (sem_Expression _expression ) )
sem_Expression (Expression_NormalApplication _range _function _arguments )  =
    (sem_Expression_NormalApplication _range (sem_Expression _function ) (sem_Expressions _arguments ) )
sem_Expression (Expression_Parenthesized _range _expression )  =
    (sem_Expression_Parenthesized _range (sem_Expression _expression ) )
sem_Expression (Expression_RecordConstruction _range _name _recordExpressionBindings )  =
    (sem_Expression_RecordConstruction _range _name (sem_RecordExpressionBindings _recordExpressionBindings ) )
sem_Expression (Expression_RecordUpdate _range _expression _recordExpressionBindings )  =
    (sem_Expression_RecordUpdate _range (sem_Expression _expression ) (sem_RecordExpressionBindings _recordExpressionBindings ) )
sem_Expression (Expression_RowRecordEmpty _range )  =
    (sem_Expression_RowRecordEmpty _range )
sem_Expression (Expression_RowRecordSelect _range _expression _name )  =
    (sem_Expression_RowRecordSelect _range (sem_Expression _expression ) _name )
sem_Expression (Expression_RowRecordUpdate _range _expression _rowRecordExpressionUpdates )  =
    (sem_Expression_RowRecordUpdate _range (sem_Expression _expression ) (sem_RowRecordExpressionUpdates _rowRecordExpressionUpdates ) )
sem_Expression (Expression_SectionApplication _range _leftExpression _operator _rightExpression )  =
    (sem_Expression_SectionApplication _range (sem_MaybeExpression _leftExpression ) (sem_Expression _operator ) (sem_MaybeExpression _rightExpression ) )
sem_Expression (Expression_Tuple _range _expressions )  =
    (sem_Expression_Tuple _range (sem_Expressions _expressions ) )
sem_Expression (Expression_TupleConstructor _range _arity )  =
    (sem_Expression_TupleConstructor _range _arity )
sem_Expression (Expression_Typed _range _expression _type )  =
    (sem_Expression_Typed _range (sem_Expression _expression ) (sem_Type _type ) )
sem_Expression (Expression_Variable _range _name )  =
    (sem_Expression_Variable _range _name )
-- semantic domain
type T_Expression  = UID ->
                     HsName ->
                     NmLev ->
                     EHCOpts ->
                     ([HsName]) ->
                     ( Name,UID,([HsName]))
sem_Expression_Annotate :: Range ->
                           T_ExpressionAnnotation  ->
                           T_Expression  ->
                           T_Expression 
sem_Expression_Annotate range_ annotation_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _annotationOgUniq :: UID
              _annotationOmoduleNm :: HsName
              _annotationOnmLev :: NmLev
              _annotationOopts :: EHCOpts
              _annotationOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _annotationIgUniq :: UID
              _annotationItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _annotationOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _annotationOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _annotationOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _annotationOopts =
                  _lhsIopts
              -- copy rule (down)
              _annotationOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _annotationIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _annotationItopInstanceNmL
              ( _annotationIgUniq,_annotationItopInstanceNmL) | True =
                  annotation_ _annotationOgUniq _annotationOmoduleNm _annotationOnmLev _annotationOopts _annotationOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Case :: Range ->
                       T_Expression  ->
                       T_Alternatives  ->
                       T_Expression 
sem_Expression_Case range_ expression_ alternatives_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _alternativesOgUniq :: UID
              _alternativesOmoduleNm :: HsName
              _alternativesOnmLev :: NmLev
              _alternativesOopts :: EHCOpts
              _alternativesOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _alternativesIgUniq :: UID
              _alternativesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _alternativesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _alternativesItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _alternativesOgUniq =
                  _expressionIgUniq
              -- copy rule (down)
              _alternativesOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _alternativesOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _alternativesOopts =
                  _lhsIopts
              -- copy rule (chain)
              _alternativesOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _alternativesIgUniq,_alternativesItopInstanceNmL) | True =
                  alternatives_ _alternativesOgUniq _alternativesOmoduleNm _alternativesOnmLev _alternativesOopts _alternativesOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Comprehension :: Range ->
                                T_Expression  ->
                                T_Qualifiers  ->
                                T_Expression 
sem_Expression_Comprehension range_ expression_ qualifiers_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _qualifiersOgUniq :: UID
              _qualifiersOmoduleNm :: HsName
              _qualifiersOnmLev :: NmLev
              _qualifiersOopts :: EHCOpts
              _qualifiersOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _qualifiersIgUniq :: UID
              _qualifiersItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _qualifiersIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _qualifiersItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _qualifiersOgUniq =
                  _expressionIgUniq
              -- copy rule (down)
              _qualifiersOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _qualifiersOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _qualifiersOopts =
                  _lhsIopts
              -- copy rule (chain)
              _qualifiersOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _qualifiersIgUniq,_qualifiersItopInstanceNmL) | True =
                  qualifiers_ _qualifiersOgUniq _qualifiersOmoduleNm _qualifiersOnmLev _qualifiersOopts _qualifiersOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Constructor :: Range ->
                              Name ->
                              T_Expression 
sem_Expression_Constructor range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 264, column 9)
              _lhsOconNm =
                  name_
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Do :: Range ->
                     T_Statements  ->
                     T_Expression 
sem_Expression_Do range_ statements_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _statementsOgUniq :: UID
              _statementsOmoduleNm :: HsName
              _statementsOnmLev :: NmLev
              _statementsOopts :: EHCOpts
              _statementsOtopInstanceNmL :: ([HsName])
              _statementsIgUniq :: UID
              _statementsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _statementsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _statementsItopInstanceNmL
              -- copy rule (down)
              _statementsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _statementsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _statementsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _statementsOopts =
                  _lhsIopts
              -- copy rule (down)
              _statementsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _statementsIgUniq,_statementsItopInstanceNmL) | True =
                  statements_ _statementsOgUniq _statementsOmoduleNm _statementsOnmLev _statementsOopts _statementsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Enum :: Range ->
                       T_Expression  ->
                       T_MaybeExpression  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Enum range_ from_ then_ to_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _fromOgUniq :: UID
              _fromOmoduleNm :: HsName
              _fromOnmLev :: NmLev
              _fromOopts :: EHCOpts
              _fromOtopInstanceNmL :: ([HsName])
              _thenOgUniq :: UID
              _thenOmoduleNm :: HsName
              _thenOnmLev :: NmLev
              _thenOopts :: EHCOpts
              _thenOtopInstanceNmL :: ([HsName])
              _toOgUniq :: UID
              _toOmoduleNm :: HsName
              _toOnmLev :: NmLev
              _toOopts :: EHCOpts
              _toOtopInstanceNmL :: ([HsName])
              _fromIconNm :: Name
              _fromIgUniq :: UID
              _fromItopInstanceNmL :: ([HsName])
              _thenIgUniq :: UID
              _thenItopInstanceNmL :: ([HsName])
              _toIgUniq :: UID
              _toItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _toIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _toItopInstanceNmL
              -- copy rule (down)
              _fromOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _fromOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _fromOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _fromOopts =
                  _lhsIopts
              -- copy rule (down)
              _fromOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _thenOgUniq =
                  _fromIgUniq
              -- copy rule (down)
              _thenOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _thenOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _thenOopts =
                  _lhsIopts
              -- copy rule (chain)
              _thenOtopInstanceNmL =
                  _fromItopInstanceNmL
              -- copy rule (chain)
              _toOgUniq =
                  _thenIgUniq
              -- copy rule (down)
              _toOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _toOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _toOopts =
                  _lhsIopts
              -- copy rule (chain)
              _toOtopInstanceNmL =
                  _thenItopInstanceNmL
              ( _fromIconNm,_fromIgUniq,_fromItopInstanceNmL) | True =
                  from_ _fromOgUniq _fromOmoduleNm _fromOnmLev _fromOopts _fromOtopInstanceNmL 
              ( _thenIgUniq,_thenItopInstanceNmL) | True =
                  then_ _thenOgUniq _thenOmoduleNm _thenOnmLev _thenOopts _thenOtopInstanceNmL 
              ( _toIgUniq,_toItopInstanceNmL) | True =
                  to_ _toOgUniq _toOmoduleNm _toOnmLev _toOopts _toOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_If :: Range ->
                     T_Expression  ->
                     T_Expression  ->
                     T_Expression  ->
                     T_Expression 
sem_Expression_If range_ guardExpression_ thenExpression_ elseExpression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _guardExpressionOgUniq :: UID
              _guardExpressionOmoduleNm :: HsName
              _guardExpressionOnmLev :: NmLev
              _guardExpressionOopts :: EHCOpts
              _guardExpressionOtopInstanceNmL :: ([HsName])
              _thenExpressionOgUniq :: UID
              _thenExpressionOmoduleNm :: HsName
              _thenExpressionOnmLev :: NmLev
              _thenExpressionOopts :: EHCOpts
              _thenExpressionOtopInstanceNmL :: ([HsName])
              _elseExpressionOgUniq :: UID
              _elseExpressionOmoduleNm :: HsName
              _elseExpressionOnmLev :: NmLev
              _elseExpressionOopts :: EHCOpts
              _elseExpressionOtopInstanceNmL :: ([HsName])
              _guardExpressionIconNm :: Name
              _guardExpressionIgUniq :: UID
              _guardExpressionItopInstanceNmL :: ([HsName])
              _thenExpressionIconNm :: Name
              _thenExpressionIgUniq :: UID
              _thenExpressionItopInstanceNmL :: ([HsName])
              _elseExpressionIconNm :: Name
              _elseExpressionIgUniq :: UID
              _elseExpressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _elseExpressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _elseExpressionItopInstanceNmL
              -- copy rule (down)
              _guardExpressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _guardExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _guardExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _guardExpressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _guardExpressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _thenExpressionOgUniq =
                  _guardExpressionIgUniq
              -- copy rule (down)
              _thenExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _thenExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _thenExpressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _thenExpressionOtopInstanceNmL =
                  _guardExpressionItopInstanceNmL
              -- copy rule (chain)
              _elseExpressionOgUniq =
                  _thenExpressionIgUniq
              -- copy rule (down)
              _elseExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _elseExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _elseExpressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _elseExpressionOtopInstanceNmL =
                  _thenExpressionItopInstanceNmL
              ( _guardExpressionIconNm,_guardExpressionIgUniq,_guardExpressionItopInstanceNmL) | True =
                  guardExpression_ _guardExpressionOgUniq _guardExpressionOmoduleNm _guardExpressionOnmLev _guardExpressionOopts _guardExpressionOtopInstanceNmL 
              ( _thenExpressionIconNm,_thenExpressionIgUniq,_thenExpressionItopInstanceNmL) | True =
                  thenExpression_ _thenExpressionOgUniq _thenExpressionOmoduleNm _thenExpressionOnmLev _thenExpressionOopts _thenExpressionOtopInstanceNmL 
              ( _elseExpressionIconNm,_elseExpressionIgUniq,_elseExpressionItopInstanceNmL) | True =
                  elseExpression_ _elseExpressionOgUniq _elseExpressionOmoduleNm _elseExpressionOnmLev _elseExpressionOopts _elseExpressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_ImplicitApplication :: Range ->
                                      T_Expression  ->
                                      T_ContextedExpressions  ->
                                      T_Expression 
sem_Expression_ImplicitApplication range_ function_ arguments_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _functionOgUniq :: UID
              _functionOmoduleNm :: HsName
              _functionOnmLev :: NmLev
              _functionOopts :: EHCOpts
              _functionOtopInstanceNmL :: ([HsName])
              _argumentsOgUniq :: UID
              _argumentsOmoduleNm :: HsName
              _argumentsOnmLev :: NmLev
              _argumentsOopts :: EHCOpts
              _argumentsOtopInstanceNmL :: ([HsName])
              _functionIconNm :: Name
              _functionIgUniq :: UID
              _functionItopInstanceNmL :: ([HsName])
              _argumentsIgUniq :: UID
              _argumentsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _argumentsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _argumentsItopInstanceNmL
              -- copy rule (down)
              _functionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _functionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _functionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _functionOopts =
                  _lhsIopts
              -- copy rule (down)
              _functionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _argumentsOgUniq =
                  _functionIgUniq
              -- copy rule (down)
              _argumentsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _argumentsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _argumentsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _argumentsOtopInstanceNmL =
                  _functionItopInstanceNmL
              ( _functionIconNm,_functionIgUniq,_functionItopInstanceNmL) | True =
                  function_ _functionOgUniq _functionOmoduleNm _functionOnmLev _functionOopts _functionOtopInstanceNmL 
              ( _argumentsIgUniq,_argumentsItopInstanceNmL) | True =
                  arguments_ _argumentsOgUniq _argumentsOmoduleNm _argumentsOnmLev _argumentsOopts _argumentsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_ImplicitLambda :: Range ->
                                 T_ContextedPatterns  ->
                                 T_Expression  ->
                                 T_Expression 
sem_Expression_ImplicitLambda range_ patterns_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _patternsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _patternsItopInstanceNmL
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_ImpredicativeApplication :: Range ->
                                           T_Expression  ->
                                           T_Expressions  ->
                                           T_Expression 
sem_Expression_ImpredicativeApplication range_ function_ arguments_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _functionOgUniq :: UID
              _functionOmoduleNm :: HsName
              _functionOnmLev :: NmLev
              _functionOopts :: EHCOpts
              _functionOtopInstanceNmL :: ([HsName])
              _argumentsOgUniq :: UID
              _argumentsOmoduleNm :: HsName
              _argumentsOnmLev :: NmLev
              _argumentsOopts :: EHCOpts
              _argumentsOtopInstanceNmL :: ([HsName])
              _functionIconNm :: Name
              _functionIgUniq :: UID
              _functionItopInstanceNmL :: ([HsName])
              _argumentsIgUniq :: UID
              _argumentsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _argumentsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _argumentsItopInstanceNmL
              -- copy rule (down)
              _functionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _functionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _functionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _functionOopts =
                  _lhsIopts
              -- copy rule (down)
              _functionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _argumentsOgUniq =
                  _functionIgUniq
              -- copy rule (down)
              _argumentsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _argumentsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _argumentsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _argumentsOtopInstanceNmL =
                  _functionItopInstanceNmL
              ( _functionIconNm,_functionIgUniq,_functionItopInstanceNmL) | True =
                  function_ _functionOgUniq _functionOmoduleNm _functionOnmLev _functionOopts _functionOtopInstanceNmL 
              ( _argumentsIgUniq,_argumentsItopInstanceNmL) | True =
                  arguments_ _argumentsOgUniq _argumentsOmoduleNm _argumentsOnmLev _argumentsOopts _argumentsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_InfixApplication :: Range ->
                                   T_Expression  ->
                                   T_Expression  ->
                                   T_Expression  ->
                                   T_Expression 
sem_Expression_InfixApplication range_ leftExpression_ operator_ rightExpression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftExpressionOgUniq :: UID
              _leftExpressionOmoduleNm :: HsName
              _leftExpressionOnmLev :: NmLev
              _leftExpressionOopts :: EHCOpts
              _leftExpressionOtopInstanceNmL :: ([HsName])
              _operatorOgUniq :: UID
              _operatorOmoduleNm :: HsName
              _operatorOnmLev :: NmLev
              _operatorOopts :: EHCOpts
              _operatorOtopInstanceNmL :: ([HsName])
              _rightExpressionOgUniq :: UID
              _rightExpressionOmoduleNm :: HsName
              _rightExpressionOnmLev :: NmLev
              _rightExpressionOopts :: EHCOpts
              _rightExpressionOtopInstanceNmL :: ([HsName])
              _leftExpressionIconNm :: Name
              _leftExpressionIgUniq :: UID
              _leftExpressionItopInstanceNmL :: ([HsName])
              _operatorIconNm :: Name
              _operatorIgUniq :: UID
              _operatorItopInstanceNmL :: ([HsName])
              _rightExpressionIconNm :: Name
              _rightExpressionIgUniq :: UID
              _rightExpressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rightExpressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightExpressionItopInstanceNmL
              -- copy rule (down)
              _leftExpressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _leftExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftExpressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _leftExpressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _operatorOgUniq =
                  _leftExpressionIgUniq
              -- copy rule (down)
              _operatorOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _operatorOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _operatorOopts =
                  _lhsIopts
              -- copy rule (chain)
              _operatorOtopInstanceNmL =
                  _leftExpressionItopInstanceNmL
              -- copy rule (chain)
              _rightExpressionOgUniq =
                  _operatorIgUniq
              -- copy rule (down)
              _rightExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rightExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _rightExpressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rightExpressionOtopInstanceNmL =
                  _operatorItopInstanceNmL
              ( _leftExpressionIconNm,_leftExpressionIgUniq,_leftExpressionItopInstanceNmL) | True =
                  leftExpression_ _leftExpressionOgUniq _leftExpressionOmoduleNm _leftExpressionOnmLev _leftExpressionOopts _leftExpressionOtopInstanceNmL 
              ( _operatorIconNm,_operatorIgUniq,_operatorItopInstanceNmL) | True =
                  operator_ _operatorOgUniq _operatorOmoduleNm _operatorOnmLev _operatorOopts _operatorOtopInstanceNmL 
              ( _rightExpressionIconNm,_rightExpressionIgUniq,_rightExpressionItopInstanceNmL) | True =
                  rightExpression_ _rightExpressionOgUniq _rightExpressionOmoduleNm _rightExpressionOnmLev _rightExpressionOopts _rightExpressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_InfixApplicationChainTop :: Range ->
                                           T_Expression  ->
                                           T_Expression 
sem_Expression_InfixApplicationChainTop range_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Lambda :: Range ->
                         T_Patterns  ->
                         T_Expression  ->
                         T_Expression 
sem_Expression_Lambda range_ patterns_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsImainValExists :: Bool
              _patternsItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 50, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _patternsOnmLev =
                  _nmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _patternsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _expressionOnmLev =
                  _nmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _patternsItopInstanceNmL
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsImainValExists,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Let :: Range ->
                      Bool ->
                      T_Declarations  ->
                      T_Expression  ->
                      T_Expression 
sem_Expression_Let range_ isStrict_ declarations_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _declarationsOgUniq :: UID
              _declarationsOmoduleNm :: HsName
              _declarationsOnmLev :: NmLev
              _declarationsOopts :: EHCOpts
              _declarationsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _declarationsIgUniq :: UID
              _declarationsIgathPragmas :: (Set.Set Pragma.Pragma)
              _declarationsIidOccDefs :: ([IdOcc])
              _declarationsImainValExists :: Bool
              _declarationsImodDefsRel :: ModEntRel
              _declarationsImodHideDefsRel :: ModEntRel
              _declarationsItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 50, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _declarationsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _declarationsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _declarationsOnmLev =
                  _nmLev
              -- copy rule (down)
              _declarationsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declarationsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _declarationsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _expressionOnmLev =
                  _nmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _declarationsItopInstanceNmL
              ( _declarationsIgUniq,_declarationsIgathPragmas,_declarationsIidOccDefs,_declarationsImainValExists,_declarationsImodDefsRel,_declarationsImodHideDefsRel,_declarationsItopInstanceNmL) | True =
                  declarations_ _declarationsOgUniq _declarationsOmoduleNm _declarationsOnmLev _declarationsOopts _declarationsOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_List :: Range ->
                       T_Expressions  ->
                       T_Expression 
sem_Expression_List range_ expressions_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionsOgUniq :: UID
              _expressionsOmoduleNm :: HsName
              _expressionsOnmLev :: NmLev
              _expressionsOopts :: EHCOpts
              _expressionsOtopInstanceNmL :: ([HsName])
              _expressionsIgUniq :: UID
              _expressionsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionsItopInstanceNmL
              -- copy rule (down)
              _expressionsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionsOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionsIgUniq,_expressionsItopInstanceNmL) | True =
                  expressions_ _expressionsOgUniq _expressionsOmoduleNm _expressionsOnmLev _expressionsOopts _expressionsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Literal :: Range ->
                          T_Literal  ->
                          T_Expression 
sem_Expression_Literal range_ literal_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _literalOgUniq :: UID
              _literalOnmLev :: NmLev
              _literalOtopInstanceNmL :: ([HsName])
              _literalIgUniq :: UID
              _literalItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _literalIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _literalItopInstanceNmL
              -- copy rule (down)
              _literalOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _literalOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _literalOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _literalIgUniq,_literalItopInstanceNmL) | True =
                  literal_ _literalOgUniq _literalOnmLev _literalOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Negate :: Range ->
                         T_Expression  ->
                         T_Expression 
sem_Expression_Negate range_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_NormalApplication :: Range ->
                                    T_Expression  ->
                                    T_Expressions  ->
                                    T_Expression 
sem_Expression_NormalApplication range_ function_ arguments_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _functionOgUniq :: UID
              _functionOmoduleNm :: HsName
              _functionOnmLev :: NmLev
              _functionOopts :: EHCOpts
              _functionOtopInstanceNmL :: ([HsName])
              _argumentsOgUniq :: UID
              _argumentsOmoduleNm :: HsName
              _argumentsOnmLev :: NmLev
              _argumentsOopts :: EHCOpts
              _argumentsOtopInstanceNmL :: ([HsName])
              _functionIconNm :: Name
              _functionIgUniq :: UID
              _functionItopInstanceNmL :: ([HsName])
              _argumentsIgUniq :: UID
              _argumentsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _argumentsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _argumentsItopInstanceNmL
              -- copy rule (down)
              _functionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _functionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _functionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _functionOopts =
                  _lhsIopts
              -- copy rule (down)
              _functionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _argumentsOgUniq =
                  _functionIgUniq
              -- copy rule (down)
              _argumentsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _argumentsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _argumentsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _argumentsOtopInstanceNmL =
                  _functionItopInstanceNmL
              ( _functionIconNm,_functionIgUniq,_functionItopInstanceNmL) | True =
                  function_ _functionOgUniq _functionOmoduleNm _functionOnmLev _functionOopts _functionOtopInstanceNmL 
              ( _argumentsIgUniq,_argumentsItopInstanceNmL) | True =
                  arguments_ _argumentsOgUniq _argumentsOmoduleNm _argumentsOnmLev _argumentsOopts _argumentsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Parenthesized :: Range ->
                                T_Expression  ->
                                T_Expression 
sem_Expression_Parenthesized range_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_RecordConstruction :: Range ->
                                     Name ->
                                     T_RecordExpressionBindings  ->
                                     T_Expression 
sem_Expression_RecordConstruction range_ name_ recordExpressionBindings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _recordExpressionBindingsOgUniq :: UID
              _recordExpressionBindingsOmoduleNm :: HsName
              _recordExpressionBindingsOnmLev :: NmLev
              _recordExpressionBindingsOopts :: EHCOpts
              _recordExpressionBindingsOtopInstanceNmL :: ([HsName])
              _recordExpressionBindingsIgUniq :: UID
              _recordExpressionBindingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _recordExpressionBindingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _recordExpressionBindingsItopInstanceNmL
              -- copy rule (down)
              _recordExpressionBindingsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _recordExpressionBindingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _recordExpressionBindingsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _recordExpressionBindingsOopts =
                  _lhsIopts
              -- copy rule (down)
              _recordExpressionBindingsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _recordExpressionBindingsIgUniq,_recordExpressionBindingsItopInstanceNmL) | True =
                  recordExpressionBindings_ _recordExpressionBindingsOgUniq _recordExpressionBindingsOmoduleNm _recordExpressionBindingsOnmLev _recordExpressionBindingsOopts _recordExpressionBindingsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_RecordUpdate :: Range ->
                               T_Expression  ->
                               T_RecordExpressionBindings  ->
                               T_Expression 
sem_Expression_RecordUpdate range_ expression_ recordExpressionBindings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _recordExpressionBindingsOgUniq :: UID
              _recordExpressionBindingsOmoduleNm :: HsName
              _recordExpressionBindingsOnmLev :: NmLev
              _recordExpressionBindingsOopts :: EHCOpts
              _recordExpressionBindingsOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _recordExpressionBindingsIgUniq :: UID
              _recordExpressionBindingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _recordExpressionBindingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _recordExpressionBindingsItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _recordExpressionBindingsOgUniq =
                  _expressionIgUniq
              -- copy rule (down)
              _recordExpressionBindingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _recordExpressionBindingsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _recordExpressionBindingsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _recordExpressionBindingsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _recordExpressionBindingsIgUniq,_recordExpressionBindingsItopInstanceNmL) | True =
                  recordExpressionBindings_ _recordExpressionBindingsOgUniq _recordExpressionBindingsOmoduleNm _recordExpressionBindingsOnmLev _recordExpressionBindingsOopts _recordExpressionBindingsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_RowRecordEmpty :: Range ->
                                 T_Expression 
sem_Expression_RowRecordEmpty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_RowRecordSelect :: Range ->
                                  T_Expression  ->
                                  Name ->
                                  T_Expression 
sem_Expression_RowRecordSelect range_ expression_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_RowRecordUpdate :: Range ->
                                  T_Expression  ->
                                  T_RowRecordExpressionUpdates  ->
                                  T_Expression 
sem_Expression_RowRecordUpdate range_ expression_ rowRecordExpressionUpdates_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _rowRecordExpressionUpdatesOgUniq :: UID
              _rowRecordExpressionUpdatesOmoduleNm :: HsName
              _rowRecordExpressionUpdatesOnmLev :: NmLev
              _rowRecordExpressionUpdatesOopts :: EHCOpts
              _rowRecordExpressionUpdatesOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _rowRecordExpressionUpdatesIgUniq :: UID
              _rowRecordExpressionUpdatesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rowRecordExpressionUpdatesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rowRecordExpressionUpdatesItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rowRecordExpressionUpdatesOgUniq =
                  _expressionIgUniq
              -- copy rule (down)
              _rowRecordExpressionUpdatesOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rowRecordExpressionUpdatesOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _rowRecordExpressionUpdatesOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rowRecordExpressionUpdatesOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _rowRecordExpressionUpdatesIgUniq,_rowRecordExpressionUpdatesItopInstanceNmL) | True =
                  rowRecordExpressionUpdates_ _rowRecordExpressionUpdatesOgUniq _rowRecordExpressionUpdatesOmoduleNm _rowRecordExpressionUpdatesOnmLev _rowRecordExpressionUpdatesOopts _rowRecordExpressionUpdatesOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_SectionApplication :: Range ->
                                     T_MaybeExpression  ->
                                     T_Expression  ->
                                     T_MaybeExpression  ->
                                     T_Expression 
sem_Expression_SectionApplication range_ leftExpression_ operator_ rightExpression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftExpressionOgUniq :: UID
              _leftExpressionOmoduleNm :: HsName
              _leftExpressionOnmLev :: NmLev
              _leftExpressionOopts :: EHCOpts
              _leftExpressionOtopInstanceNmL :: ([HsName])
              _operatorOgUniq :: UID
              _operatorOmoduleNm :: HsName
              _operatorOnmLev :: NmLev
              _operatorOopts :: EHCOpts
              _operatorOtopInstanceNmL :: ([HsName])
              _rightExpressionOgUniq :: UID
              _rightExpressionOmoduleNm :: HsName
              _rightExpressionOnmLev :: NmLev
              _rightExpressionOopts :: EHCOpts
              _rightExpressionOtopInstanceNmL :: ([HsName])
              _leftExpressionIgUniq :: UID
              _leftExpressionItopInstanceNmL :: ([HsName])
              _operatorIconNm :: Name
              _operatorIgUniq :: UID
              _operatorItopInstanceNmL :: ([HsName])
              _rightExpressionIgUniq :: UID
              _rightExpressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rightExpressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightExpressionItopInstanceNmL
              -- copy rule (down)
              _leftExpressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _leftExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftExpressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _leftExpressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _operatorOgUniq =
                  _leftExpressionIgUniq
              -- copy rule (down)
              _operatorOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _operatorOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _operatorOopts =
                  _lhsIopts
              -- copy rule (chain)
              _operatorOtopInstanceNmL =
                  _leftExpressionItopInstanceNmL
              -- copy rule (chain)
              _rightExpressionOgUniq =
                  _operatorIgUniq
              -- copy rule (down)
              _rightExpressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rightExpressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _rightExpressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _rightExpressionOtopInstanceNmL =
                  _operatorItopInstanceNmL
              ( _leftExpressionIgUniq,_leftExpressionItopInstanceNmL) | True =
                  leftExpression_ _leftExpressionOgUniq _leftExpressionOmoduleNm _leftExpressionOnmLev _leftExpressionOopts _leftExpressionOtopInstanceNmL 
              ( _operatorIconNm,_operatorIgUniq,_operatorItopInstanceNmL) | True =
                  operator_ _operatorOgUniq _operatorOmoduleNm _operatorOnmLev _operatorOopts _operatorOtopInstanceNmL 
              ( _rightExpressionIgUniq,_rightExpressionItopInstanceNmL) | True =
                  rightExpression_ _rightExpressionOgUniq _rightExpressionOmoduleNm _rightExpressionOnmLev _rightExpressionOopts _rightExpressionOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Tuple :: Range ->
                        T_Expressions  ->
                        T_Expression 
sem_Expression_Tuple range_ expressions_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionsOgUniq :: UID
              _expressionsOmoduleNm :: HsName
              _expressionsOnmLev :: NmLev
              _expressionsOopts :: EHCOpts
              _expressionsOtopInstanceNmL :: ([HsName])
              _expressionsIgUniq :: UID
              _expressionsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionsItopInstanceNmL
              -- copy rule (down)
              _expressionsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionsOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionsIgUniq,_expressionsItopInstanceNmL) | True =
                  expressions_ _expressionsOgUniq _expressionsOmoduleNm _expressionsOnmLev _expressionsOopts _expressionsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_TupleConstructor :: Range ->
                                   Int ->
                                   T_Expression 
sem_Expression_TupleConstructor range_ arity_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Typed :: Range ->
                        T_Expression  ->
                        T_Type  ->
                        T_Expression 
sem_Expression_Typed range_ expression_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _typeOnmLev :: NmLev
              _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 52, column 9)
              _typeOnmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _expressionIgUniq
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expression_Variable :: Range ->
                           Name ->
                           T_Expression 
sem_Expression_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 266, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
-- ExpressionAnnotation ----------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Empty:
-}
-- cata
sem_ExpressionAnnotation :: ExpressionAnnotation  ->
                            T_ExpressionAnnotation 
sem_ExpressionAnnotation (ExpressionAnnotation_Empty )  =
    (sem_ExpressionAnnotation_Empty )
-- semantic domain
type T_ExpressionAnnotation  = UID ->
                               HsName ->
                               NmLev ->
                               EHCOpts ->
                               ([HsName]) ->
                               ( UID,([HsName]))
sem_ExpressionAnnotation_Empty :: T_ExpressionAnnotation 
sem_ExpressionAnnotation_Empty  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Expressions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : Expression 
         child tl             : Expressions 
      alternative Nil:
-}
-- cata
sem_Expressions :: Expressions  ->
                   T_Expressions 
sem_Expressions list  =
    (Prelude.foldr sem_Expressions_Cons sem_Expressions_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_Expressions  = UID ->
                      HsName ->
                      NmLev ->
                      EHCOpts ->
                      ([HsName]) ->
                      ( UID,([HsName]))
sem_Expressions_Cons :: T_Expression  ->
                        T_Expressions  ->
                        T_Expressions 
sem_Expressions_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIconNm :: Name
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIconNm,_hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Expressions_Nil :: T_Expressions 
sem_Expressions_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- FieldDeclaration --------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         fldrefnames          : Names
   alternatives:
      alternative FieldDeclaration:
         child range          : {Range}
         child names          : {Names}
         child type           : Type 
         visit 0:
            local nmLev       : _
            local fldrefnames : _
-}
-- cata
sem_FieldDeclaration :: FieldDeclaration  ->
                        T_FieldDeclaration 
sem_FieldDeclaration (FieldDeclaration_FieldDeclaration _range _names _type )  =
    (sem_FieldDeclaration_FieldDeclaration _range _names (sem_Type _type ) )
-- semantic domain
type T_FieldDeclaration  = UID ->
                           HsName ->
                           NmLev ->
                           ([HsName]) ->
                           ( Names,UID,([HsName]))
sem_FieldDeclaration_FieldDeclaration :: Range ->
                                         Names ->
                                         T_Type  ->
                                         T_FieldDeclaration 
sem_FieldDeclaration_FieldDeclaration range_ names_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOfldrefnames :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 42, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 83, column 9)
              _fldrefnames =
                  map (hsnSetLevQual _lhsInmLev _lhsImoduleNm) names_
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 78, column 44)
              _lhsOfldrefnames =
                  _fldrefnames
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOfldrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
-- FieldDeclarations -------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         fldrefnames          : Names
   alternatives:
      alternative Cons:
         child hd             : FieldDeclaration 
         child tl             : FieldDeclarations 
      alternative Nil:
-}
-- cata
sem_FieldDeclarations :: FieldDeclarations  ->
                         T_FieldDeclarations 
sem_FieldDeclarations list  =
    (Prelude.foldr sem_FieldDeclarations_Cons sem_FieldDeclarations_Nil (Prelude.map sem_FieldDeclaration list) )
-- semantic domain
type T_FieldDeclarations  = UID ->
                            HsName ->
                            NmLev ->
                            ([HsName]) ->
                            ( Names,UID,([HsName]))
sem_FieldDeclarations_Cons :: T_FieldDeclaration  ->
                              T_FieldDeclarations  ->
                              T_FieldDeclarations 
sem_FieldDeclarations_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOfldrefnames :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIfldrefnames :: Names
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIfldrefnames :: Names
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 78, column 44)
              _lhsOfldrefnames =
                  _hdIfldrefnames ++ _tlIfldrefnames
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIfldrefnames,_hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIfldrefnames,_tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOfldrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_FieldDeclarations_Nil :: T_FieldDeclarations 
sem_FieldDeclarations_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOfldrefnames :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 78, column 44)
              _lhsOfldrefnames =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOfldrefnames,_lhsOgUniq,_lhsOtopInstanceNmL)))
-- FunctionBinding ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
   alternatives:
      alternative FunctionBinding:
         child range          : {Range}
         child lefthandside   : LeftHandSide 
         child righthandside  : RightHandSide 
         visit 0:
            local nmLev       : _
            local nmLevFun    : _
            local idOccDef    : _
            local idOccDefs   : _
-}
-- cata
sem_FunctionBinding :: FunctionBinding  ->
                       T_FunctionBinding 
sem_FunctionBinding (FunctionBinding_FunctionBinding _range _lefthandside _righthandside )  =
    (sem_FunctionBinding_FunctionBinding _range (sem_LeftHandSide _lefthandside ) (sem_RightHandSide _righthandside ) )
-- semantic domain
type T_FunctionBinding  = UID ->
                          HsName ->
                          NmLev ->
                          EHCOpts ->
                          ([HsName]) ->
                          ( UID,([IdOcc]),Bool,([HsName]))
sem_FunctionBinding_FunctionBinding :: Range ->
                                       T_LeftHandSide  ->
                                       T_RightHandSide  ->
                                       T_FunctionBinding 
sem_FunctionBinding_FunctionBinding range_ lefthandside_ righthandside_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _lefthandsideOgUniq :: UID
              _lefthandsideOmoduleNm :: HsName
              _lefthandsideOnmLev :: NmLev
              _lefthandsideOnmLevFun :: NmLev
              _lefthandsideOopts :: EHCOpts
              _lefthandsideOtopInstanceNmL :: ([HsName])
              _righthandsideOgUniq :: UID
              _righthandsideOmoduleNm :: HsName
              _righthandsideOnmLev :: NmLev
              _righthandsideOopts :: EHCOpts
              _righthandsideOtopInstanceNmL :: ([HsName])
              _lefthandsideIgUniq :: UID
              _lefthandsideIidOccDefs :: ([IdOcc])
              _lefthandsideImainValExists :: Bool
              _lefthandsideIrefname :: HsName
              _lefthandsideItopInstanceNmL :: ([HsName])
              _righthandsideIgUniq :: UID
              _righthandsideItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 64, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 64, column 9)
              _nmLevFun =
                  _lhsInmLev
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 169, column 9)
              _idOccDef =
                  IdOcc _lefthandsideIrefname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 169, column 9)
              _idOccDefs =
                  [_idOccDef]
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _lefthandsideImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _righthandsideIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _righthandsideItopInstanceNmL
              -- copy rule (down)
              _lefthandsideOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _lefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _lefthandsideOnmLev =
                  _nmLev
              -- copy rule (from local)
              _lefthandsideOnmLevFun =
                  _nmLevFun
              -- copy rule (down)
              _lefthandsideOopts =
                  _lhsIopts
              -- copy rule (down)
              _lefthandsideOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _righthandsideOgUniq =
                  _lefthandsideIgUniq
              -- copy rule (down)
              _righthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _righthandsideOnmLev =
                  _nmLev
              -- copy rule (down)
              _righthandsideOopts =
                  _lhsIopts
              -- copy rule (chain)
              _righthandsideOtopInstanceNmL =
                  _lefthandsideItopInstanceNmL
              ( _lefthandsideIgUniq,_lefthandsideIidOccDefs,_lefthandsideImainValExists,_lefthandsideIrefname,_lefthandsideItopInstanceNmL) | True =
                  lefthandside_ _lefthandsideOgUniq _lefthandsideOmoduleNm _lefthandsideOnmLev _lefthandsideOnmLevFun _lefthandsideOopts _lefthandsideOtopInstanceNmL 
              ( _righthandsideIgUniq,_righthandsideItopInstanceNmL) | True =
                  righthandside_ _righthandsideOgUniq _righthandsideOmoduleNm _righthandsideOnmLev _righthandsideOopts _righthandsideOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
-- FunctionBindings --------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
   alternatives:
      alternative Cons:
         child hd             : FunctionBinding 
         child tl             : FunctionBindings 
      alternative Nil:
-}
-- cata
sem_FunctionBindings :: FunctionBindings  ->
                        T_FunctionBindings 
sem_FunctionBindings list  =
    (Prelude.foldr sem_FunctionBindings_Cons sem_FunctionBindings_Nil (Prelude.map sem_FunctionBinding list) )
-- semantic domain
type T_FunctionBindings  = UID ->
                           HsName ->
                           NmLev ->
                           EHCOpts ->
                           ([HsName]) ->
                           ( UID,([IdOcc]),Bool,([HsName]))
sem_FunctionBindings_Cons :: T_FunctionBinding  ->
                             T_FunctionBindings  ->
                             T_FunctionBindings 
sem_FunctionBindings_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIidOccDefs :: ([IdOcc])
              _hdImainValExists :: Bool
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIidOccDefs :: ([IdOcc])
              _tlImainValExists :: Bool
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _hdImainValExists || _tlImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIidOccDefs,_hdImainValExists,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIidOccDefs,_tlImainValExists,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
sem_FunctionBindings_Nil :: T_FunctionBindings 
sem_FunctionBindings_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
-- FunctionalDependencies --------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Cons:
         child hd             : FunctionalDependency 
         child tl             : FunctionalDependencies 
      alternative Nil:
-}
-- cata
sem_FunctionalDependencies :: FunctionalDependencies  ->
                              T_FunctionalDependencies 
sem_FunctionalDependencies list  =
    (Prelude.foldr sem_FunctionalDependencies_Cons sem_FunctionalDependencies_Nil (Prelude.map sem_FunctionalDependency list) )
-- semantic domain
type T_FunctionalDependencies  = UID ->
                                 HsName ->
                                 NmLev ->
                                 EHCOpts ->
                                 ([HsName]) ->
                                 ( UID,(Set.Set Pragma.Pragma),([IdOcc]),([HsName]))
sem_FunctionalDependencies_Cons :: T_FunctionalDependency  ->
                                   T_FunctionalDependencies  ->
                                   T_FunctionalDependencies 
sem_FunctionalDependencies_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIgathPragmas :: (Set.Set Pragma.Pragma)
              _hdIidOccDefs :: ([IdOcc])
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIgathPragmas :: (Set.Set Pragma.Pragma)
              _tlIidOccDefs :: ([IdOcc])
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _hdIgathPragmas `Set.union` _tlIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIgathPragmas,_hdIidOccDefs,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIgathPragmas,_tlIidOccDefs,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
sem_FunctionalDependencies_Nil :: T_FunctionalDependencies 
sem_FunctionalDependencies_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- FunctionalDependency ----------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Dependency:
         child range          : {Range}
         child fromtypevariables : {Names}
         child totypevariables : {Names}
-}
-- cata
sem_FunctionalDependency :: FunctionalDependency  ->
                            T_FunctionalDependency 
sem_FunctionalDependency (FunctionalDependency_Dependency _range _fromtypevariables _totypevariables )  =
    (sem_FunctionalDependency_Dependency _range _fromtypevariables _totypevariables )
-- semantic domain
type T_FunctionalDependency  = UID ->
                               HsName ->
                               NmLev ->
                               EHCOpts ->
                               ([HsName]) ->
                               ( UID,(Set.Set Pragma.Pragma),([IdOcc]),([HsName]))
sem_FunctionalDependency_Dependency :: Range ->
                                       Names ->
                                       Names ->
                                       T_FunctionalDependency 
sem_FunctionalDependency_Dependency range_ fromtypevariables_ totypevariables_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- GuardedExpression -------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative GuardedExpression:
         child range          : {Range}
         child guard          : Expression 
         child expression     : Expression 
-}
-- cata
sem_GuardedExpression :: GuardedExpression  ->
                         T_GuardedExpression 
sem_GuardedExpression (GuardedExpression_GuardedExpression _range _guard _expression )  =
    (sem_GuardedExpression_GuardedExpression _range (sem_Expression _guard ) (sem_Expression _expression ) )
-- semantic domain
type T_GuardedExpression  = UID ->
                            HsName ->
                            NmLev ->
                            EHCOpts ->
                            ([HsName]) ->
                            ( UID,([HsName]))
sem_GuardedExpression_GuardedExpression :: Range ->
                                           T_Expression  ->
                                           T_Expression  ->
                                           T_GuardedExpression 
sem_GuardedExpression_GuardedExpression range_ guard_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _guardOgUniq :: UID
              _guardOmoduleNm :: HsName
              _guardOnmLev :: NmLev
              _guardOopts :: EHCOpts
              _guardOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _guardIconNm :: Name
              _guardIgUniq :: UID
              _guardItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _guardOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _guardOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _guardOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _guardOopts =
                  _lhsIopts
              -- copy rule (down)
              _guardOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _guardIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _guardItopInstanceNmL
              ( _guardIconNm,_guardIgUniq,_guardItopInstanceNmL) | True =
                  guard_ _guardOgUniq _guardOmoduleNm _guardOnmLev _guardOopts _guardOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- GuardedExpressions ------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : GuardedExpression 
         child tl             : GuardedExpressions 
      alternative Nil:
-}
-- cata
sem_GuardedExpressions :: GuardedExpressions  ->
                          T_GuardedExpressions 
sem_GuardedExpressions list  =
    (Prelude.foldr sem_GuardedExpressions_Cons sem_GuardedExpressions_Nil (Prelude.map sem_GuardedExpression list) )
-- semantic domain
type T_GuardedExpressions  = UID ->
                             HsName ->
                             NmLev ->
                             EHCOpts ->
                             ([HsName]) ->
                             ( UID,([HsName]))
sem_GuardedExpressions_Cons :: T_GuardedExpression  ->
                               T_GuardedExpressions  ->
                               T_GuardedExpressions 
sem_GuardedExpressions_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_GuardedExpressions_Nil :: T_GuardedExpressions 
sem_GuardedExpressions_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Import ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modEntSpec           : ModEntSpec
   alternatives:
      alternative TypeOrClass:
         child range          : {Range}
         child name           : {Name}
         child names          : {MaybeNames}
      alternative TypeOrClassComplete:
         child range          : {Range}
         child name           : {Name}
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
-}
-- cata
sem_Import :: Import  ->
              T_Import 
sem_Import (Import_TypeOrClass _range _name _names )  =
    (sem_Import_TypeOrClass _range _name _names )
sem_Import (Import_TypeOrClassComplete _range _name )  =
    (sem_Import_TypeOrClassComplete _range _name )
sem_Import (Import_Variable _range _name )  =
    (sem_Import_Variable _range _name )
-- semantic domain
type T_Import  = UID ->
                 HsName ->
                 NmLev ->
                 EHCOpts ->
                 ([HsName]) ->
                 ( UID,ModEntSpec,([HsName]))
sem_Import_TypeOrClass :: Range ->
                          Name ->
                          MaybeNames ->
                          T_Import 
sem_Import_TypeOrClass range_ name_ names_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodEntSpec :: ModEntSpec
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 7, column 9)
              _lhsOmodEntSpec =
                  case names_ of
                    Just ns -> ModEntSpec name_ range_ (Just (ModEntSubs ns))
                    Nothing -> ModEntSpec name_ range_ Nothing
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodEntSpec,_lhsOtopInstanceNmL)))
sem_Import_TypeOrClassComplete :: Range ->
                                  Name ->
                                  T_Import 
sem_Import_TypeOrClassComplete range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodEntSpec :: ModEntSpec
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 11, column 9)
              _lhsOmodEntSpec =
                  ModEntSpec name_ range_ (Just ModEntSubAll)
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodEntSpec,_lhsOtopInstanceNmL)))
sem_Import_Variable :: Range ->
                       Name ->
                       T_Import 
sem_Import_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodEntSpec :: ModEntSpec
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 5, column 9)
              _lhsOmodEntSpec =
                  ModEntSpec name_ range_ Nothing
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodEntSpec,_lhsOtopInstanceNmL)))
-- ImportDeclaration -------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modImpL              : [ModImp]
   alternatives:
      alternative Empty:
         child range          : {Range}
      alternative Import:
         child range          : {Range}
         child qualified      : {Bool}
         child name           : {Name}
         child asname         : {MaybeName}
         child importspecification : MaybeImportSpecification 
-}
-- cata
sem_ImportDeclaration :: ImportDeclaration  ->
                         T_ImportDeclaration 
sem_ImportDeclaration (ImportDeclaration_Empty _range )  =
    (sem_ImportDeclaration_Empty _range )
sem_ImportDeclaration (ImportDeclaration_Import _range _qualified _name _asname _importspecification )  =
    (sem_ImportDeclaration_Import _range _qualified _name _asname (sem_MaybeImportSpecification _importspecification ) )
-- semantic domain
type T_ImportDeclaration  = UID ->
                            HsName ->
                            NmLev ->
                            EHCOpts ->
                            ([HsName]) ->
                            ( UID,([ModImp]),([HsName]))
sem_ImportDeclaration_Empty :: Range ->
                               T_ImportDeclaration 
sem_ImportDeclaration_Empty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodImpL :: ([ModImp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 42, column 9)
              _lhsOmodImpL =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodImpL,_lhsOtopInstanceNmL)))
sem_ImportDeclaration_Import :: Range ->
                                Bool ->
                                Name ->
                                MaybeName ->
                                T_MaybeImportSpecification  ->
                                T_ImportDeclaration 
sem_ImportDeclaration_Import range_ qualified_ name_ asname_ importspecification_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodImpL :: ([ModImp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _importspecificationOgUniq :: UID
              _importspecificationOmoduleNm :: HsName
              _importspecificationOnmLev :: NmLev
              _importspecificationOopts :: EHCOpts
              _importspecificationOtopInstanceNmL :: ([HsName])
              _importspecificationIgUniq :: UID
              _importspecificationIisHiding :: Bool
              _importspecificationImodEntSpecL :: ([ModEntSpec])
              _importspecificationItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 39, column 9)
              _lhsOmodImpL =
                  let as = maybe name_ id asname_
                  in  [ModImp qualified_ name_ as _importspecificationIisHiding _importspecificationImodEntSpecL range_]
              -- copy rule (up)
              _lhsOgUniq =
                  _importspecificationIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _importspecificationItopInstanceNmL
              -- copy rule (down)
              _importspecificationOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _importspecificationOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _importspecificationOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _importspecificationOopts =
                  _lhsIopts
              -- copy rule (down)
              _importspecificationOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _importspecificationIgUniq,_importspecificationIisHiding,_importspecificationImodEntSpecL,_importspecificationItopInstanceNmL) | True =
                  importspecification_ _importspecificationOgUniq _importspecificationOmoduleNm _importspecificationOnmLev _importspecificationOopts _importspecificationOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOmodImpL,_lhsOtopInstanceNmL)))
-- ImportDeclarations ------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modImpL              : [ModImp]
   alternatives:
      alternative Cons:
         child hd             : ImportDeclaration 
         child tl             : ImportDeclarations 
      alternative Nil:
-}
-- cata
sem_ImportDeclarations :: ImportDeclarations  ->
                          T_ImportDeclarations 
sem_ImportDeclarations list  =
    (Prelude.foldr sem_ImportDeclarations_Cons sem_ImportDeclarations_Nil (Prelude.map sem_ImportDeclaration list) )
-- semantic domain
type T_ImportDeclarations  = UID ->
                             HsName ->
                             NmLev ->
                             EHCOpts ->
                             ([HsName]) ->
                             ( UID,([ModImp]),([HsName]))
sem_ImportDeclarations_Cons :: T_ImportDeclaration  ->
                               T_ImportDeclarations  ->
                               T_ImportDeclarations 
sem_ImportDeclarations_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodImpL :: ([ModImp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdImodImpL :: ([ModImp])
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlImodImpL :: ([ModImp])
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 35, column 46)
              _lhsOmodImpL =
                  _hdImodImpL ++ _tlImodImpL
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdImodImpL,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlImodImpL,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOmodImpL,_lhsOtopInstanceNmL)))
sem_ImportDeclarations_Nil :: T_ImportDeclarations 
sem_ImportDeclarations_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodImpL :: ([ModImp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Module.ag"(line 35, column 46)
              _lhsOmodImpL =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodImpL,_lhsOtopInstanceNmL)))
-- ImportSpecification -----------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         isHiding             : Bool
         modEntSpecL          : [ModEntSpec]
   alternatives:
      alternative Import:
         child range          : {Range}
         child hiding         : {Bool}
         child imports        : Imports 
-}
-- cata
sem_ImportSpecification :: ImportSpecification  ->
                           T_ImportSpecification 
sem_ImportSpecification (ImportSpecification_Import _range _hiding _imports )  =
    (sem_ImportSpecification_Import _range _hiding (sem_Imports _imports ) )
-- semantic domain
type T_ImportSpecification  = UID ->
                              HsName ->
                              NmLev ->
                              EHCOpts ->
                              ([HsName]) ->
                              ( UID,Bool,([ModEntSpec]),([HsName]))
sem_ImportSpecification_Import :: Range ->
                                  Bool ->
                                  T_Imports  ->
                                  T_ImportSpecification 
sem_ImportSpecification_Import range_ hiding_ imports_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOisHiding :: Bool
              _lhsOgUniq :: UID
              _lhsOmodEntSpecL :: ([ModEntSpec])
              _lhsOtopInstanceNmL :: ([HsName])
              _importsOgUniq :: UID
              _importsOmoduleNm :: HsName
              _importsOnmLev :: NmLev
              _importsOopts :: EHCOpts
              _importsOtopInstanceNmL :: ([HsName])
              _importsIgUniq :: UID
              _importsImodEntSpecL :: ([ModEntSpec])
              _importsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 29, column 9)
              _lhsOisHiding =
                  hiding_
              -- copy rule (up)
              _lhsOgUniq =
                  _importsIgUniq
              -- copy rule (up)
              _lhsOmodEntSpecL =
                  _importsImodEntSpecL
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _importsItopInstanceNmL
              -- copy rule (down)
              _importsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _importsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _importsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _importsOopts =
                  _lhsIopts
              -- copy rule (down)
              _importsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _importsIgUniq,_importsImodEntSpecL,_importsItopInstanceNmL) | True =
                  imports_ _importsOgUniq _importsOmoduleNm _importsOnmLev _importsOopts _importsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOisHiding,_lhsOmodEntSpecL,_lhsOtopInstanceNmL)))
-- Imports -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modEntSpecL          : [ModEntSpec]
   alternatives:
      alternative Cons:
         child hd             : Import 
         child tl             : Imports 
      alternative Nil:
-}
-- cata
sem_Imports :: Imports  ->
               T_Imports 
sem_Imports list  =
    (Prelude.foldr sem_Imports_Cons sem_Imports_Nil (Prelude.map sem_Import list) )
-- semantic domain
type T_Imports  = UID ->
                  HsName ->
                  NmLev ->
                  EHCOpts ->
                  ([HsName]) ->
                  ( UID,([ModEntSpec]),([HsName]))
sem_Imports_Cons :: T_Import  ->
                    T_Imports  ->
                    T_Imports 
sem_Imports_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodEntSpecL :: ([ModEntSpec])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdImodEntSpec :: ModEntSpec
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlImodEntSpecL :: ([ModEntSpec])
              _tlItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 17, column 9)
              _lhsOmodEntSpecL =
                  _hdImodEntSpec : _tlImodEntSpecL
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdImodEntSpec,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlImodEntSpecL,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOmodEntSpecL,_lhsOtopInstanceNmL)))
sem_Imports_Nil :: T_Imports 
sem_Imports_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodEntSpecL :: ([ModEntSpec])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 19, column 9)
              _lhsOmodEntSpecL =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodEntSpecL,_lhsOtopInstanceNmL)))
-- Kind --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Annotate:
         child range          : {Range}
         child annotation     : KindAnnotation 
         child kind           : Kind 
      alternative Constructor:
         child range          : {Range}
         child name           : {Name}
      alternative Forall:
         child range          : {Range}
         child kindvariables  : {Names}
         child kind           : Kind 
      alternative InfixApplication:
         child range          : {Range}
         child leftKind       : Kind 
         child operator       : Kind 
         child rightKind      : Kind 
      alternative NormalApplication:
         child range          : {Range}
         child function       : Kind 
         child arguments      : Kinds 
      alternative Parenthesized:
         child range          : {Range}
         child kind           : Kind 
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
-}
-- cata
sem_Kind :: Kind  ->
            T_Kind 
sem_Kind (Kind_Annotate _range _annotation _kind )  =
    (sem_Kind_Annotate _range (sem_KindAnnotation _annotation ) (sem_Kind _kind ) )
sem_Kind (Kind_Constructor _range _name )  =
    (sem_Kind_Constructor _range _name )
sem_Kind (Kind_Forall _range _kindvariables _kind )  =
    (sem_Kind_Forall _range _kindvariables (sem_Kind _kind ) )
sem_Kind (Kind_InfixApplication _range _leftKind _operator _rightKind )  =
    (sem_Kind_InfixApplication _range (sem_Kind _leftKind ) (sem_Kind _operator ) (sem_Kind _rightKind ) )
sem_Kind (Kind_NormalApplication _range _function _arguments )  =
    (sem_Kind_NormalApplication _range (sem_Kind _function ) (sem_Kinds _arguments ) )
sem_Kind (Kind_Parenthesized _range _kind )  =
    (sem_Kind_Parenthesized _range (sem_Kind _kind ) )
sem_Kind (Kind_Variable _range _name )  =
    (sem_Kind_Variable _range _name )
-- semantic domain
type T_Kind  = UID ->
               NmLev ->
               ([HsName]) ->
               ( UID,([HsName]))
sem_Kind_Annotate :: Range ->
                     T_KindAnnotation  ->
                     T_Kind  ->
                     T_Kind 
sem_Kind_Annotate range_ annotation_ kind_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _annotationOgUniq :: UID
              _annotationOnmLev :: NmLev
              _annotationOtopInstanceNmL :: ([HsName])
              _kindOgUniq :: UID
              _kindOnmLev :: NmLev
              _kindOtopInstanceNmL :: ([HsName])
              _annotationIgUniq :: UID
              _annotationItopInstanceNmL :: ([HsName])
              _kindIgUniq :: UID
              _kindItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _kindIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _kindItopInstanceNmL
              -- copy rule (down)
              _annotationOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _annotationOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _annotationOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _kindOgUniq =
                  _annotationIgUniq
              -- copy rule (down)
              _kindOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _kindOtopInstanceNmL =
                  _annotationItopInstanceNmL
              ( _annotationIgUniq,_annotationItopInstanceNmL) | True =
                  annotation_ _annotationOgUniq _annotationOnmLev _annotationOtopInstanceNmL 
              ( _kindIgUniq,_kindItopInstanceNmL) | True =
                  kind_ _kindOgUniq _kindOnmLev _kindOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kind_Constructor :: Range ->
                        Name ->
                        T_Kind 
sem_Kind_Constructor range_ name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kind_Forall :: Range ->
                   Names ->
                   T_Kind  ->
                   T_Kind 
sem_Kind_Forall range_ kindvariables_ kind_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _kindOgUniq :: UID
              _kindOnmLev :: NmLev
              _kindOtopInstanceNmL :: ([HsName])
              _kindIgUniq :: UID
              _kindItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _kindIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _kindItopInstanceNmL
              -- copy rule (down)
              _kindOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _kindOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _kindOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _kindIgUniq,_kindItopInstanceNmL) | True =
                  kind_ _kindOgUniq _kindOnmLev _kindOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kind_InfixApplication :: Range ->
                             T_Kind  ->
                             T_Kind  ->
                             T_Kind  ->
                             T_Kind 
sem_Kind_InfixApplication range_ leftKind_ operator_ rightKind_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftKindOgUniq :: UID
              _leftKindOnmLev :: NmLev
              _leftKindOtopInstanceNmL :: ([HsName])
              _operatorOgUniq :: UID
              _operatorOnmLev :: NmLev
              _operatorOtopInstanceNmL :: ([HsName])
              _rightKindOgUniq :: UID
              _rightKindOnmLev :: NmLev
              _rightKindOtopInstanceNmL :: ([HsName])
              _leftKindIgUniq :: UID
              _leftKindItopInstanceNmL :: ([HsName])
              _operatorIgUniq :: UID
              _operatorItopInstanceNmL :: ([HsName])
              _rightKindIgUniq :: UID
              _rightKindItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _rightKindIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightKindItopInstanceNmL
              -- copy rule (down)
              _leftKindOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftKindOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftKindOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _operatorOgUniq =
                  _leftKindIgUniq
              -- copy rule (down)
              _operatorOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _operatorOtopInstanceNmL =
                  _leftKindItopInstanceNmL
              -- copy rule (chain)
              _rightKindOgUniq =
                  _operatorIgUniq
              -- copy rule (down)
              _rightKindOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rightKindOtopInstanceNmL =
                  _operatorItopInstanceNmL
              ( _leftKindIgUniq,_leftKindItopInstanceNmL) | True =
                  leftKind_ _leftKindOgUniq _leftKindOnmLev _leftKindOtopInstanceNmL 
              ( _operatorIgUniq,_operatorItopInstanceNmL) | True =
                  operator_ _operatorOgUniq _operatorOnmLev _operatorOtopInstanceNmL 
              ( _rightKindIgUniq,_rightKindItopInstanceNmL) | True =
                  rightKind_ _rightKindOgUniq _rightKindOnmLev _rightKindOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kind_NormalApplication :: Range ->
                              T_Kind  ->
                              T_Kinds  ->
                              T_Kind 
sem_Kind_NormalApplication range_ function_ arguments_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _functionOgUniq :: UID
              _functionOnmLev :: NmLev
              _functionOtopInstanceNmL :: ([HsName])
              _argumentsOgUniq :: UID
              _argumentsOnmLev :: NmLev
              _argumentsOtopInstanceNmL :: ([HsName])
              _functionIgUniq :: UID
              _functionItopInstanceNmL :: ([HsName])
              _argumentsIgUniq :: UID
              _argumentsItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _argumentsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _argumentsItopInstanceNmL
              -- copy rule (down)
              _functionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _functionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _functionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _argumentsOgUniq =
                  _functionIgUniq
              -- copy rule (down)
              _argumentsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _argumentsOtopInstanceNmL =
                  _functionItopInstanceNmL
              ( _functionIgUniq,_functionItopInstanceNmL) | True =
                  function_ _functionOgUniq _functionOnmLev _functionOtopInstanceNmL 
              ( _argumentsIgUniq,_argumentsItopInstanceNmL) | True =
                  arguments_ _argumentsOgUniq _argumentsOnmLev _argumentsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kind_Parenthesized :: Range ->
                          T_Kind  ->
                          T_Kind 
sem_Kind_Parenthesized range_ kind_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _kindOgUniq :: UID
              _kindOnmLev :: NmLev
              _kindOtopInstanceNmL :: ([HsName])
              _kindIgUniq :: UID
              _kindItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _kindIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _kindItopInstanceNmL
              -- copy rule (down)
              _kindOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _kindOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _kindOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _kindIgUniq,_kindItopInstanceNmL) | True =
                  kind_ _kindOgUniq _kindOnmLev _kindOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kind_Variable :: Range ->
                     Name ->
                     T_Kind 
sem_Kind_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- KindAnnotation ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Empty:
-}
-- cata
sem_KindAnnotation :: KindAnnotation  ->
                      T_KindAnnotation 
sem_KindAnnotation (KindAnnotation_Empty )  =
    (sem_KindAnnotation_Empty )
-- semantic domain
type T_KindAnnotation  = UID ->
                         NmLev ->
                         ([HsName]) ->
                         ( UID,([HsName]))
sem_KindAnnotation_Empty :: T_KindAnnotation 
sem_KindAnnotation_Empty  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Kinds -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : Kind 
         child tl             : Kinds 
      alternative Nil:
-}
-- cata
sem_Kinds :: Kinds  ->
             T_Kinds 
sem_Kinds list  =
    (Prelude.foldr sem_Kinds_Cons sem_Kinds_Nil (Prelude.map sem_Kind list) )
-- semantic domain
type T_Kinds  = UID ->
                NmLev ->
                ([HsName]) ->
                ( UID,([HsName]))
sem_Kinds_Cons :: T_Kind  ->
                  T_Kinds  ->
                  T_Kinds 
sem_Kinds_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Kinds_Nil :: T_Kinds 
sem_Kinds_Nil  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- LeftHandSide ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         nmLevFun             : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
         refname              : HsName
   alternatives:
      alternative Function:
         child range          : {Range}
         child name           : {Name}
         child patterns       : Patterns 
         visit 0:
            local mainValExists : _
            local refname     : _
      alternative Infix:
         child range          : {Range}
         child leftPattern    : Pattern 
         child operator       : {Name}
         child rightPattern   : Pattern 
         visit 0:
            local refname     : _
      alternative Parenthesized:
         child range          : {Range}
         child lefthandside   : LeftHandSide 
         child patterns       : Patterns 
      alternative Typed:
         child range          : {Range}
         child lefthandside   : LeftHandSide 
         child type           : Type 
-}
-- cata
sem_LeftHandSide :: LeftHandSide  ->
                    T_LeftHandSide 
sem_LeftHandSide (LeftHandSide_Function _range _name _patterns )  =
    (sem_LeftHandSide_Function _range _name (sem_Patterns _patterns ) )
sem_LeftHandSide (LeftHandSide_Infix _range _leftPattern _operator _rightPattern )  =
    (sem_LeftHandSide_Infix _range (sem_Pattern _leftPattern ) _operator (sem_Pattern _rightPattern ) )
sem_LeftHandSide (LeftHandSide_Parenthesized _range _lefthandside _patterns )  =
    (sem_LeftHandSide_Parenthesized _range (sem_LeftHandSide _lefthandside ) (sem_Patterns _patterns ) )
sem_LeftHandSide (LeftHandSide_Typed _range _lefthandside _type )  =
    (sem_LeftHandSide_Typed _range (sem_LeftHandSide _lefthandside ) (sem_Type _type ) )
-- semantic domain
type T_LeftHandSide  = UID ->
                       HsName ->
                       NmLev ->
                       NmLev ->
                       EHCOpts ->
                       ([HsName]) ->
                       ( UID,([IdOcc]),Bool,HsName,([HsName]))
sem_LeftHandSide_Function :: Range ->
                             Name ->
                             T_Patterns  ->
                             T_LeftHandSide 
sem_LeftHandSide_Function range_ name_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsInmLevFun
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOrefname :: HsName
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsImainValExists :: Bool
              _patternsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 75, column 9)
              _mainValExists =
                  name_ == hsnMain
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 47, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLevFun _lhsImoduleNm name_
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _mainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (from local)
              _lhsOrefname =
                  _refname
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsImainValExists,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOrefname,_lhsOtopInstanceNmL)))
sem_LeftHandSide_Infix :: Range ->
                          T_Pattern  ->
                          Name ->
                          T_Pattern  ->
                          T_LeftHandSide 
sem_LeftHandSide_Infix range_ leftPattern_ operator_ rightPattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsInmLevFun
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOrefname :: HsName
              _lhsOtopInstanceNmL :: ([HsName])
              _leftPatternOgUniq :: UID
              _leftPatternOmoduleNm :: HsName
              _leftPatternOnmLev :: NmLev
              _leftPatternOtopInstanceNmL :: ([HsName])
              _rightPatternOgUniq :: UID
              _rightPatternOmoduleNm :: HsName
              _rightPatternOnmLev :: NmLev
              _rightPatternOtopInstanceNmL :: ([HsName])
              _leftPatternIgUniq :: UID
              _leftPatternIidOccDefs :: ([IdOcc])
              _leftPatternImainValExists :: Bool
              _leftPatternImbTopRefname :: (Maybe HsName)
              _leftPatternItopInstanceNmL :: ([HsName])
              _rightPatternIgUniq :: UID
              _rightPatternIidOccDefs :: ([IdOcc])
              _rightPatternImainValExists :: Bool
              _rightPatternImbTopRefname :: (Maybe HsName)
              _rightPatternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 49, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLevFun _lhsImoduleNm operator_
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _leftPatternIidOccDefs ++ _rightPatternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _leftPatternImainValExists || _rightPatternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _rightPatternIgUniq
              -- copy rule (from local)
              _lhsOrefname =
                  _refname
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightPatternItopInstanceNmL
              -- copy rule (down)
              _leftPatternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftPatternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _leftPatternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftPatternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rightPatternOgUniq =
                  _leftPatternIgUniq
              -- copy rule (down)
              _rightPatternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rightPatternOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rightPatternOtopInstanceNmL =
                  _leftPatternItopInstanceNmL
              ( _leftPatternIgUniq,_leftPatternIidOccDefs,_leftPatternImainValExists,_leftPatternImbTopRefname,_leftPatternItopInstanceNmL) | True =
                  leftPattern_ _leftPatternOgUniq _leftPatternOmoduleNm _leftPatternOnmLev _leftPatternOtopInstanceNmL 
              ( _rightPatternIgUniq,_rightPatternIidOccDefs,_rightPatternImainValExists,_rightPatternImbTopRefname,_rightPatternItopInstanceNmL) | True =
                  rightPattern_ _rightPatternOgUniq _rightPatternOmoduleNm _rightPatternOnmLev _rightPatternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOrefname,_lhsOtopInstanceNmL)))
sem_LeftHandSide_Parenthesized :: Range ->
                                  T_LeftHandSide  ->
                                  T_Patterns  ->
                                  T_LeftHandSide 
sem_LeftHandSide_Parenthesized range_ lefthandside_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsInmLevFun
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOrefname :: HsName
              _lhsOtopInstanceNmL :: ([HsName])
              _lefthandsideOgUniq :: UID
              _lefthandsideOmoduleNm :: HsName
              _lefthandsideOnmLev :: NmLev
              _lefthandsideOnmLevFun :: NmLev
              _lefthandsideOopts :: EHCOpts
              _lefthandsideOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _lefthandsideIgUniq :: UID
              _lefthandsideIidOccDefs :: ([IdOcc])
              _lefthandsideImainValExists :: Bool
              _lefthandsideIrefname :: HsName
              _lefthandsideItopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsImainValExists :: Bool
              _patternsItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _lefthandsideIidOccDefs ++ _patternsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _lefthandsideImainValExists || _patternsImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (up)
              _lhsOrefname =
                  _lefthandsideIrefname
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _lefthandsideOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _lefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _lefthandsideOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _lefthandsideOnmLevFun =
                  _lhsInmLevFun
              -- copy rule (down)
              _lefthandsideOopts =
                  _lhsIopts
              -- copy rule (down)
              _lefthandsideOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _patternsOgUniq =
                  _lefthandsideIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _patternsOtopInstanceNmL =
                  _lefthandsideItopInstanceNmL
              ( _lefthandsideIgUniq,_lefthandsideIidOccDefs,_lefthandsideImainValExists,_lefthandsideIrefname,_lefthandsideItopInstanceNmL) | True =
                  lefthandside_ _lefthandsideOgUniq _lefthandsideOmoduleNm _lefthandsideOnmLev _lefthandsideOnmLevFun _lefthandsideOopts _lefthandsideOtopInstanceNmL 
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsImainValExists,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOrefname,_lhsOtopInstanceNmL)))
sem_LeftHandSide_Typed :: Range ->
                          T_LeftHandSide  ->
                          T_Type  ->
                          T_LeftHandSide 
sem_LeftHandSide_Typed range_ lefthandside_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsInmLevFun
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _typeOnmLev :: NmLev
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOrefname :: HsName
              _lhsOtopInstanceNmL :: ([HsName])
              _lefthandsideOgUniq :: UID
              _lefthandsideOmoduleNm :: HsName
              _lefthandsideOnmLev :: NmLev
              _lefthandsideOnmLevFun :: NmLev
              _lefthandsideOopts :: EHCOpts
              _lefthandsideOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOtopInstanceNmL :: ([HsName])
              _lefthandsideIgUniq :: UID
              _lefthandsideIidOccDefs :: ([IdOcc])
              _lefthandsideImainValExists :: Bool
              _lefthandsideIrefname :: HsName
              _lefthandsideItopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 60, column 9)
              _typeOnmLev =
                  _lhsInmLev + 1
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _lefthandsideIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _lefthandsideImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOrefname =
                  _lefthandsideIrefname
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _lefthandsideOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _lefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _lefthandsideOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _lefthandsideOnmLevFun =
                  _lhsInmLevFun
              -- copy rule (down)
              _lefthandsideOopts =
                  _lhsIopts
              -- copy rule (down)
              _lefthandsideOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _lefthandsideIgUniq
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _lefthandsideItopInstanceNmL
              ( _lefthandsideIgUniq,_lefthandsideIidOccDefs,_lefthandsideImainValExists,_lefthandsideIrefname,_lefthandsideItopInstanceNmL) | True =
                  lefthandside_ _lefthandsideOgUniq _lefthandsideOmoduleNm _lefthandsideOnmLev _lefthandsideOnmLevFun _lefthandsideOopts _lefthandsideOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOrefname,_lhsOtopInstanceNmL)))
-- Literal -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Char:
         child range          : {Range}
         child value          : {String}
      alternative Float:
         child range          : {Range}
         child value          : {String}
      alternative Int:
         child range          : {Range}
         child base           : {Int}
         child value          : {String}
      alternative String:
         child range          : {Range}
         child value          : {String}
-}
-- cata
sem_Literal :: Literal  ->
               T_Literal 
sem_Literal (Literal_Char _range _value )  =
    (sem_Literal_Char _range _value )
sem_Literal (Literal_Float _range _value )  =
    (sem_Literal_Float _range _value )
sem_Literal (Literal_Int _range _base _value )  =
    (sem_Literal_Int _range _base _value )
sem_Literal (Literal_String _range _value )  =
    (sem_Literal_String _range _value )
-- semantic domain
type T_Literal  = UID ->
                  NmLev ->
                  ([HsName]) ->
                  ( UID,([HsName]))
sem_Literal_Char :: Range ->
                    String ->
                    T_Literal 
sem_Literal_Char range_ value_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Literal_Float :: Range ->
                     String ->
                     T_Literal 
sem_Literal_Float range_ value_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Literal_Int :: Range ->
                   Int ->
                   String ->
                   T_Literal 
sem_Literal_Int range_ base_ value_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Literal_String :: Range ->
                      String ->
                      T_Literal 
sem_Literal_String range_ value_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- MaybeDeclarations -------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         gathPragmas          : Set.Set Pragma.Pragma
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
   alternatives:
      alternative Just:
         child just           : Declarations 
      alternative Nothing:
-}
-- cata
sem_MaybeDeclarations :: MaybeDeclarations  ->
                         T_MaybeDeclarations 
sem_MaybeDeclarations (Prelude.Just x )  =
    (sem_MaybeDeclarations_Just (sem_Declarations x ) )
sem_MaybeDeclarations Prelude.Nothing  =
    sem_MaybeDeclarations_Nothing
-- semantic domain
type T_MaybeDeclarations  = UID ->
                            HsName ->
                            NmLev ->
                            EHCOpts ->
                            ([HsName]) ->
                            ( UID,(Set.Set Pragma.Pragma),([IdOcc]),Bool,([HsName]))
sem_MaybeDeclarations_Just :: T_Declarations  ->
                              T_MaybeDeclarations 
sem_MaybeDeclarations_Just just_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _justOgUniq :: UID
              _justOmoduleNm :: HsName
              _justOnmLev :: NmLev
              _justOopts :: EHCOpts
              _justOtopInstanceNmL :: ([HsName])
              _justIgUniq :: UID
              _justIgathPragmas :: (Set.Set Pragma.Pragma)
              _justIidOccDefs :: ([IdOcc])
              _justImainValExists :: Bool
              _justImodDefsRel :: ModEntRel
              _justImodHideDefsRel :: ModEntRel
              _justItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _justIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _justIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _justImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _justItopInstanceNmL
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _justOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _justIgUniq,_justIgathPragmas,_justIidOccDefs,_justImainValExists,_justImodDefsRel,_justImodHideDefsRel,_justItopInstanceNmL) | True =
                  just_ _justOgUniq _justOmoduleNm _justOnmLev _justOopts _justOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
sem_MaybeDeclarations_Nothing :: T_MaybeDeclarations 
sem_MaybeDeclarations_Nothing  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
-- MaybeExports ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         modExpsMb            : Maybe [ModExp]
   alternatives:
      alternative Just:
         child just           : Exports 
      alternative Nothing:
-}
-- cata
sem_MaybeExports :: MaybeExports  ->
                    T_MaybeExports 
sem_MaybeExports (Prelude.Just x )  =
    (sem_MaybeExports_Just (sem_Exports x ) )
sem_MaybeExports Prelude.Nothing  =
    sem_MaybeExports_Nothing
-- semantic domain
type T_MaybeExports  = UID ->
                       HsName ->
                       NmLev ->
                       EHCOpts ->
                       ([HsName]) ->
                       ( UID,(Maybe [ModExp]),([HsName]))
sem_MaybeExports_Just :: T_Exports  ->
                         T_MaybeExports 
sem_MaybeExports_Just just_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExpsMb :: (Maybe [ModExp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _justOgUniq :: UID
              _justOmoduleNm :: HsName
              _justOnmLev :: NmLev
              _justOopts :: EHCOpts
              _justOtopInstanceNmL :: ([HsName])
              _justIgUniq :: UID
              _justImodExpL :: ([ModExp])
              _justItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 92, column 9)
              _lhsOmodExpsMb =
                  Just _justImodExpL
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _justItopInstanceNmL
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _justOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _justIgUniq,_justImodExpL,_justItopInstanceNmL) | True =
                  just_ _justOgUniq _justOmoduleNm _justOnmLev _justOopts _justOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOmodExpsMb,_lhsOtopInstanceNmL)))
sem_MaybeExports_Nothing :: T_MaybeExports 
sem_MaybeExports_Nothing  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodExpsMb :: (Maybe [ModExp])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 94, column 9)
              _lhsOmodExpsMb =
                  Nothing
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOmodExpsMb,_lhsOtopInstanceNmL)))
-- MaybeExpression ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Just:
         child just           : Expression 
      alternative Nothing:
-}
-- cata
sem_MaybeExpression :: MaybeExpression  ->
                       T_MaybeExpression 
sem_MaybeExpression (Prelude.Just x )  =
    (sem_MaybeExpression_Just (sem_Expression x ) )
sem_MaybeExpression Prelude.Nothing  =
    sem_MaybeExpression_Nothing
-- semantic domain
type T_MaybeExpression  = UID ->
                          HsName ->
                          NmLev ->
                          EHCOpts ->
                          ([HsName]) ->
                          ( UID,([HsName]))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _justOgUniq :: UID
              _justOmoduleNm :: HsName
              _justOnmLev :: NmLev
              _justOopts :: EHCOpts
              _justOtopInstanceNmL :: ([HsName])
              _justIconNm :: Name
              _justIgUniq :: UID
              _justItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _justItopInstanceNmL
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _justOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _justIconNm,_justIgUniq,_justItopInstanceNmL) | True =
                  just_ _justOgUniq _justOmoduleNm _justOnmLev _justOopts _justOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- MaybeImportSpecification ------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         isHiding             : Bool
         modEntSpecL          : [ModEntSpec]
   alternatives:
      alternative Just:
         child just           : ImportSpecification 
      alternative Nothing:
-}
-- cata
sem_MaybeImportSpecification :: MaybeImportSpecification  ->
                                T_MaybeImportSpecification 
sem_MaybeImportSpecification (Prelude.Just x )  =
    (sem_MaybeImportSpecification_Just (sem_ImportSpecification x ) )
sem_MaybeImportSpecification Prelude.Nothing  =
    sem_MaybeImportSpecification_Nothing
-- semantic domain
type T_MaybeImportSpecification  = UID ->
                                   HsName ->
                                   NmLev ->
                                   EHCOpts ->
                                   ([HsName]) ->
                                   ( UID,Bool,([ModEntSpec]),([HsName]))
sem_MaybeImportSpecification_Just :: T_ImportSpecification  ->
                                     T_MaybeImportSpecification 
sem_MaybeImportSpecification_Just just_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOisHiding :: Bool
              _lhsOmodEntSpecL :: ([ModEntSpec])
              _lhsOtopInstanceNmL :: ([HsName])
              _justOgUniq :: UID
              _justOmoduleNm :: HsName
              _justOnmLev :: NmLev
              _justOopts :: EHCOpts
              _justOtopInstanceNmL :: ([HsName])
              _justIgUniq :: UID
              _justIisHiding :: Bool
              _justImodEntSpecL :: ([ModEntSpec])
              _justItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (up)
              _lhsOisHiding =
                  _justIisHiding
              -- copy rule (up)
              _lhsOmodEntSpecL =
                  _justImodEntSpecL
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _justItopInstanceNmL
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _justOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _justIgUniq,_justIisHiding,_justImodEntSpecL,_justItopInstanceNmL) | True =
                  just_ _justOgUniq _justOmoduleNm _justOnmLev _justOopts _justOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOisHiding,_lhsOmodEntSpecL,_lhsOtopInstanceNmL)))
sem_MaybeImportSpecification_Nothing :: T_MaybeImportSpecification 
sem_MaybeImportSpecification_Nothing  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodEntSpecL :: ([ModEntSpec])
              _lhsOisHiding :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 23, column 9)
              _lhsOmodEntSpecL =
                  []
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 33, column 9)
              _lhsOisHiding =
                  True
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOisHiding,_lhsOmodEntSpecL,_lhsOtopInstanceNmL)))
-- MaybeType ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Just:
         child just           : Type 
      alternative Nothing:
-}
-- cata
sem_MaybeType :: MaybeType  ->
                 T_MaybeType 
sem_MaybeType (Prelude.Just x )  =
    (sem_MaybeType_Just (sem_Type x ) )
sem_MaybeType Prelude.Nothing  =
    sem_MaybeType_Nothing
-- semantic domain
type T_MaybeType  = UID ->
                    NmLev ->
                    ([HsName]) ->
                    ( UID,([HsName]))
sem_MaybeType_Just :: T_Type  ->
                      T_MaybeType 
sem_MaybeType_Just just_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _justOgUniq :: UID
              _justOnmLev :: NmLev
              _justOtopInstanceNmL :: ([HsName])
              _justIconNm :: Name
              _justIgUniq :: UID
              _justItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _justItopInstanceNmL
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _justOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _justIconNm,_justIgUniq,_justItopInstanceNmL) | True =
                  just_ _justOgUniq _justOnmLev _justOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_MaybeType_Nothing :: T_MaybeType 
sem_MaybeType_Nothing  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Module ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         fileHeaderPragmas    : Set.Set Pragma.Pragma
         mainValExists        : Bool
         mod                  : Mod
         modImpNmS            : Set.Set HsName
         realModuleNm         : HsName
   alternatives:
      alternative Module:
         child range          : {Range}
         child name           : {MaybeName}
         child fileheaderpragmas : Pragmas 
         child exports        : MaybeExports 
         child body           : Body 
         visit 0:
            local modImpL     : _
            local realModuleNm : _
            local fileHeaderPragmas : _
-}
-- cata
sem_Module :: Module  ->
              T_Module 
sem_Module (Module_Module _range _name _fileheaderpragmas _exports _body )  =
    (sem_Module_Module _range _name (sem_Pragmas _fileheaderpragmas ) (sem_MaybeExports _exports ) (sem_Body _body ) )
-- semantic domain
type T_Module  = UID ->
                 HsName ->
                 NmLev ->
                 EHCOpts ->
                 ([HsName]) ->
                 ( (Set.Set Pragma.Pragma),UID,Bool,Mod,(Set.Set HsName),HsName,([HsName]))
sem_Module_Module :: Range ->
                     MaybeName ->
                     T_Pragmas  ->
                     T_MaybeExports  ->
                     T_Body  ->
                     T_Module 
sem_Module_Module range_ name_ fileheaderpragmas_ exports_ body_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOmodImpNmS :: (Set.Set HsName)
              _lhsOmod :: Mod
              _lhsOfileHeaderPragmas :: (Set.Set Pragma.Pragma)
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOrealModuleNm :: HsName
              _lhsOtopInstanceNmL :: ([HsName])
              _fileheaderpragmasOgUniq :: UID
              _fileheaderpragmasOnmLev :: NmLev
              _fileheaderpragmasOtopInstanceNmL :: ([HsName])
              _exportsOgUniq :: UID
              _exportsOmoduleNm :: HsName
              _exportsOnmLev :: NmLev
              _exportsOopts :: EHCOpts
              _exportsOtopInstanceNmL :: ([HsName])
              _bodyOgUniq :: UID
              _bodyOmoduleNm :: HsName
              _bodyOnmLev :: NmLev
              _bodyOopts :: EHCOpts
              _bodyOtopInstanceNmL :: ([HsName])
              _fileheaderpragmasIgUniq :: UID
              _fileheaderpragmasIgathPragmas :: (Set.Set Pragma.Pragma)
              _fileheaderpragmasItopInstanceNmL :: ([HsName])
              _exportsIgUniq :: UID
              _exportsImodExpsMb :: (Maybe [ModExp])
              _exportsItopInstanceNmL :: ([HsName])
              _bodyIgUniq :: UID
              _bodyImainValExists :: Bool
              _bodyImodDefsRel :: ModEntRel
              _bodyImodHideDefsRel :: ModEntRel
              _bodyImodImpL :: ([ModImp])
              _bodyItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 46, column 9)
              _modImpL =
                  if hsnModPrelude `elem` map mimpSource _bodyImodImpL
                     || _realModuleNm == hsnModPrelude
                     || hsnIsInPrelude _realModuleNm
                     || not (ehcOptUseAssumePrelude _lhsIopts)
                     || Set.member Pragma.Pragma_NoImplicitPrelude _fileHeaderPragmas
                  then _bodyImodImpL
                  else modImpPrelude : _bodyImodImpL
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 58, column 9)
              _lhsOmodImpNmS =
                  Set.fromList $ map mimpSource _modImpL
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 64, column 9)
              _realModuleNm =
                  maybe _lhsImoduleNm id name_
              -- "build/101/lib-ehc/EH101/HS/Module.ag"(line 156, column 9)
              _lhsOmod =
                  Mod _lhsImoduleNm name_
                      _exportsImodExpsMb _modImpL
                      _bodyImodDefsRel _bodyImodHideDefsRel
                      (reverse _bodyItopInstanceNmL)
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 21, column 9)
              _fileHeaderPragmas =
                  _fileheaderpragmasIgathPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 2, column 43)
              _lhsOfileHeaderPragmas =
                  _fileHeaderPragmas
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _bodyImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOrealModuleNm =
                  _realModuleNm
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _bodyItopInstanceNmL
              -- copy rule (down)
              _fileheaderpragmasOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _fileheaderpragmasOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _fileheaderpragmasOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _exportsOgUniq =
                  _fileheaderpragmasIgUniq
              -- copy rule (down)
              _exportsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _exportsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _exportsOopts =
                  _lhsIopts
              -- copy rule (chain)
              _exportsOtopInstanceNmL =
                  _fileheaderpragmasItopInstanceNmL
              -- copy rule (chain)
              _bodyOgUniq =
                  _exportsIgUniq
              -- copy rule (down)
              _bodyOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _bodyOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (chain)
              _bodyOtopInstanceNmL =
                  _exportsItopInstanceNmL
              ( _fileheaderpragmasIgUniq,_fileheaderpragmasIgathPragmas,_fileheaderpragmasItopInstanceNmL) | True =
                  fileheaderpragmas_ _fileheaderpragmasOgUniq _fileheaderpragmasOnmLev _fileheaderpragmasOtopInstanceNmL 
              ( _exportsIgUniq,_exportsImodExpsMb,_exportsItopInstanceNmL) | True =
                  exports_ _exportsOgUniq _exportsOmoduleNm _exportsOnmLev _exportsOopts _exportsOtopInstanceNmL 
              ( _bodyIgUniq,_bodyImainValExists,_bodyImodDefsRel,_bodyImodHideDefsRel,_bodyImodImpL,_bodyItopInstanceNmL) | True =
                  body_ _bodyOgUniq _bodyOmoduleNm _bodyOnmLev _bodyOopts _bodyOtopInstanceNmL 
          in  ( _lhsOfileHeaderPragmas,_lhsOgUniq,_lhsOmainValExists,_lhsOmod,_lhsOmodImpNmS,_lhsOrealModuleNm,_lhsOtopInstanceNmL)))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
         mbTopRefname         : Maybe HsName
   alternatives:
      alternative Annotate:
         child range          : {Range}
         child annotation     : PatternAnnotation 
         child pattern        : Pattern 
      alternative As:
         child range          : {Range}
         child name           : {Name}
         child pattern        : Pattern 
         visit 0:
            local refname     : _
            local isWildcard  : _
            local idOccDef    : _
            local idOccDefsNoWildcard : _
            local idOccDefs   : _
      alternative Bang:
         child range          : {Range}
         child pattern        : Pattern 
      alternative Constructor:
         child range          : {Range}
         child name           : {Name}
         child patterns       : Patterns 
         visit 0:
            local conNm       : _
      alternative InfixConstructor:
         child range          : {Range}
         child leftPattern    : Pattern 
         child constructorOperator : {Name}
         child rightPattern   : Pattern 
         visit 0:
            local conNm       : _
      alternative Irrefutable:
         child range          : {Range}
         child pattern        : Pattern 
      alternative List:
         child range          : {Range}
         child patterns       : Patterns 
      alternative Literal:
         child range          : {Range}
         child sign           : {Int}
         child literal        : Literal 
      alternative Parenthesized:
         child range          : {Range}
         child pattern        : Pattern 
      alternative Record:
         child range          : {Range}
         child name           : {Name}
         child recordPatternBindings : RecordPatternBindings 
         visit 0:
            local conNm       : _
      alternative RowRecordBinding:
         child range          : {Range}
         child pattern        : Pattern 
         child rowRecordPattternBindings : RowRecordPatternBindings 
      alternative RowRecordEmpty:
         child range          : {Range}
      alternative Tuple:
         child range          : {Range}
         child arity          : {Int}
         child patterns       : Patterns 
      alternative Typed:
         child range          : {Range}
         child pattern        : Pattern 
         child type           : Type 
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
         visit 0:
            local mainValExists : _
            local refname     : _
            local isWildcard  : _
            local idOccDef    : _
            local idOccDefsNoWildcard : _
            local idOccDefs   : _
      alternative Wildcard:
         child range          : {Range}
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Pattern_Annotate _range _annotation _pattern )  =
    (sem_Pattern_Annotate _range (sem_PatternAnnotation _annotation ) (sem_Pattern _pattern ) )
sem_Pattern (Pattern_As _range _name _pattern )  =
    (sem_Pattern_As _range _name (sem_Pattern _pattern ) )
sem_Pattern (Pattern_Bang _range _pattern )  =
    (sem_Pattern_Bang _range (sem_Pattern _pattern ) )
sem_Pattern (Pattern_Constructor _range _name _patterns )  =
    (sem_Pattern_Constructor _range _name (sem_Patterns _patterns ) )
sem_Pattern (Pattern_InfixConstructor _range _leftPattern _constructorOperator _rightPattern )  =
    (sem_Pattern_InfixConstructor _range (sem_Pattern _leftPattern ) _constructorOperator (sem_Pattern _rightPattern ) )
sem_Pattern (Pattern_Irrefutable _range _pattern )  =
    (sem_Pattern_Irrefutable _range (sem_Pattern _pattern ) )
sem_Pattern (Pattern_List _range _patterns )  =
    (sem_Pattern_List _range (sem_Patterns _patterns ) )
sem_Pattern (Pattern_Literal _range _sign _literal )  =
    (sem_Pattern_Literal _range _sign (sem_Literal _literal ) )
sem_Pattern (Pattern_Parenthesized _range _pattern )  =
    (sem_Pattern_Parenthesized _range (sem_Pattern _pattern ) )
sem_Pattern (Pattern_Record _range _name _recordPatternBindings )  =
    (sem_Pattern_Record _range _name (sem_RecordPatternBindings _recordPatternBindings ) )
sem_Pattern (Pattern_RowRecordBinding _range _pattern _rowRecordPattternBindings )  =
    (sem_Pattern_RowRecordBinding _range (sem_Pattern _pattern ) (sem_RowRecordPatternBindings _rowRecordPattternBindings ) )
sem_Pattern (Pattern_RowRecordEmpty _range )  =
    (sem_Pattern_RowRecordEmpty _range )
sem_Pattern (Pattern_Tuple _range _arity _patterns )  =
    (sem_Pattern_Tuple _range _arity (sem_Patterns _patterns ) )
sem_Pattern (Pattern_Typed _range _pattern _type )  =
    (sem_Pattern_Typed _range (sem_Pattern _pattern ) (sem_Type _type ) )
sem_Pattern (Pattern_Variable _range _name )  =
    (sem_Pattern_Variable _range _name )
sem_Pattern (Pattern_Wildcard _range )  =
    (sem_Pattern_Wildcard _range )
-- semantic domain
type T_Pattern  = UID ->
                  HsName ->
                  NmLev ->
                  ([HsName]) ->
                  ( UID,([IdOcc]),Bool,(Maybe HsName),([HsName]))
sem_Pattern_Annotate :: Range ->
                        T_PatternAnnotation  ->
                        T_Pattern  ->
                        T_Pattern 
sem_Pattern_Annotate range_ annotation_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _annotationOgUniq :: UID
              _annotationOmoduleNm :: HsName
              _annotationOnmLev :: NmLev
              _annotationOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _annotationIgUniq :: UID
              _annotationIidOccDefs :: ([IdOcc])
              _annotationItopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _annotationIidOccDefs ++ _patternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _annotationOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _annotationOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _annotationOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _annotationOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _patternOgUniq =
                  _annotationIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _patternOtopInstanceNmL =
                  _annotationItopInstanceNmL
              ( _annotationIgUniq,_annotationIidOccDefs,_annotationItopInstanceNmL) | True =
                  annotation_ _annotationOgUniq _annotationOmoduleNm _annotationOnmLev _annotationOtopInstanceNmL 
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_As :: Range ->
                  Name ->
                  T_Pattern  ->
                  T_Pattern 
sem_Pattern_As range_ name_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 54, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 100, column 9)
              _isWildcard =
                  hsnIsWild name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 174, column 9)
              _idOccDef =
                  IdOcc _refname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 174, column 9)
              _idOccDefsNoWildcard =
                  if _isWildcard then [] else [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 180, column 9)
              _idOccDefs =
                  _idOccDefsNoWildcard ++ _patternIidOccDefs
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 227, column 9)
              _lhsOmbTopRefname =
                  if _isWildcard then Just _refname else Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Bang :: Range ->
                    T_Pattern  ->
                    T_Pattern 
sem_Pattern_Bang range_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Constructor :: Range ->
                           Name ->
                           T_Patterns  ->
                           T_Pattern 
sem_Pattern_Constructor range_ name_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsImainValExists :: Bool
              _patternsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 219, column 9)
              _conNm =
                  name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternsImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsImainValExists,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_InfixConstructor :: Range ->
                                T_Pattern  ->
                                Name ->
                                T_Pattern  ->
                                T_Pattern 
sem_Pattern_InfixConstructor range_ leftPattern_ constructorOperator_ rightPattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftPatternOgUniq :: UID
              _leftPatternOmoduleNm :: HsName
              _leftPatternOnmLev :: NmLev
              _leftPatternOtopInstanceNmL :: ([HsName])
              _rightPatternOgUniq :: UID
              _rightPatternOmoduleNm :: HsName
              _rightPatternOnmLev :: NmLev
              _rightPatternOtopInstanceNmL :: ([HsName])
              _leftPatternIgUniq :: UID
              _leftPatternIidOccDefs :: ([IdOcc])
              _leftPatternImainValExists :: Bool
              _leftPatternImbTopRefname :: (Maybe HsName)
              _leftPatternItopInstanceNmL :: ([HsName])
              _rightPatternIgUniq :: UID
              _rightPatternIidOccDefs :: ([IdOcc])
              _rightPatternImainValExists :: Bool
              _rightPatternImbTopRefname :: (Maybe HsName)
              _rightPatternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 221, column 9)
              _conNm =
                  constructorOperator_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _leftPatternIidOccDefs ++ _rightPatternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _leftPatternImainValExists || _rightPatternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _rightPatternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightPatternItopInstanceNmL
              -- copy rule (down)
              _leftPatternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftPatternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _leftPatternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftPatternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rightPatternOgUniq =
                  _leftPatternIgUniq
              -- copy rule (down)
              _rightPatternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rightPatternOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rightPatternOtopInstanceNmL =
                  _leftPatternItopInstanceNmL
              ( _leftPatternIgUniq,_leftPatternIidOccDefs,_leftPatternImainValExists,_leftPatternImbTopRefname,_leftPatternItopInstanceNmL) | True =
                  leftPattern_ _leftPatternOgUniq _leftPatternOmoduleNm _leftPatternOnmLev _leftPatternOtopInstanceNmL 
              ( _rightPatternIgUniq,_rightPatternIidOccDefs,_rightPatternImainValExists,_rightPatternImbTopRefname,_rightPatternItopInstanceNmL) | True =
                  rightPattern_ _rightPatternOgUniq _rightPatternOmoduleNm _rightPatternOnmLev _rightPatternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Irrefutable :: Range ->
                           T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable range_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_List :: Range ->
                    T_Patterns  ->
                    T_Pattern 
sem_Pattern_List range_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsImainValExists :: Bool
              _patternsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternsImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsImainValExists,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Literal :: Range ->
                       Int ->
                       T_Literal  ->
                       T_Pattern 
sem_Pattern_Literal range_ sign_ literal_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _literalOgUniq :: UID
              _literalOnmLev :: NmLev
              _literalOtopInstanceNmL :: ([HsName])
              _literalIgUniq :: UID
              _literalItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (up)
              _lhsOgUniq =
                  _literalIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _literalItopInstanceNmL
              -- copy rule (down)
              _literalOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _literalOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _literalOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _literalIgUniq,_literalItopInstanceNmL) | True =
                  literal_ _literalOgUniq _literalOnmLev _literalOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Parenthesized :: Range ->
                             T_Pattern  ->
                             T_Pattern 
sem_Pattern_Parenthesized range_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Record :: Range ->
                      Name ->
                      T_RecordPatternBindings  ->
                      T_Pattern 
sem_Pattern_Record range_ name_ recordPatternBindings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _recordPatternBindingsOgUniq :: UID
              _recordPatternBindingsOmoduleNm :: HsName
              _recordPatternBindingsOnmLev :: NmLev
              _recordPatternBindingsOtopInstanceNmL :: ([HsName])
              _recordPatternBindingsIgUniq :: UID
              _recordPatternBindingsIidOccDefs :: ([IdOcc])
              _recordPatternBindingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 219, column 9)
              _conNm =
                  name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _recordPatternBindingsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (up)
              _lhsOgUniq =
                  _recordPatternBindingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _recordPatternBindingsItopInstanceNmL
              -- copy rule (down)
              _recordPatternBindingsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _recordPatternBindingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _recordPatternBindingsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _recordPatternBindingsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _recordPatternBindingsIgUniq,_recordPatternBindingsIidOccDefs,_recordPatternBindingsItopInstanceNmL) | True =
                  recordPatternBindings_ _recordPatternBindingsOgUniq _recordPatternBindingsOmoduleNm _recordPatternBindingsOnmLev _recordPatternBindingsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_RowRecordBinding :: Range ->
                                T_Pattern  ->
                                T_RowRecordPatternBindings  ->
                                T_Pattern 
sem_Pattern_RowRecordBinding range_ pattern_ rowRecordPattternBindings_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _rowRecordPattternBindingsOgUniq :: UID
              _rowRecordPattternBindingsOmoduleNm :: HsName
              _rowRecordPattternBindingsOnmLev :: NmLev
              _rowRecordPattternBindingsOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _rowRecordPattternBindingsIgUniq :: UID
              _rowRecordPattternBindingsIidOccDefs :: ([IdOcc])
              _rowRecordPattternBindingsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs ++ _rowRecordPattternBindingsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _rowRecordPattternBindingsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rowRecordPattternBindingsItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rowRecordPattternBindingsOgUniq =
                  _patternIgUniq
              -- copy rule (down)
              _rowRecordPattternBindingsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rowRecordPattternBindingsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rowRecordPattternBindingsOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _rowRecordPattternBindingsIgUniq,_rowRecordPattternBindingsIidOccDefs,_rowRecordPattternBindingsItopInstanceNmL) | True =
                  rowRecordPattternBindings_ _rowRecordPattternBindingsOgUniq _rowRecordPattternBindingsOmoduleNm _rowRecordPattternBindingsOnmLev _rowRecordPattternBindingsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_RowRecordEmpty :: Range ->
                              T_Pattern 
sem_Pattern_RowRecordEmpty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Tuple :: Range ->
                     Int ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Tuple range_ arity_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsIidOccDefs :: ([IdOcc])
              _patternsImainValExists :: Bool
              _patternsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternsIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternsImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternsIgUniq,_patternsIidOccDefs,_patternsImainValExists,_patternsItopInstanceNmL) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Typed :: Range ->
                     T_Pattern  ->
                     T_Type  ->
                     T_Pattern 
sem_Pattern_Typed range_ pattern_ type_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _typeOnmLev :: NmLev
              _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 56, column 9)
              _typeOnmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _patternImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _patternIgUniq
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Variable :: Range ->
                        Name ->
                        T_Pattern 
sem_Pattern_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 81, column 9)
              _mainValExists =
                  name_ == hsnMain
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 54, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 100, column 9)
              _isWildcard =
                  hsnIsWild name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 174, column 9)
              _idOccDef =
                  IdOcc _refname IdOcc_Val
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 174, column 9)
              _idOccDefsNoWildcard =
                  if _isWildcard then [] else [_idOccDef]
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 178, column 9)
              _idOccDefs =
                  _idOccDefsNoWildcard
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 227, column 9)
              _lhsOmbTopRefname =
                  if _isWildcard then Just _refname else Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _idOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _mainValExists
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
sem_Pattern_Wildcard :: Range ->
                        T_Pattern 
sem_Pattern_Wildcard range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOmbTopRefname :: (Maybe HsName)
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 229, column 9)
              _lhsOmbTopRefname =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOmbTopRefname,_lhsOtopInstanceNmL)))
-- PatternAnnotation -------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Empty:
-}
-- cata
sem_PatternAnnotation :: PatternAnnotation  ->
                         T_PatternAnnotation 
sem_PatternAnnotation (PatternAnnotation_Empty )  =
    (sem_PatternAnnotation_Empty )
-- semantic domain
type T_PatternAnnotation  = UID ->
                            HsName ->
                            NmLev ->
                            ([HsName]) ->
                            ( UID,([IdOcc]),([HsName]))
sem_PatternAnnotation_Empty :: T_PatternAnnotation 
sem_PatternAnnotation_Empty  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         idOccDefs            : [IdOcc]
         mainValExists        : Bool
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
      alternative Nil:
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
type T_Patterns  = UID ->
                   HsName ->
                   NmLev ->
                   ([HsName]) ->
                   ( UID,([IdOcc]),Bool,([HsName]))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIidOccDefs :: ([IdOcc])
              _hdImainValExists :: Bool
              _hdImbTopRefname :: (Maybe HsName)
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIidOccDefs :: ([IdOcc])
              _tlImainValExists :: Bool
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  _hdImainValExists || _tlImainValExists
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIidOccDefs,_hdImainValExists,_hdImbTopRefname,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIidOccDefs,_tlImainValExists,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOmainValExists :: Bool
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/ModImpExp.ag"(line 71, column 25)
              _lhsOmainValExists =
                  False
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOmainValExists,_lhsOtopInstanceNmL)))
-- Pragma ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         gathPragmas          : Set.Set Pragma.Pragma
   alternatives:
      alternative Derivable:
         child range          : {Range}
         child className      : {Name}
         child fieldName      : {Name}
         child defaultName    : {Name}
      alternative ExcludeIfTarget:
         child range          : {Range}
         child targetNames    : {[String]}
         visit 0:
            local targets     : _
      alternative Language:
         child range          : {Range}
         child pragmas        : {Names}
         visit 0:
            local pragmaNames : _
      alternative OptionsGHC:
         child range          : {Range}
         child options        : {Names}
         visit 0:
            local pragmaNames : _
-}
-- cata
sem_Pragma :: Pragma  ->
              T_Pragma 
sem_Pragma (Pragma_Derivable _range _className _fieldName _defaultName )  =
    (sem_Pragma_Derivable _range _className _fieldName _defaultName )
sem_Pragma (Pragma_ExcludeIfTarget _range _targetNames )  =
    (sem_Pragma_ExcludeIfTarget _range _targetNames )
sem_Pragma (Pragma_Language _range _pragmas )  =
    (sem_Pragma_Language _range _pragmas )
sem_Pragma (Pragma_OptionsGHC _range _options )  =
    (sem_Pragma_OptionsGHC _range _options )
-- semantic domain
type T_Pragma  = UID ->
                 NmLev ->
                 ([HsName]) ->
                 ( UID,(Set.Set Pragma.Pragma),([HsName]))
sem_Pragma_Derivable :: Range ->
                        Name ->
                        Name ->
                        Name ->
                        T_Pragma 
sem_Pragma_Derivable range_ className_ fieldName_ defaultName_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 14, column 9)
              _lhsOgathPragmas =
                  Set.singleton (Pragma.Pragma_Derivable className_ fieldName_ defaultName_)
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOtopInstanceNmL)))
sem_Pragma_ExcludeIfTarget :: Range ->
                              ([String]) ->
                              T_Pragma 
sem_Pragma_ExcludeIfTarget range_ targetNames_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 16, column 9)
              _targets =
                  catMaybes $ map (\t -> Map.lookup t supportedTargetMp) targetNames_
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 17, column 9)
              _lhsOgathPragmas =
                  Set.singleton (Pragma.Pragma_ExcludeIfTarget _targets)
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOtopInstanceNmL)))
sem_Pragma_Language :: Range ->
                       Names ->
                       T_Pragma 
sem_Pragma_Language range_ pragmas_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 6, column 9)
              _pragmaNames =
                  [ p |      p  <- map show pragmas_ ]
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 12, column 9)
              _lhsOgathPragmas =
                  Set.fromList $ catMaybes [ Map.lookup p Pragma.allSimplePragmaMp | p <- _pragmaNames ]
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOtopInstanceNmL)))
sem_Pragma_OptionsGHC :: Range ->
                         Names ->
                         T_Pragma 
sem_Pragma_OptionsGHC range_ options_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 8, column 9)
              _pragmaNames =
                  []
              -- "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 12, column 9)
              _lhsOgathPragmas =
                  Set.fromList $ catMaybes [ Map.lookup p Pragma.allSimplePragmaMp | p <- _pragmaNames ]
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOtopInstanceNmL)))
-- Pragmas -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         gathPragmas          : Set.Set Pragma.Pragma
   alternatives:
      alternative Cons:
         child hd             : Pragma 
         child tl             : Pragmas 
      alternative Nil:
-}
-- cata
sem_Pragmas :: Pragmas  ->
               T_Pragmas 
sem_Pragmas list  =
    (Prelude.foldr sem_Pragmas_Cons sem_Pragmas_Nil (Prelude.map sem_Pragma list) )
-- semantic domain
type T_Pragmas  = UID ->
                  NmLev ->
                  ([HsName]) ->
                  ( UID,(Set.Set Pragma.Pragma),([HsName]))
sem_Pragmas_Cons :: T_Pragma  ->
                    T_Pragmas  ->
                    T_Pragmas 
sem_Pragmas_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIgathPragmas :: (Set.Set Pragma.Pragma)
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIgathPragmas :: (Set.Set Pragma.Pragma)
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  _hdIgathPragmas `Set.union` _tlIgathPragmas
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIgathPragmas,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIgathPragmas,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOtopInstanceNmL)))
sem_Pragmas_Nil :: T_Pragmas 
sem_Pragmas_Nil  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgathPragmas :: (Set.Set Pragma.Pragma)
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/Pragmas.ag"(line 1, column 49)
              _lhsOgathPragmas =
                  Set.empty
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOgathPragmas,_lhsOtopInstanceNmL)))
-- Qualifier ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         nmLev                : NmLev
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Empty:
         child range          : {Range}
      alternative Generator:
         child range          : {Range}
         child pattern        : Pattern 
         child expression     : Expression 
         visit 0:
            local nmLev       : _
      alternative Guard:
         child range          : {Range}
         child guard          : Expression 
      alternative Let:
         child range          : {Range}
         child declarations   : Declarations 
         visit 0:
            local nmLev       : _
-}
-- cata
sem_Qualifier :: Qualifier  ->
                 T_Qualifier 
sem_Qualifier (Qualifier_Empty _range )  =
    (sem_Qualifier_Empty _range )
sem_Qualifier (Qualifier_Generator _range _pattern _expression )  =
    (sem_Qualifier_Generator _range (sem_Pattern _pattern ) (sem_Expression _expression ) )
sem_Qualifier (Qualifier_Guard _range _guard )  =
    (sem_Qualifier_Guard _range (sem_Expression _guard ) )
sem_Qualifier (Qualifier_Let _range _declarations )  =
    (sem_Qualifier_Let _range (sem_Declarations _declarations ) )
-- semantic domain
type T_Qualifier  = UID ->
                    HsName ->
                    NmLev ->
                    EHCOpts ->
                    ([HsName]) ->
                    ( UID,NmLev,([HsName]))
sem_Qualifier_Empty :: Range ->
                       T_Qualifier 
sem_Qualifier_Empty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
sem_Qualifier_Generator :: Range ->
                           T_Pattern  ->
                           T_Expression  ->
                           T_Qualifier 
sem_Qualifier_Generator range_ pattern_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 77, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (from local)
              _lhsOnmLev =
                  _nmLev
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _patternOnmLev =
                  _nmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _patternIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _expressionOnmLev =
                  _nmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
sem_Qualifier_Guard :: Range ->
                       T_Expression  ->
                       T_Qualifier 
sem_Qualifier_Guard range_ guard_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              _guardOgUniq :: UID
              _guardOmoduleNm :: HsName
              _guardOnmLev :: NmLev
              _guardOopts :: EHCOpts
              _guardOtopInstanceNmL :: ([HsName])
              _guardIconNm :: Name
              _guardIgUniq :: UID
              _guardItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _guardIgUniq
              -- copy rule (chain)
              _lhsOnmLev =
                  _lhsInmLev
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _guardItopInstanceNmL
              -- copy rule (down)
              _guardOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _guardOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _guardOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _guardOopts =
                  _lhsIopts
              -- copy rule (down)
              _guardOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _guardIconNm,_guardIgUniq,_guardItopInstanceNmL) | True =
                  guard_ _guardOgUniq _guardOmoduleNm _guardOnmLev _guardOopts _guardOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
sem_Qualifier_Let :: Range ->
                     T_Declarations  ->
                     T_Qualifier 
sem_Qualifier_Let range_ declarations_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              _declarationsOgUniq :: UID
              _declarationsOmoduleNm :: HsName
              _declarationsOnmLev :: NmLev
              _declarationsOopts :: EHCOpts
              _declarationsOtopInstanceNmL :: ([HsName])
              _declarationsIgUniq :: UID
              _declarationsIgathPragmas :: (Set.Set Pragma.Pragma)
              _declarationsIidOccDefs :: ([IdOcc])
              _declarationsImainValExists :: Bool
              _declarationsImodDefsRel :: ModEntRel
              _declarationsImodHideDefsRel :: ModEntRel
              _declarationsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 77, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _declarationsIgUniq
              -- copy rule (from local)
              _lhsOnmLev =
                  _nmLev
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _declarationsItopInstanceNmL
              -- copy rule (down)
              _declarationsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _declarationsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _declarationsOnmLev =
                  _nmLev
              -- copy rule (down)
              _declarationsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declarationsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _declarationsIgUniq,_declarationsIgathPragmas,_declarationsIidOccDefs,_declarationsImainValExists,_declarationsImodDefsRel,_declarationsImodHideDefsRel,_declarationsItopInstanceNmL) | True =
                  declarations_ _declarationsOgUniq _declarationsOmoduleNm _declarationsOnmLev _declarationsOopts _declarationsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
-- Qualifiers --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : Qualifier 
         child tl             : Qualifiers 
      alternative Nil:
-}
-- cata
sem_Qualifiers :: Qualifiers  ->
                  T_Qualifiers 
sem_Qualifiers list  =
    (Prelude.foldr sem_Qualifiers_Cons sem_Qualifiers_Nil (Prelude.map sem_Qualifier list) )
-- semantic domain
type T_Qualifiers  = UID ->
                     HsName ->
                     NmLev ->
                     EHCOpts ->
                     ([HsName]) ->
                     ( UID,([HsName]))
sem_Qualifiers_Cons :: T_Qualifier  ->
                       T_Qualifiers  ->
                       T_Qualifiers 
sem_Qualifiers_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdInmLev :: NmLev
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (chain)
              _tlOnmLev =
                  _hdInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdInmLev,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Qualifiers_Nil :: T_Qualifiers 
sem_Qualifiers_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RecordExpressionBinding -------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Binding:
         child range          : {Range}
         child name           : {Name}
         child expression     : Expression 
-}
-- cata
sem_RecordExpressionBinding :: RecordExpressionBinding  ->
                               T_RecordExpressionBinding 
sem_RecordExpressionBinding (RecordExpressionBinding_Binding _range _name _expression )  =
    (sem_RecordExpressionBinding_Binding _range _name (sem_Expression _expression ) )
-- semantic domain
type T_RecordExpressionBinding  = UID ->
                                  HsName ->
                                  NmLev ->
                                  EHCOpts ->
                                  ([HsName]) ->
                                  ( UID,([HsName]))
sem_RecordExpressionBinding_Binding :: Range ->
                                       Name ->
                                       T_Expression  ->
                                       T_RecordExpressionBinding 
sem_RecordExpressionBinding_Binding range_ name_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RecordExpressionBindings ------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : RecordExpressionBinding 
         child tl             : RecordExpressionBindings 
      alternative Nil:
-}
-- cata
sem_RecordExpressionBindings :: RecordExpressionBindings  ->
                                T_RecordExpressionBindings 
sem_RecordExpressionBindings list  =
    (Prelude.foldr sem_RecordExpressionBindings_Cons sem_RecordExpressionBindings_Nil (Prelude.map sem_RecordExpressionBinding list) )
-- semantic domain
type T_RecordExpressionBindings  = UID ->
                                   HsName ->
                                   NmLev ->
                                   EHCOpts ->
                                   ([HsName]) ->
                                   ( UID,([HsName]))
sem_RecordExpressionBindings_Cons :: T_RecordExpressionBinding  ->
                                     T_RecordExpressionBindings  ->
                                     T_RecordExpressionBindings 
sem_RecordExpressionBindings_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_RecordExpressionBindings_Nil :: T_RecordExpressionBindings 
sem_RecordExpressionBindings_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RecordPatternBinding ----------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Binding:
         child range          : {Range}
         child name           : {Name}
         child pattern        : Pattern 
      alternative Pun:
         child range          : {Range}
         child name           : {Name}
         visit 0:
            local refname     : _
            local idOccDef    : _
-}
-- cata
sem_RecordPatternBinding :: RecordPatternBinding  ->
                            T_RecordPatternBinding 
sem_RecordPatternBinding (RecordPatternBinding_Binding _range _name _pattern )  =
    (sem_RecordPatternBinding_Binding _range _name (sem_Pattern _pattern ) )
sem_RecordPatternBinding (RecordPatternBinding_Pun _range _name )  =
    (sem_RecordPatternBinding_Pun _range _name )
-- semantic domain
type T_RecordPatternBinding  = UID ->
                               HsName ->
                               NmLev ->
                               ([HsName]) ->
                               ( UID,([IdOcc]),([HsName]))
sem_RecordPatternBinding_Binding :: Range ->
                                    Name ->
                                    T_Pattern  ->
                                    T_RecordPatternBinding 
sem_RecordPatternBinding_Binding range_ name_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
sem_RecordPatternBinding_Pun :: Range ->
                                Name ->
                                T_RecordPatternBinding 
sem_RecordPatternBinding_Pun range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 92, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 184, column 9)
              _idOccDef =
                  IdOcc _refname IdOcc_Val
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- RecordPatternBindings ---------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Cons:
         child hd             : RecordPatternBinding 
         child tl             : RecordPatternBindings 
      alternative Nil:
-}
-- cata
sem_RecordPatternBindings :: RecordPatternBindings  ->
                             T_RecordPatternBindings 
sem_RecordPatternBindings list  =
    (Prelude.foldr sem_RecordPatternBindings_Cons sem_RecordPatternBindings_Nil (Prelude.map sem_RecordPatternBinding list) )
-- semantic domain
type T_RecordPatternBindings  = UID ->
                                HsName ->
                                NmLev ->
                                ([HsName]) ->
                                ( UID,([IdOcc]),([HsName]))
sem_RecordPatternBindings_Cons :: T_RecordPatternBinding  ->
                                  T_RecordPatternBindings  ->
                                  T_RecordPatternBindings 
sem_RecordPatternBindings_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIidOccDefs :: ([IdOcc])
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIidOccDefs :: ([IdOcc])
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIidOccDefs,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIidOccDefs,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
sem_RecordPatternBindings_Nil :: T_RecordPatternBindings 
sem_RecordPatternBindings_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- RightHandSide -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Expression:
         child range          : {Range}
         child expression     : Expression 
         child where          : MaybeDeclarations 
         visit 0:
            local nmLev       : _
      alternative Guarded:
         child range          : {Range}
         child guardedexpressions : GuardedExpressions 
         child where          : MaybeDeclarations 
         visit 0:
            local nmLev       : _
-}
-- cata
sem_RightHandSide :: RightHandSide  ->
                     T_RightHandSide 
sem_RightHandSide (RightHandSide_Expression _range _expression _where )  =
    (sem_RightHandSide_Expression _range (sem_Expression _expression ) (sem_MaybeDeclarations _where ) )
sem_RightHandSide (RightHandSide_Guarded _range _guardedexpressions _where )  =
    (sem_RightHandSide_Guarded _range (sem_GuardedExpressions _guardedexpressions ) (sem_MaybeDeclarations _where ) )
-- semantic domain
type T_RightHandSide  = UID ->
                        HsName ->
                        NmLev ->
                        EHCOpts ->
                        ([HsName]) ->
                        ( UID,([HsName]))
sem_RightHandSide_Expression :: Range ->
                                T_Expression  ->
                                T_MaybeDeclarations  ->
                                T_RightHandSide 
sem_RightHandSide_Expression range_ expression_ where_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _whereOgUniq :: UID
              _whereOmoduleNm :: HsName
              _whereOnmLev :: NmLev
              _whereOopts :: EHCOpts
              _whereOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              _whereIgUniq :: UID
              _whereIgathPragmas :: (Set.Set Pragma.Pragma)
              _whereIidOccDefs :: ([IdOcc])
              _whereImainValExists :: Bool
              _whereItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 69, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _whereIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _whereItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _expressionOnmLev =
                  _nmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _whereOgUniq =
                  _expressionIgUniq
              -- copy rule (down)
              _whereOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _whereOnmLev =
                  _nmLev
              -- copy rule (down)
              _whereOopts =
                  _lhsIopts
              -- copy rule (chain)
              _whereOtopInstanceNmL =
                  _expressionItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
              ( _whereIgUniq,_whereIgathPragmas,_whereIidOccDefs,_whereImainValExists,_whereItopInstanceNmL) | True =
                  where_ _whereOgUniq _whereOmoduleNm _whereOnmLev _whereOopts _whereOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_RightHandSide_Guarded :: Range ->
                             T_GuardedExpressions  ->
                             T_MaybeDeclarations  ->
                             T_RightHandSide 
sem_RightHandSide_Guarded range_ guardedexpressions_ where_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _guardedexpressionsOgUniq :: UID
              _guardedexpressionsOmoduleNm :: HsName
              _guardedexpressionsOnmLev :: NmLev
              _guardedexpressionsOopts :: EHCOpts
              _guardedexpressionsOtopInstanceNmL :: ([HsName])
              _whereOgUniq :: UID
              _whereOmoduleNm :: HsName
              _whereOnmLev :: NmLev
              _whereOopts :: EHCOpts
              _whereOtopInstanceNmL :: ([HsName])
              _guardedexpressionsIgUniq :: UID
              _guardedexpressionsItopInstanceNmL :: ([HsName])
              _whereIgUniq :: UID
              _whereIgathPragmas :: (Set.Set Pragma.Pragma)
              _whereIidOccDefs :: ([IdOcc])
              _whereImainValExists :: Bool
              _whereItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 69, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _whereIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _whereItopInstanceNmL
              -- copy rule (down)
              _guardedexpressionsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _guardedexpressionsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _guardedexpressionsOnmLev =
                  _nmLev
              -- copy rule (down)
              _guardedexpressionsOopts =
                  _lhsIopts
              -- copy rule (down)
              _guardedexpressionsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _whereOgUniq =
                  _guardedexpressionsIgUniq
              -- copy rule (down)
              _whereOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _whereOnmLev =
                  _nmLev
              -- copy rule (down)
              _whereOopts =
                  _lhsIopts
              -- copy rule (chain)
              _whereOtopInstanceNmL =
                  _guardedexpressionsItopInstanceNmL
              ( _guardedexpressionsIgUniq,_guardedexpressionsItopInstanceNmL) | True =
                  guardedexpressions_ _guardedexpressionsOgUniq _guardedexpressionsOmoduleNm _guardedexpressionsOnmLev _guardedexpressionsOopts _guardedexpressionsOtopInstanceNmL 
              ( _whereIgUniq,_whereIgathPragmas,_whereIidOccDefs,_whereImainValExists,_whereItopInstanceNmL) | True =
                  where_ _whereOgUniq _whereOmoduleNm _whereOnmLev _whereOopts _whereOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RowRecordExpressionUpdate -----------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Extends:
         child range          : {Range}
         child name           : {MaybeName}
         child expression     : Expression 
      alternative Update:
         child range          : {Range}
         child name           : {Name}
         child expression     : Expression 
-}
-- cata
sem_RowRecordExpressionUpdate :: RowRecordExpressionUpdate  ->
                                 T_RowRecordExpressionUpdate 
sem_RowRecordExpressionUpdate (RowRecordExpressionUpdate_Extends _range _name _expression )  =
    (sem_RowRecordExpressionUpdate_Extends _range _name (sem_Expression _expression ) )
sem_RowRecordExpressionUpdate (RowRecordExpressionUpdate_Update _range _name _expression )  =
    (sem_RowRecordExpressionUpdate_Update _range _name (sem_Expression _expression ) )
-- semantic domain
type T_RowRecordExpressionUpdate  = UID ->
                                    HsName ->
                                    NmLev ->
                                    EHCOpts ->
                                    ([HsName]) ->
                                    ( UID,([HsName]))
sem_RowRecordExpressionUpdate_Extends :: Range ->
                                         MaybeName ->
                                         T_Expression  ->
                                         T_RowRecordExpressionUpdate 
sem_RowRecordExpressionUpdate_Extends range_ name_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_RowRecordExpressionUpdate_Update :: Range ->
                                        Name ->
                                        T_Expression  ->
                                        T_RowRecordExpressionUpdate 
sem_RowRecordExpressionUpdate_Update range_ name_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RowRecordExpressionUpdates ----------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : RowRecordExpressionUpdate 
         child tl             : RowRecordExpressionUpdates 
      alternative Nil:
-}
-- cata
sem_RowRecordExpressionUpdates :: RowRecordExpressionUpdates  ->
                                  T_RowRecordExpressionUpdates 
sem_RowRecordExpressionUpdates list  =
    (Prelude.foldr sem_RowRecordExpressionUpdates_Cons sem_RowRecordExpressionUpdates_Nil (Prelude.map sem_RowRecordExpressionUpdate list) )
-- semantic domain
type T_RowRecordExpressionUpdates  = UID ->
                                     HsName ->
                                     NmLev ->
                                     EHCOpts ->
                                     ([HsName]) ->
                                     ( UID,([HsName]))
sem_RowRecordExpressionUpdates_Cons :: T_RowRecordExpressionUpdate  ->
                                       T_RowRecordExpressionUpdates  ->
                                       T_RowRecordExpressionUpdates 
sem_RowRecordExpressionUpdates_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_RowRecordExpressionUpdates_Nil :: T_RowRecordExpressionUpdates 
sem_RowRecordExpressionUpdates_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RowRecordPatternBinding -------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Binding:
         child range          : {Range}
         child name           : {MaybeName}
         child pattern        : Pattern 
-}
-- cata
sem_RowRecordPatternBinding :: RowRecordPatternBinding  ->
                               T_RowRecordPatternBinding 
sem_RowRecordPatternBinding (RowRecordPatternBinding_Binding _range _name _pattern )  =
    (sem_RowRecordPatternBinding_Binding _range _name (sem_Pattern _pattern ) )
-- semantic domain
type T_RowRecordPatternBinding  = UID ->
                                  HsName ->
                                  NmLev ->
                                  ([HsName]) ->
                                  ( UID,([IdOcc]),([HsName]))
sem_RowRecordPatternBinding_Binding :: Range ->
                                       MaybeName ->
                                       T_Pattern  ->
                                       T_RowRecordPatternBinding 
sem_RowRecordPatternBinding_Binding range_ name_ pattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _patternIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _patternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- RowRecordPatternBindings ------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         idOccDefs            : [IdOcc]
   alternatives:
      alternative Cons:
         child hd             : RowRecordPatternBinding 
         child tl             : RowRecordPatternBindings 
      alternative Nil:
-}
-- cata
sem_RowRecordPatternBindings :: RowRecordPatternBindings  ->
                                T_RowRecordPatternBindings 
sem_RowRecordPatternBindings list  =
    (Prelude.foldr sem_RowRecordPatternBindings_Cons sem_RowRecordPatternBindings_Nil (Prelude.map sem_RowRecordPatternBinding list) )
-- semantic domain
type T_RowRecordPatternBindings  = UID ->
                                   HsName ->
                                   NmLev ->
                                   ([HsName]) ->
                                   ( UID,([IdOcc]),([HsName]))
sem_RowRecordPatternBindings_Cons :: T_RowRecordPatternBinding  ->
                                     T_RowRecordPatternBindings  ->
                                     T_RowRecordPatternBindings 
sem_RowRecordPatternBindings_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdIidOccDefs :: ([IdOcc])
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlIidOccDefs :: ([IdOcc])
              _tlItopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _hdIidOccDefs ++ _tlIidOccDefs
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdIidOccDefs,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlIidOccDefs,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
sem_RowRecordPatternBindings_Nil :: T_RowRecordPatternBindings 
sem_RowRecordPatternBindings_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOtopInstanceNmL)))
-- RowTypeUpdate -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Extends:
         child range          : {Range}
         child name           : {MaybeName}
         child type           : Type 
-}
-- cata
sem_RowTypeUpdate :: RowTypeUpdate  ->
                     T_RowTypeUpdate 
sem_RowTypeUpdate (RowTypeUpdate_Extends _range _name _type )  =
    (sem_RowTypeUpdate_Extends _range _name (sem_Type _type ) )
-- semantic domain
type T_RowTypeUpdate  = UID ->
                        NmLev ->
                        ([HsName]) ->
                        ( UID,([HsName]))
sem_RowTypeUpdate_Extends :: Range ->
                             MaybeName ->
                             T_Type  ->
                             T_RowTypeUpdate 
sem_RowTypeUpdate_Extends range_ name_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- RowTypeUpdates ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : RowTypeUpdate 
         child tl             : RowTypeUpdates 
      alternative Nil:
-}
-- cata
sem_RowTypeUpdates :: RowTypeUpdates  ->
                      T_RowTypeUpdates 
sem_RowTypeUpdates list  =
    (Prelude.foldr sem_RowTypeUpdates_Cons sem_RowTypeUpdates_Nil (Prelude.map sem_RowTypeUpdate list) )
-- semantic domain
type T_RowTypeUpdates  = UID ->
                         NmLev ->
                         ([HsName]) ->
                         ( UID,([HsName]))
sem_RowTypeUpdates_Cons :: T_RowTypeUpdate  ->
                           T_RowTypeUpdates  ->
                           T_RowTypeUpdates 
sem_RowTypeUpdates_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_RowTypeUpdates_Nil :: T_RowTypeUpdates 
sem_RowTypeUpdates_Nil  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Statement ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         nmLev                : NmLev
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Empty:
         child range          : {Range}
      alternative Expression:
         child range          : {Range}
         child expression     : Expression 
      alternative Generator:
         child range          : {Range}
         child pattern        : Pattern 
         child expression     : Expression 
         visit 0:
            local nmLev       : _
      alternative Let:
         child range          : {Range}
         child declarations   : Declarations 
         visit 0:
            local nmLev       : _
-}
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (Statement_Empty _range )  =
    (sem_Statement_Empty _range )
sem_Statement (Statement_Expression _range _expression )  =
    (sem_Statement_Expression _range (sem_Expression _expression ) )
sem_Statement (Statement_Generator _range _pattern _expression )  =
    (sem_Statement_Generator _range (sem_Pattern _pattern ) (sem_Expression _expression ) )
sem_Statement (Statement_Let _range _declarations )  =
    (sem_Statement_Let _range (sem_Declarations _declarations ) )
-- semantic domain
type T_Statement  = UID ->
                    HsName ->
                    NmLev ->
                    EHCOpts ->
                    ([HsName]) ->
                    ( UID,NmLev,([HsName]))
sem_Statement_Empty :: Range ->
                       T_Statement 
sem_Statement_Empty range_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
sem_Statement_Expression :: Range ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Expression range_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (chain)
              _lhsOnmLev =
                  _lhsInmLev
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _expressionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _expressionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (down)
              _expressionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
sem_Statement_Generator :: Range ->
                           T_Pattern  ->
                           T_Expression  ->
                           T_Statement 
sem_Statement_Generator range_ pattern_ expression_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              _patternOgUniq :: UID
              _patternOmoduleNm :: HsName
              _patternOnmLev :: NmLev
              _patternOtopInstanceNmL :: ([HsName])
              _expressionOgUniq :: UID
              _expressionOmoduleNm :: HsName
              _expressionOnmLev :: NmLev
              _expressionOopts :: EHCOpts
              _expressionOtopInstanceNmL :: ([HsName])
              _patternIgUniq :: UID
              _patternIidOccDefs :: ([IdOcc])
              _patternImainValExists :: Bool
              _patternImbTopRefname :: (Maybe HsName)
              _patternItopInstanceNmL :: ([HsName])
              _expressionIconNm :: Name
              _expressionIgUniq :: UID
              _expressionItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 81, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _expressionIgUniq
              -- copy rule (from local)
              _lhsOnmLev =
                  _nmLev
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _expressionItopInstanceNmL
              -- copy rule (down)
              _patternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _patternOnmLev =
                  _nmLev
              -- copy rule (down)
              _patternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _expressionOgUniq =
                  _patternIgUniq
              -- copy rule (down)
              _expressionOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _expressionOnmLev =
                  _nmLev
              -- copy rule (down)
              _expressionOopts =
                  _lhsIopts
              -- copy rule (chain)
              _expressionOtopInstanceNmL =
                  _patternItopInstanceNmL
              ( _patternIgUniq,_patternIidOccDefs,_patternImainValExists,_patternImbTopRefname,_patternItopInstanceNmL) | True =
                  pattern_ _patternOgUniq _patternOmoduleNm _patternOnmLev _patternOtopInstanceNmL 
              ( _expressionIconNm,_expressionIgUniq,_expressionItopInstanceNmL) | True =
                  expression_ _expressionOgUniq _expressionOmoduleNm _expressionOnmLev _expressionOopts _expressionOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
sem_Statement_Let :: Range ->
                     T_Declarations  ->
                     T_Statement 
sem_Statement_Let range_ declarations_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOnmLev :: NmLev
              _lhsOtopInstanceNmL :: ([HsName])
              _declarationsOgUniq :: UID
              _declarationsOmoduleNm :: HsName
              _declarationsOnmLev :: NmLev
              _declarationsOopts :: EHCOpts
              _declarationsOtopInstanceNmL :: ([HsName])
              _declarationsIgUniq :: UID
              _declarationsIgathPragmas :: (Set.Set Pragma.Pragma)
              _declarationsIidOccDefs :: ([IdOcc])
              _declarationsImainValExists :: Bool
              _declarationsImodDefsRel :: ModEntRel
              _declarationsImodHideDefsRel :: ModEntRel
              _declarationsItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 81, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- copy rule (up)
              _lhsOgUniq =
                  _declarationsIgUniq
              -- copy rule (from local)
              _lhsOnmLev =
                  _nmLev
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _declarationsItopInstanceNmL
              -- copy rule (down)
              _declarationsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _declarationsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (from local)
              _declarationsOnmLev =
                  _nmLev
              -- copy rule (down)
              _declarationsOopts =
                  _lhsIopts
              -- copy rule (down)
              _declarationsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _declarationsIgUniq,_declarationsIgathPragmas,_declarationsIidOccDefs,_declarationsImainValExists,_declarationsImodDefsRel,_declarationsImodHideDefsRel,_declarationsItopInstanceNmL) | True =
                  declarations_ _declarationsOgUniq _declarationsOmoduleNm _declarationsOnmLev _declarationsOopts _declarationsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOnmLev,_lhsOtopInstanceNmL)))
-- Statements --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
         opts                 : EHCOpts
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : Statement 
         child tl             : Statements 
      alternative Nil:
-}
-- cata
sem_Statements :: Statements  ->
                  T_Statements 
sem_Statements list  =
    (Prelude.foldr sem_Statements_Cons sem_Statements_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_Statements  = UID ->
                     HsName ->
                     NmLev ->
                     EHCOpts ->
                     ([HsName]) ->
                     ( UID,([HsName]))
sem_Statements_Cons :: T_Statement  ->
                       T_Statements  ->
                       T_Statements 
sem_Statements_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOopts :: EHCOpts
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOopts :: EHCOpts
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdInmLev :: NmLev
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (chain)
              _tlOnmLev =
                  _hdInmLev
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdInmLev,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOopts _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOopts _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Statements_Nil :: T_Statements 
sem_Statements_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsIopts
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Strings -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : {String}
         child tl             : Strings 
      alternative Nil:
-}
-- cata
sem_Strings :: Strings  ->
               T_Strings 
sem_Strings list  =
    (Prelude.foldr sem_Strings_Cons sem_Strings_Nil list )
-- semantic domain
type T_Strings  = UID ->
                  NmLev ->
                  ([HsName]) ->
                  ( UID,([HsName]))
sem_Strings_Cons :: String ->
                    T_Strings  ->
                    T_Strings 
sem_Strings_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _tlOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _tlOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Strings_Nil :: T_Strings 
sem_Strings_Nil  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- Type --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         conNm                : Name
   alternatives:
      alternative Annotate:
         child range          : {Range}
         child annotation     : TypeAnnotation 
         child type           : Type 
      alternative Constructor:
         child range          : {Range}
         child name           : {Name}
      alternative Exists:
         child range          : {Range}
         child typevariables  : {Names}
         child type           : Type 
         visit 0:
            local nmLev       : _
      alternative Forall:
         child range          : {Range}
         child typevariables  : {Names}
         child type           : Type 
         visit 0:
            local nmLev       : _
      alternative InfixApplication:
         child range          : {Range}
         child leftType       : Type 
         child operator       : Type 
         child rightType      : Type 
      alternative InfixApplicationChainTop:
         child range          : {Range}
         child type           : Type 
      alternative MonoWildcard:
         child range          : {Range}
      alternative NamedWildcard:
         child range          : {Range}
         child name           : {Name}
      alternative NormalApplication:
         child range          : {Range}
         child function       : Type 
         child arguments      : Types 
      alternative Parenthesized:
         child range          : {Range}
         child type           : Type 
      alternative Qualified:
         child range          : {Range}
         child context        : ContextItems 
         child type           : Type 
      alternative RowEmpty:
         child range          : {Range}
      alternative RowRecEmpty:
         child range          : {Range}
      alternative RowRecUpdate:
         child range          : {Range}
         child type           : Type 
         child rowTypeUpdates : RowTypeUpdates 
      alternative RowSumEmpty:
         child range          : {Range}
      alternative RowSumUpdate:
         child range          : {Range}
         child type           : Type 
         child rowTypeUpdates : RowTypeUpdates 
      alternative RowUpdate:
         child range          : {Range}
         child type           : Type 
         child rowTypeUpdates : RowTypeUpdates 
      alternative SectionApplication:
         child range          : {Range}
         child leftType       : MaybeType 
         child operator       : Type 
         child rightType      : MaybeType 
      alternative TupleConstructor:
         child range          : {Range}
         child arity          : {Int}
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
      alternative Wildcard:
         child range          : {Range}
-}
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (Type_Annotate _range _annotation _type )  =
    (sem_Type_Annotate _range (sem_TypeAnnotation _annotation ) (sem_Type _type ) )
sem_Type (Type_Constructor _range _name )  =
    (sem_Type_Constructor _range _name )
sem_Type (Type_Exists _range _typevariables _type )  =
    (sem_Type_Exists _range _typevariables (sem_Type _type ) )
sem_Type (Type_Forall _range _typevariables _type )  =
    (sem_Type_Forall _range _typevariables (sem_Type _type ) )
sem_Type (Type_InfixApplication _range _leftType _operator _rightType )  =
    (sem_Type_InfixApplication _range (sem_Type _leftType ) (sem_Type _operator ) (sem_Type _rightType ) )
sem_Type (Type_InfixApplicationChainTop _range _type )  =
    (sem_Type_InfixApplicationChainTop _range (sem_Type _type ) )
sem_Type (Type_MonoWildcard _range )  =
    (sem_Type_MonoWildcard _range )
sem_Type (Type_NamedWildcard _range _name )  =
    (sem_Type_NamedWildcard _range _name )
sem_Type (Type_NormalApplication _range _function _arguments )  =
    (sem_Type_NormalApplication _range (sem_Type _function ) (sem_Types _arguments ) )
sem_Type (Type_Parenthesized _range _type )  =
    (sem_Type_Parenthesized _range (sem_Type _type ) )
sem_Type (Type_Qualified _range _context _type )  =
    (sem_Type_Qualified _range (sem_ContextItems _context ) (sem_Type _type ) )
sem_Type (Type_RowEmpty _range )  =
    (sem_Type_RowEmpty _range )
sem_Type (Type_RowRecEmpty _range )  =
    (sem_Type_RowRecEmpty _range )
sem_Type (Type_RowRecUpdate _range _type _rowTypeUpdates )  =
    (sem_Type_RowRecUpdate _range (sem_Type _type ) (sem_RowTypeUpdates _rowTypeUpdates ) )
sem_Type (Type_RowSumEmpty _range )  =
    (sem_Type_RowSumEmpty _range )
sem_Type (Type_RowSumUpdate _range _type _rowTypeUpdates )  =
    (sem_Type_RowSumUpdate _range (sem_Type _type ) (sem_RowTypeUpdates _rowTypeUpdates ) )
sem_Type (Type_RowUpdate _range _type _rowTypeUpdates )  =
    (sem_Type_RowUpdate _range (sem_Type _type ) (sem_RowTypeUpdates _rowTypeUpdates ) )
sem_Type (Type_SectionApplication _range _leftType _operator _rightType )  =
    (sem_Type_SectionApplication _range (sem_MaybeType _leftType ) (sem_Type _operator ) (sem_MaybeType _rightType ) )
sem_Type (Type_TupleConstructor _range _arity )  =
    (sem_Type_TupleConstructor _range _arity )
sem_Type (Type_Variable _range _name )  =
    (sem_Type_Variable _range _name )
sem_Type (Type_Wildcard _range )  =
    (sem_Type_Wildcard _range )
-- semantic domain
type T_Type  = UID ->
               NmLev ->
               ([HsName]) ->
               ( Name,UID,([HsName]))
sem_Type_Annotate :: Range ->
                     T_TypeAnnotation  ->
                     T_Type  ->
                     T_Type 
sem_Type_Annotate range_ annotation_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _annotationOgUniq :: UID
              _annotationOnmLev :: NmLev
              _annotationOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _annotationIgUniq :: UID
              _annotationItopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _annotationOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _annotationOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _annotationOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _annotationIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _annotationItopInstanceNmL
              ( _annotationIgUniq,_annotationItopInstanceNmL) | True =
                  annotation_ _annotationOgUniq _annotationOnmLev _annotationOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Constructor :: Range ->
                        Name ->
                        T_Type 
sem_Type_Constructor range_ name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 255, column 9)
              _lhsOconNm =
                  name_
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Exists :: Range ->
                   Names ->
                   T_Type  ->
                   T_Type 
sem_Type_Exists range_ typevariables_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 33, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Forall :: Range ->
                   Names ->
                   T_Type  ->
                   T_Type 
sem_Type_Forall range_ typevariables_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameLevel.ag"(line 33, column 9)
              _nmLev =
                  _lhsInmLev + 1
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _typeOnmLev =
                  _nmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_InfixApplication :: Range ->
                             T_Type  ->
                             T_Type  ->
                             T_Type  ->
                             T_Type 
sem_Type_InfixApplication range_ leftType_ operator_ rightType_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftTypeOgUniq :: UID
              _leftTypeOnmLev :: NmLev
              _leftTypeOtopInstanceNmL :: ([HsName])
              _operatorOgUniq :: UID
              _operatorOnmLev :: NmLev
              _operatorOtopInstanceNmL :: ([HsName])
              _rightTypeOgUniq :: UID
              _rightTypeOnmLev :: NmLev
              _rightTypeOtopInstanceNmL :: ([HsName])
              _leftTypeIconNm :: Name
              _leftTypeIgUniq :: UID
              _leftTypeItopInstanceNmL :: ([HsName])
              _operatorIconNm :: Name
              _operatorIgUniq :: UID
              _operatorItopInstanceNmL :: ([HsName])
              _rightTypeIconNm :: Name
              _rightTypeIgUniq :: UID
              _rightTypeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 257, column 9)
              _lhsOconNm =
                  _operatorIconNm
              -- copy rule (up)
              _lhsOgUniq =
                  _rightTypeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightTypeItopInstanceNmL
              -- copy rule (down)
              _leftTypeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftTypeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftTypeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _operatorOgUniq =
                  _leftTypeIgUniq
              -- copy rule (down)
              _operatorOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _operatorOtopInstanceNmL =
                  _leftTypeItopInstanceNmL
              -- copy rule (chain)
              _rightTypeOgUniq =
                  _operatorIgUniq
              -- copy rule (down)
              _rightTypeOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rightTypeOtopInstanceNmL =
                  _operatorItopInstanceNmL
              ( _leftTypeIconNm,_leftTypeIgUniq,_leftTypeItopInstanceNmL) | True =
                  leftType_ _leftTypeOgUniq _leftTypeOnmLev _leftTypeOtopInstanceNmL 
              ( _operatorIconNm,_operatorIgUniq,_operatorItopInstanceNmL) | True =
                  operator_ _operatorOgUniq _operatorOnmLev _operatorOtopInstanceNmL 
              ( _rightTypeIconNm,_rightTypeIgUniq,_rightTypeItopInstanceNmL) | True =
                  rightType_ _rightTypeOgUniq _rightTypeOnmLev _rightTypeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_InfixApplicationChainTop :: Range ->
                                     T_Type  ->
                                     T_Type 
sem_Type_InfixApplicationChainTop range_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOconNm =
                  _typeIconNm
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_MonoWildcard :: Range ->
                         T_Type 
sem_Type_MonoWildcard range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_NamedWildcard :: Range ->
                          Name ->
                          T_Type 
sem_Type_NamedWildcard range_ name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_NormalApplication :: Range ->
                              T_Type  ->
                              T_Types  ->
                              T_Type 
sem_Type_NormalApplication range_ function_ arguments_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _functionOgUniq :: UID
              _functionOnmLev :: NmLev
              _functionOtopInstanceNmL :: ([HsName])
              _argumentsOgUniq :: UID
              _argumentsOnmLev :: NmLev
              _argumentsOtopInstanceNmL :: ([HsName])
              _functionIconNm :: Name
              _functionIgUniq :: UID
              _functionItopInstanceNmL :: ([HsName])
              _argumentsIgUniq :: UID
              _argumentsItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOconNm =
                  _functionIconNm
              -- copy rule (up)
              _lhsOgUniq =
                  _argumentsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _argumentsItopInstanceNmL
              -- copy rule (down)
              _functionOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _functionOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _functionOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _argumentsOgUniq =
                  _functionIgUniq
              -- copy rule (down)
              _argumentsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _argumentsOtopInstanceNmL =
                  _functionItopInstanceNmL
              ( _functionIconNm,_functionIgUniq,_functionItopInstanceNmL) | True =
                  function_ _functionOgUniq _functionOnmLev _functionOtopInstanceNmL 
              ( _argumentsIgUniq,_argumentsItopInstanceNmL) | True =
                  arguments_ _argumentsOgUniq _argumentsOnmLev _argumentsOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Parenthesized :: Range ->
                          T_Type  ->
                          T_Type 
sem_Type_Parenthesized range_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Qualified :: Range ->
                      T_ContextItems  ->
                      T_Type  ->
                      T_Type 
sem_Type_Qualified range_ context_ type_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _contextOgUniq :: UID
              _contextOnmLev :: NmLev
              _contextOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _contextIgUniq :: UID
              _contextItopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _typeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _typeItopInstanceNmL
              -- copy rule (down)
              _contextOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _contextOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _contextOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _typeOgUniq =
                  _contextIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _typeOtopInstanceNmL =
                  _contextItopInstanceNmL
              ( _contextIgUniq,_contextItopInstanceNmL) | True =
                  context_ _contextOgUniq _contextOnmLev _contextOtopInstanceNmL 
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_RowEmpty :: Range ->
                     T_Type 
sem_Type_RowEmpty range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_RowRecEmpty :: Range ->
                        T_Type 
sem_Type_RowRecEmpty range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_RowRecUpdate :: Range ->
                         T_Type  ->
                         T_RowTypeUpdates  ->
                         T_Type 
sem_Type_RowRecUpdate range_ type_ rowTypeUpdates_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _rowTypeUpdatesOgUniq :: UID
              _rowTypeUpdatesOnmLev :: NmLev
              _rowTypeUpdatesOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              _rowTypeUpdatesIgUniq :: UID
              _rowTypeUpdatesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rowTypeUpdatesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rowTypeUpdatesItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rowTypeUpdatesOgUniq =
                  _typeIgUniq
              -- copy rule (down)
              _rowTypeUpdatesOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rowTypeUpdatesOtopInstanceNmL =
                  _typeItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
              ( _rowTypeUpdatesIgUniq,_rowTypeUpdatesItopInstanceNmL) | True =
                  rowTypeUpdates_ _rowTypeUpdatesOgUniq _rowTypeUpdatesOnmLev _rowTypeUpdatesOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_RowSumEmpty :: Range ->
                        T_Type 
sem_Type_RowSumEmpty range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_RowSumUpdate :: Range ->
                         T_Type  ->
                         T_RowTypeUpdates  ->
                         T_Type 
sem_Type_RowSumUpdate range_ type_ rowTypeUpdates_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _rowTypeUpdatesOgUniq :: UID
              _rowTypeUpdatesOnmLev :: NmLev
              _rowTypeUpdatesOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              _rowTypeUpdatesIgUniq :: UID
              _rowTypeUpdatesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rowTypeUpdatesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rowTypeUpdatesItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rowTypeUpdatesOgUniq =
                  _typeIgUniq
              -- copy rule (down)
              _rowTypeUpdatesOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rowTypeUpdatesOtopInstanceNmL =
                  _typeItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
              ( _rowTypeUpdatesIgUniq,_rowTypeUpdatesItopInstanceNmL) | True =
                  rowTypeUpdates_ _rowTypeUpdatesOgUniq _rowTypeUpdatesOnmLev _rowTypeUpdatesOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_RowUpdate :: Range ->
                      T_Type  ->
                      T_RowTypeUpdates  ->
                      T_Type 
sem_Type_RowUpdate range_ type_ rowTypeUpdates_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _typeOgUniq :: UID
              _typeOnmLev :: NmLev
              _typeOtopInstanceNmL :: ([HsName])
              _rowTypeUpdatesOgUniq :: UID
              _rowTypeUpdatesOnmLev :: NmLev
              _rowTypeUpdatesOtopInstanceNmL :: ([HsName])
              _typeIconNm :: Name
              _typeIgUniq :: UID
              _typeItopInstanceNmL :: ([HsName])
              _rowTypeUpdatesIgUniq :: UID
              _rowTypeUpdatesItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rowTypeUpdatesIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rowTypeUpdatesItopInstanceNmL
              -- copy rule (down)
              _typeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _typeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _typeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rowTypeUpdatesOgUniq =
                  _typeIgUniq
              -- copy rule (down)
              _rowTypeUpdatesOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rowTypeUpdatesOtopInstanceNmL =
                  _typeItopInstanceNmL
              ( _typeIconNm,_typeIgUniq,_typeItopInstanceNmL) | True =
                  type_ _typeOgUniq _typeOnmLev _typeOtopInstanceNmL 
              ( _rowTypeUpdatesIgUniq,_rowTypeUpdatesItopInstanceNmL) | True =
                  rowTypeUpdates_ _rowTypeUpdatesOgUniq _rowTypeUpdatesOnmLev _rowTypeUpdatesOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_SectionApplication :: Range ->
                               T_MaybeType  ->
                               T_Type  ->
                               T_MaybeType  ->
                               T_Type 
sem_Type_SectionApplication range_ leftType_ operator_ rightType_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftTypeOgUniq :: UID
              _leftTypeOnmLev :: NmLev
              _leftTypeOtopInstanceNmL :: ([HsName])
              _operatorOgUniq :: UID
              _operatorOnmLev :: NmLev
              _operatorOtopInstanceNmL :: ([HsName])
              _rightTypeOgUniq :: UID
              _rightTypeOnmLev :: NmLev
              _rightTypeOtopInstanceNmL :: ([HsName])
              _leftTypeIgUniq :: UID
              _leftTypeItopInstanceNmL :: ([HsName])
              _operatorIconNm :: Name
              _operatorIgUniq :: UID
              _operatorItopInstanceNmL :: ([HsName])
              _rightTypeIgUniq :: UID
              _rightTypeItopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (up)
              _lhsOgUniq =
                  _rightTypeIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightTypeItopInstanceNmL
              -- copy rule (down)
              _leftTypeOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftTypeOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftTypeOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _operatorOgUniq =
                  _leftTypeIgUniq
              -- copy rule (down)
              _operatorOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _operatorOtopInstanceNmL =
                  _leftTypeItopInstanceNmL
              -- copy rule (chain)
              _rightTypeOgUniq =
                  _operatorIgUniq
              -- copy rule (down)
              _rightTypeOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rightTypeOtopInstanceNmL =
                  _operatorItopInstanceNmL
              ( _leftTypeIgUniq,_leftTypeItopInstanceNmL) | True =
                  leftType_ _leftTypeOgUniq _leftTypeOnmLev _leftTypeOtopInstanceNmL 
              ( _operatorIconNm,_operatorIgUniq,_operatorItopInstanceNmL) | True =
                  operator_ _operatorOgUniq _operatorOnmLev _operatorOtopInstanceNmL 
              ( _rightTypeIgUniq,_rightTypeItopInstanceNmL) | True =
                  rightType_ _rightTypeOgUniq _rightTypeOnmLev _rightTypeOtopInstanceNmL 
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_TupleConstructor :: Range ->
                             Int ->
                             T_Type 
sem_Type_TupleConstructor range_ arity_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Variable :: Range ->
                     Name ->
                     T_Type 
sem_Type_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Type_Wildcard :: Range ->
                     T_Type 
sem_Type_Wildcard range_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOconNm :: Name
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 260, column 9)
              _lhsOconNm =
                  hsnUnknown
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOconNm,_lhsOgUniq,_lhsOtopInstanceNmL)))
-- TypeAnnotation ----------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative AnnotationName:
         child name           : {Name}
      alternative AnnotationVar:
         child name           : {Name}
         child var            : {Name}
      alternative Strict:
-}
-- cata
sem_TypeAnnotation :: TypeAnnotation  ->
                      T_TypeAnnotation 
sem_TypeAnnotation (TypeAnnotation_AnnotationName _name )  =
    (sem_TypeAnnotation_AnnotationName _name )
sem_TypeAnnotation (TypeAnnotation_AnnotationVar _name _var )  =
    (sem_TypeAnnotation_AnnotationVar _name _var )
sem_TypeAnnotation (TypeAnnotation_Strict )  =
    (sem_TypeAnnotation_Strict )
-- semantic domain
type T_TypeAnnotation  = UID ->
                         NmLev ->
                         ([HsName]) ->
                         ( UID,([HsName]))
sem_TypeAnnotation_AnnotationName :: Name ->
                                     T_TypeAnnotation 
sem_TypeAnnotation_AnnotationName name_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_TypeAnnotation_AnnotationVar :: Name ->
                                    Name ->
                                    T_TypeAnnotation 
sem_TypeAnnotation_AnnotationVar name_ var_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_TypeAnnotation_Strict :: T_TypeAnnotation 
sem_TypeAnnotation_Strict  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
-- TypeLeftHandSide --------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attributes:
         idOccDefs            : [IdOcc]
         name                 : Name
         range                : Range
         typevariables        : Names
   alternatives:
      alternative Function:
         child range          : {Range}
         child name           : {Name}
         child patterns       : TypePatterns 
      alternative Infix:
         child range          : {Range}
         child leftPattern    : TypePattern 
         child operator       : {Name}
         child rightPattern   : TypePattern 
      alternative Parenthesized:
         child range          : {Range}
         child lefthandside   : TypeLeftHandSide 
         child patterns       : TypePatterns 
-}
-- cata
sem_TypeLeftHandSide :: TypeLeftHandSide  ->
                        T_TypeLeftHandSide 
sem_TypeLeftHandSide (TypeLeftHandSide_Function _range _name _patterns )  =
    (sem_TypeLeftHandSide_Function _range _name (sem_TypePatterns _patterns ) )
sem_TypeLeftHandSide (TypeLeftHandSide_Infix _range _leftPattern _operator _rightPattern )  =
    (sem_TypeLeftHandSide_Infix _range (sem_TypePattern _leftPattern ) _operator (sem_TypePattern _rightPattern ) )
sem_TypeLeftHandSide (TypeLeftHandSide_Parenthesized _range _lefthandside _patterns )  =
    (sem_TypeLeftHandSide_Parenthesized _range (sem_TypeLeftHandSide _lefthandside ) (sem_TypePatterns _patterns ) )
-- semantic domain
type T_TypeLeftHandSide  = UID ->
                           HsName ->
                           NmLev ->
                           ([HsName]) ->
                           ( UID,([IdOcc]),Name,Range,([HsName]),Names)
sem_TypeLeftHandSide_Function :: Range ->
                                 Name ->
                                 T_TypePatterns  ->
                                 T_TypeLeftHandSide 
sem_TypeLeftHandSide_Function range_ name_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOrange :: Range
              _lhsOname :: Name
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOtypevariables :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _patternsIgUniq :: UID
              _patternsItopInstanceNmL :: ([HsName])
              _patternsItypevariables :: Names
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 205, column 9)
              _lhsOrange =
                  range_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 205, column 9)
              _lhsOname =
                  name_
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 201, column 25)
              _lhsOtypevariables =
                  _patternsItypevariables
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _patternsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _patternsOtopInstanceNmL =
                  _lhsItopInstanceNmL
              ( _patternsIgUniq,_patternsItopInstanceNmL,_patternsItypevariables) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOname,_lhsOrange,_lhsOtopInstanceNmL,_lhsOtypevariables)))
sem_TypeLeftHandSide_Infix :: Range ->
                              T_TypePattern  ->
                              Name ->
                              T_TypePattern  ->
                              T_TypeLeftHandSide 
sem_TypeLeftHandSide_Infix range_ leftPattern_ operator_ rightPattern_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOrange :: Range
              _lhsOname :: Name
              _lhsOidOccDefs :: ([IdOcc])
              _lhsOtypevariables :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _leftPatternOgUniq :: UID
              _leftPatternOmoduleNm :: HsName
              _leftPatternOnmLev :: NmLev
              _leftPatternOtopInstanceNmL :: ([HsName])
              _rightPatternOgUniq :: UID
              _rightPatternOmoduleNm :: HsName
              _rightPatternOnmLev :: NmLev
              _rightPatternOtopInstanceNmL :: ([HsName])
              _leftPatternIgUniq :: UID
              _leftPatternItopInstanceNmL :: ([HsName])
              _leftPatternItypevariables :: Names
              _rightPatternIgUniq :: UID
              _rightPatternItopInstanceNmL :: ([HsName])
              _rightPatternItypevariables :: Names
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 208, column 9)
              _lhsOrange =
                  range_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 208, column 9)
              _lhsOname =
                  operator_
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 201, column 25)
              _lhsOtypevariables =
                  _leftPatternItypevariables ++ _rightPatternItypevariables
              -- copy rule (up)
              _lhsOgUniq =
                  _rightPatternIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _rightPatternItopInstanceNmL
              -- copy rule (down)
              _leftPatternOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _leftPatternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _leftPatternOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _leftPatternOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _rightPatternOgUniq =
                  _leftPatternIgUniq
              -- copy rule (down)
              _rightPatternOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _rightPatternOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _rightPatternOtopInstanceNmL =
                  _leftPatternItopInstanceNmL
              ( _leftPatternIgUniq,_leftPatternItopInstanceNmL,_leftPatternItypevariables) | True =
                  leftPattern_ _leftPatternOgUniq _leftPatternOmoduleNm _leftPatternOnmLev _leftPatternOtopInstanceNmL 
              ( _rightPatternIgUniq,_rightPatternItopInstanceNmL,_rightPatternItypevariables) | True =
                  rightPattern_ _rightPatternOgUniq _rightPatternOmoduleNm _rightPatternOnmLev _rightPatternOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOname,_lhsOrange,_lhsOtopInstanceNmL,_lhsOtypevariables)))
sem_TypeLeftHandSide_Parenthesized :: Range ->
                                      T_TypeLeftHandSide  ->
                                      T_TypePatterns  ->
                                      T_TypeLeftHandSide 
sem_TypeLeftHandSide_Parenthesized range_ lefthandside_ patterns_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOidOccDefs :: ([IdOcc])
              _lhsOtypevariables :: Names
              _lhsOgUniq :: UID
              _lhsOname :: Name
              _lhsOrange :: Range
              _lhsOtopInstanceNmL :: ([HsName])
              _lefthandsideOgUniq :: UID
              _lefthandsideOmoduleNm :: HsName
              _lefthandsideOnmLev :: NmLev
              _lefthandsideOtopInstanceNmL :: ([HsName])
              _patternsOgUniq :: UID
              _patternsOmoduleNm :: HsName
              _patternsOnmLev :: NmLev
              _patternsOtopInstanceNmL :: ([HsName])
              _lefthandsideIgUniq :: UID
              _lefthandsideIidOccDefs :: ([IdOcc])
              _lefthandsideIname :: Name
              _lefthandsideIrange :: Range
              _lefthandsideItopInstanceNmL :: ([HsName])
              _lefthandsideItypevariables :: Names
              _patternsIgUniq :: UID
              _patternsItopInstanceNmL :: ([HsName])
              _patternsItypevariables :: Names
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 109, column 21)
              _lhsOidOccDefs =
                  _lefthandsideIidOccDefs
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 201, column 25)
              _lhsOtypevariables =
                  _lefthandsideItypevariables ++ _patternsItypevariables
              -- copy rule (up)
              _lhsOgUniq =
                  _patternsIgUniq
              -- copy rule (up)
              _lhsOname =
                  _lefthandsideIname
              -- copy rule (up)
              _lhsOrange =
                  _lefthandsideIrange
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _patternsItopInstanceNmL
              -- copy rule (down)
              _lefthandsideOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _lefthandsideOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _lefthandsideOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _lefthandsideOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _patternsOgUniq =
                  _lefthandsideIgUniq
              -- copy rule (down)
              _patternsOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _patternsOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _patternsOtopInstanceNmL =
                  _lefthandsideItopInstanceNmL
              ( _lefthandsideIgUniq,_lefthandsideIidOccDefs,_lefthandsideIname,_lefthandsideIrange,_lefthandsideItopInstanceNmL,_lefthandsideItypevariables) | True =
                  lefthandside_ _lefthandsideOgUniq _lefthandsideOmoduleNm _lefthandsideOnmLev _lefthandsideOtopInstanceNmL 
              ( _patternsIgUniq,_patternsItopInstanceNmL,_patternsItypevariables) | True =
                  patterns_ _patternsOgUniq _patternsOmoduleNm _patternsOnmLev _patternsOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOidOccDefs,_lhsOname,_lhsOrange,_lhsOtopInstanceNmL,_lhsOtypevariables)))
-- TypePattern -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         typevariables        : Names
   alternatives:
      alternative Variable:
         child range          : {Range}
         child name           : {Name}
         visit 0:
            local refname     : _
-}
-- cata
sem_TypePattern :: TypePattern  ->
                   T_TypePattern 
sem_TypePattern (TypePattern_Variable _range _name )  =
    (sem_TypePattern_Variable _range _name )
-- semantic domain
type T_TypePattern  = UID ->
                      HsName ->
                      NmLev ->
                      ([HsName]) ->
                      ( UID,([HsName]),Names)
sem_TypePattern_Variable :: Range ->
                            Name ->
                            T_TypePattern 
sem_TypePattern_Variable range_ name_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOtypevariables :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 58, column 9)
              _refname =
                  hsnSetLevQual _lhsInmLev _lhsImoduleNm name_
              -- "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 213, column 9)
              _lhsOtypevariables =
                  [name_]
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL,_lhsOtypevariables)))
-- TypePatterns ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         moduleNm             : HsName
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
      synthesized attribute:
         typevariables        : Names
   alternatives:
      alternative Cons:
         child hd             : TypePattern 
         child tl             : TypePatterns 
      alternative Nil:
-}
-- cata
sem_TypePatterns :: TypePatterns  ->
                    T_TypePatterns 
sem_TypePatterns list  =
    (Prelude.foldr sem_TypePatterns_Cons sem_TypePatterns_Nil (Prelude.map sem_TypePattern list) )
-- semantic domain
type T_TypePatterns  = UID ->
                       HsName ->
                       NmLev ->
                       ([HsName]) ->
                       ( UID,([HsName]),Names)
sem_TypePatterns_Cons :: T_TypePattern  ->
                         T_TypePatterns  ->
                         T_TypePatterns 
sem_TypePatterns_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOtypevariables :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOmoduleNm :: HsName
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOmoduleNm :: HsName
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _hdItypevariables :: Names
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              _tlItypevariables :: Names
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 201, column 25)
              _lhsOtypevariables =
                  _hdItypevariables ++ _tlItypevariables
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOmoduleNm =
                  _lhsImoduleNm
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIgUniq,_hdItopInstanceNmL,_hdItypevariables) | True =
                  hd_ _hdOgUniq _hdOmoduleNm _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL,_tlItypevariables) | True =
                  tl_ _tlOgUniq _tlOmoduleNm _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL,_lhsOtypevariables)))
sem_TypePatterns_Nil :: T_TypePatterns 
sem_TypePatterns_Nil  =
    (\ _lhsIgUniq
       _lhsImoduleNm
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOtypevariables :: Names
              _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/HS/NameDef.ag"(line 201, column 25)
              _lhsOtypevariables =
                  []
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL,_lhsOtypevariables)))
-- Types -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmLev                : NmLev
      chained attributes:
         gUniq                : UID
         topInstanceNmL       : [HsName]
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
      alternative Nil:
-}
-- cata
sem_Types :: Types  ->
             T_Types 
sem_Types list  =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list) )
-- semantic domain
type T_Types  = UID ->
                NmLev ->
                ([HsName]) ->
                ( UID,([HsName]))
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons hd_ tl_  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              _hdOgUniq :: UID
              _hdOnmLev :: NmLev
              _hdOtopInstanceNmL :: ([HsName])
              _tlOgUniq :: UID
              _tlOnmLev :: NmLev
              _tlOtopInstanceNmL :: ([HsName])
              _hdIconNm :: Name
              _hdIgUniq :: UID
              _hdItopInstanceNmL :: ([HsName])
              _tlIgUniq :: UID
              _tlItopInstanceNmL :: ([HsName])
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (up)
              _lhsOtopInstanceNmL =
                  _tlItopInstanceNmL
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOnmLev =
                  _lhsInmLev
              -- copy rule (down)
              _hdOtopInstanceNmL =
                  _lhsItopInstanceNmL
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOnmLev =
                  _lhsInmLev
              -- copy rule (chain)
              _tlOtopInstanceNmL =
                  _hdItopInstanceNmL
              ( _hdIconNm,_hdIgUniq,_hdItopInstanceNmL) | True =
                  hd_ _hdOgUniq _hdOnmLev _hdOtopInstanceNmL 
              ( _tlIgUniq,_tlItopInstanceNmL) | True =
                  tl_ _tlOgUniq _tlOnmLev _tlOtopInstanceNmL 
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))
sem_Types_Nil :: T_Types 
sem_Types_Nil  =
    (\ _lhsIgUniq
       _lhsInmLev
       _lhsItopInstanceNmL ->
         (let _lhsOgUniq :: UID
              _lhsOtopInstanceNmL :: ([HsName])
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (chain)
              _lhsOtopInstanceNmL =
                  _lhsItopInstanceNmL
          in  ( _lhsOgUniq,_lhsOtopInstanceNmL)))