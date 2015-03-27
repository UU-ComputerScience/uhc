%%[0 hs
{-# LANGUAGE ExistentialQuantification #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction for dealing with general functionaly required for specific AST/formats/file types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Abstraction for dealing with AST formats
%%]

%%[8 module {%{EH}EHC.ASTHandler}
%%]

-- general imports
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.CompileRun})
%%]

%%[8 import (Data.Typeable, GHC.Generics)
%%]

%%[8 import (qualified Data.Map as Map)
%%]
%%[8 import (qualified UHC.Util.RelMap as Rel, UHC.Util.Lens)
%%]
%%[50 import (UHC.Util.Time)
%%]

-- parsing
%%[8 import(qualified UHC.Util.ScanUtils as ScanUtils)
%%]
%%[8 import({%{EH}Base.ParseUtils})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AST parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTParser(..))
data ASTParser ast
  = forall prs inp sym symmsg pos .
      ( EHParser prs inp sym symmsg pos
      ) =>
		ASTParser
		  { unASTParser 	:: EHPrs prs inp sym pos ast
		  }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AST Handler, type parameterized
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTHandler'(..))
data ASTHandler' ast
  = -- forall prs inp sym symmsg pos . -- msg .
      -- ( PP msg
      -- , EHParser prs inp sym symmsg pos
      -- ) =>
		ASTHandler'
		  {
		  --- * Meta
	  
		  --- | Meta info: name of ast
			_asthdlrName				:: !String

		  --- * File
	  
		  --- | Construct output FPath from module name, path, suffix
		  , _asthdlrMkInputFPath		:: EHCOpts -> EHCompileUnit -> HsName -> FPath -> String -> FPath

		  --- | Construct output FPath from module name, path, suffix
		  , _asthdlrMkOutputFPath		:: EHCOpts -> HsName -> FPath -> String -> FPath

		  --- | Suffix info
		  , _asthdlrSuffixRel			:: ASTSuffixRel ast
	  
		  --- * Compile unit
	  
		  --- | Update EHCompileUnit
		  , _asthdlrEcuStore			:: EcuUpdater ast
	  
		  --- * Output, pretty printing
	  
		  --- | Generate a pretty printed text version
		  , _asthdlrPretty				:: EHCOpts -> EHCompileUnit -> ast -> Maybe PP_Doc
	  
%%[[50
		  --- | Generate a serialized binary version directly on file, yielding True if this could be done
		  , _asthdlrPutSerializeFileIO 	:: FilePath -> ast -> IO Bool
%%]]
	  
		  --- * Output
	  
		  --- | Write to an ast to a file in the IO monad, return True if could be done
		  , _asthdlrOutputIO			:: ASTFileContent -> EHCOpts -> EHCompileUnit -> HsName -> FPath -> FilePath -> ast -> IO Bool
	  
		  --- * Input, textual, parsing
	  
		  --- | Scanning parameterisation
		  , _asthdlrParseScanOpts 		:: EHCOpts -> EHParseOpts -> ScanUtils.ScanOpts
	  
		  --- | Parsing
		  , _asthdlrParser				:: EHCOpts -> EHParseOpts -> Maybe (ASTParser ast)
	  
		  --- * Input, parsing

		  --- | Input an ast
		  , _asthdlrInput				:: forall m . EHCCompileRunner m => ASTFileContent -> HsName -> EHCompilePhaseT m (Maybe ast)

%%[[50
		  --- | Read/decode from serialized binary version on file
		  , _asthdlrGetSerializeFileIO 	:: EHCOpts -> FPath -> IO (Maybe ast)
		  
		  --- | Check after deserialization
		  , _asthdlrPostInputCheck 		:: EHCOpts -> EHCompileUnit -> HsName -> FPath -> ast -> [Err]
%%]]
	  
		  }
		  deriving Typeable
%%]

%%[8 export(emptyASTHandler')
emptyASTHandler' :: forall ast . ASTHandler' ast
emptyASTHandler'
  = ASTHandler'
      { _asthdlrName 				= "Unknown AST"
      , _asthdlrSuffixRel 			= (emptyASTSuffixRel :: ASTSuffixRel ast)
      
      , _asthdlrMkInputFPath		= \_ _ _ fp s -> fpathSetSuff s fp
      , _asthdlrMkOutputFPath		= mkOutputFPath
      
      , _asthdlrEcuStore			= const id

      , _asthdlrPretty				= \_ _ _ -> Nothing
%%[[50
      , _asthdlrPutSerializeFileIO	= \_ _ -> return False
%%]]
      , _asthdlrOutputIO 			= \_ _ _ _ _ _ _ -> return False

      , _asthdlrInput 				= \_ _ -> return Nothing
      , _asthdlrParseScanOpts		= \_ _ -> ScanUtils.defaultScanOpts
      , _asthdlrParser				= \_ _ -> (Nothing :: Maybe (ASTParser ast))
%%[[50
      , _asthdlrGetSerializeFileIO	= \_ _ -> return Nothing
      , _asthdlrPostInputCheck		= \_ _ _ _ _ -> []
%%]]
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AST Handler, type hidden a la Dynamic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTHandler(..))
data ASTHandler
  = forall ast .
      Typeable ast =>
        ASTHandler (ASTHandler' ast)
%%]

%%[8 export(ASTHandlerMp)
type ASTHandlerMp = Map.Map ASTType ASTHandler
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AST variation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTSuffixInfo(..))
-- | Per suffix AST specific info
data ASTSuffixInfo ast
  = ASTSuffixInfo
      { _astsuffinfoSuff		:: String
      , _astsuffinfoASTLensMp	:: Map.Map ASTFileTiming (Lens EHCompileUnit (Maybe ast))
%%[[50
      , _astsuffinfoModfTimeMp	:: Map.Map ASTFileTiming (Lens EHCompileUnit (Maybe ClockTime))
%%]]
      }
  deriving (Typeable, Generic)

instance Eq (ASTSuffixInfo ast) where
  i1 == i2 = _astsuffinfoSuff i1 == _astsuffinfoSuff i2

instance Ord (ASTSuffixInfo ast) where
  i1 `compare` i2 = _astsuffinfoSuff i1 `compare` _astsuffinfoSuff i2
%%]

%%[8 export(ASTSuffixRel, mkASTSuffixRel, mkASTSuffixRel', emptyASTSuffixRel, astsuffixLookup, astsuffixLookupSuff, astsuffixLookupLens)
type ASTSuffixRel ast = Rel.Rel ASTSuffixKey (ASTSuffixInfo ast)

emptyASTSuffixRel :: ASTSuffixRel ast
emptyASTSuffixRel = Rel.empty

mkASTSuffixRel'
  :: AssocL
       ASTSuffixKey
       ( String
       , AssocL ASTFileTiming (Lens EHCompileUnit (Maybe ast))
%%[[8
       , AssocL ASTFileTiming (Maybe ())
%%][50
       , AssocL ASTFileTiming (Lens EHCompileUnit (Maybe ClockTime))
%%]]
       )
     -> ASTSuffixRel ast
mkASTSuffixRel' l = Rel.fromList
  [ ( sk
    , ASTSuffixInfo
        s
        (Map.fromList il)
%%[[50
        (Map.fromList cl)
%%]]
    )
  | (sk,(s,il,cl)) <- l
  ]

mkASTSuffixRel
  :: AssocL
       ASTSuffixKey
       ( String
       , Lens EHCompileUnit (Maybe ast)
%%[[8
       , Maybe ()
%%][50
       , Maybe (Lens EHCompileUnit (Maybe ClockTime))
%%]]
       )
     -> ASTSuffixRel ast
mkASTSuffixRel l = mkASTSuffixRel' $
  [ ( sk
    , ( s
      , [(ASTFileTiming_Current,i)]
%%[[8
      , []
%%][50
      , maybe [] (\c -> [(ASTFileTiming_Current,c)]) mc
%%]]
    ) )
  | (sk,(s,i,mc)) <- l
  ]

-- | Lookup suffix info
astsuffixLookup :: ASTSuffixKey -> ASTSuffixRel ast -> Maybe (ASTSuffixInfo ast)
astsuffixLookup = Rel.lookupDom 

-- | Lookup suffix
astsuffixLookupSuff :: ASTSuffixKey -> ASTSuffixRel ast -> Maybe String
astsuffixLookupSuff k r = fmap _astsuffinfoSuff $ astsuffixLookup k r

-- | Lookup lens
astsuffixLookupLens :: ASTSuffixKey -> ASTFileTiming -> ASTSuffixRel ast -> Maybe (Lens EHCompileUnit (Maybe ast))
astsuffixLookupLens sk tk r = do
  i <- astsuffixLookup sk r
  Map.lookup tk $ _astsuffinfoASTLensMp i
%%]

%%[50 export(astsuffixLookupTmLens)
-- | Lookup lens for modf time of
astsuffixLookupTmLens :: ASTSuffixKey -> ASTFileTiming -> ASTSuffixRel ast -> Maybe (Lens EHCompileUnit (Maybe ClockTime))
astsuffixLookupTmLens sk tk r = do
  i <- astsuffixLookup sk r
  Map.lookup tk $ _astsuffinfoModfTimeMp i
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(asthdlrOutputIO)
-- | Write to an ast to a file in the IO monad, return True if could be done
asthdlrOutputIO :: ASTHandler' ast -> ASTFileContent -> EHCOpts -> EHCompileUnit -> HsName -> FPath -> FilePath -> ast -> IO Bool
asthdlrOutputIO hdlr how opts ecu modNm fpC fnC ast = do
    fpathEnsureExists fpC
    case how of
      ASTFileContent_Text -> do
        case _asthdlrPretty hdlr opts ecu ast of
          Just ppAst -> do
            putPPFPath fpC ppAst 1000
            return True
          _ -> return False
%%[[50
      ASTFileContent_Binary -> do
        _asthdlrPutSerializeFileIO hdlr fnC ast
%%]]
      _ -> return False
%%]

%%[8 export(asthdlrMkInputFPath)
-- | Construct a FPath given a handler
asthdlrMkInputFPath :: ASTHandler' ast -> EHCOpts -> EHCompileUnit -> ASTSuffixKey -> HsName -> FPath -> FPath
asthdlrMkInputFPath hdlr opts ecu skey modNm fp = _asthdlrMkInputFPath hdlr opts ecu modNm fp suff
  where suff = maybe "" id $ astsuffixLookupSuff skey $ _asthdlrSuffixRel hdlr
%%]
