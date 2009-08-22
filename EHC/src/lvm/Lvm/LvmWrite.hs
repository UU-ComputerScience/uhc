{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: LvmWrite.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.LvmWrite( lvmWriteFile, lvmToBytes ) where

import Lvm.Common.Standard ( assert, strict )
import Lvm.Common.Id       ( Id, stringFromId, setNameSpace )
import Lvm.Common.IdMap    ( IdMap, emptyMap, insertMapWith, lookupMap, listFromMap )
import System   ( exitWith, ExitCode(..))
import Lvm.Common.Byte
import Lvm.Lvm.Instr
import Lvm.Lvm.Lvm

{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
lvmMajorVersion,lvmMinorVersion :: Int
lvmMajorVersion  = 15
lvmMinorVersion  = 0

{--------------------------------------------------------------
  Emit an LVM file
--------------------------------------------------------------}
lvmWriteFile :: FilePath -> LvmModule -> IO ()
lvmWriteFile path lvm
  = let bytes = lvmToBytes lvm
    in seq bytes $
        writeBytes path bytes `catch` (\exception ->
            let message = show exception ++ "\n\nUnable to write to file " ++ show path
            in do { putStrLn message; exitWith (ExitFailure 1) })

lvmToBytes :: LvmModule -> Bytes
lvmToBytes mod
  = let (idxInfo,recs) = bytesFromModule mod
        headerlen = 24
        header    = block
                    [ recHeader
                    , encodeInt headerlen
                    , encodeInt totallen
                    , encodeInt lvmMajorVersion
                    , encodeInt lvmMinorVersion
                    , encodeInt (length recs)
                    , encodeInt (bytesLength brecs)
                    , encodeIdx idxInfo
                    ]

        footerlen = 4
        footer    = block [ recFooter, encodeInt footerlen, encodeInt totallen ]

        brecs     = cats recs 
        totallen  = bytesLength brecs + headerlen + 8 + footerlen + 8
        total     = cats [header,brecs,footer]
    in seq totallen total

bytesFromModule :: LvmModule -> (Index,[Bytes])
bytesFromModule mod
  = runEmit (emitLvmModule mod) 


emitLvmModule :: LvmModule -> Emit Index
emitLvmModule mod
  = do{ idxInfo <- emitModule (moduleName mod) (moduleMajorVer mod) (moduleMinorVer mod)
      ; mapM_ emitDecl (moduleDecls mod)
      ; return idxInfo
      }

{--------------------------------------------------------------
  emit declarations
  TODO: emit  assumes some canonical order: We should do
  allocation of named blocks first and than emit to fix this.
--------------------------------------------------------------}
flags :: Access -> Int
flags access    = if (accessPublic access) then 1 else 0

isImported decl
  = case declAccess decl of
      Imported{}  -> True
      other       -> False

emitDecl DeclExtern{ externCall = CallInstr }  
  = return 0
emitDecl decl
  | isImported decl   = emitImport (declName decl) (declKindFromDecl decl) (declAccess decl) []
emitDecl decl
  = case decl of
      DeclValue{}     -> emitDValue decl
      DeclAbstract{}  -> emitDAbstract decl
      DeclCon{}       -> emitDCon decl
      DeclExtern{}    -> emitDExtern decl
      DeclCustom{}    -> emitDCustom decl
      other           -> error "LvmWrite.emitDecl: invalid declaration at this phase"

emitDValue (DeclValue id access mbEnc instrs custom)
  = do{ idxEnc  <- maybe (return 0) (\id -> findIndex DeclKindValue id) mbEnc
      ; idxCode <- emitInstrs instrs
      ; emitNamedBlock id DeclKindValue [encodeInt (flags access), encodeInt arity
                                        ,encodeIdx idxEnc, encodeIdx idxCode] custom
      }
  where
    arity = case instrs of
              (ARGCHK n:other) -> n
              otherwise        -> error ("LvmWrite.emitDecl: instructions do not start with an argument check: " ++ show id)

emitDCon (DeclCon id access arity tag custom)
  = emitNamedBlock id DeclKindCon [encodeInt (flags access),encodeInt arity,encodeInt tag] custom

emitDCustom (DeclCustom id access kind custom)
  = emitNamedBlock id kind [encodeInt (flags access)] custom

emitDExtern (DeclExtern id access arity tp linkconv callconv libname externname custom)
  = do{ idxType            <- emitExternType tp
      ; idxLibName         <- emitNameString libname
      ; (nameFlag,idxName) <- emitNameExtern
      ; idxId              <- emitName id
      ; emitBlockEx (Just id) DeclKindValue DeclKindExtern  
            (block [encodeIdx idxId, encodeInt (flags access), encodeInt arity
                    ,encodeIdx idxType
                    ,encodeIdx idxLibName
                    ,idxName      -- already encoded
                    ,encodeInt nameFlag
                    ,encodeInt (fromEnum linkconv)
                    ,encodeInt (fromEnum callconv)
                    ]) custom
      }
  where
    emitNameExtern  = case externname of
                        Plain s    -> do{ idx <- emitNameString s; return (0,encodeIdx idx) }
                        Decorate s -> do{ idx <- emitNameString s; return (1,encodeIdx idx) }
                        Ordinal i  -> return (2,encodeInt i)


emitDAbstract (DeclAbstract id access arity customs)
  = error ("LvmWrite.emitDAbstract: abstract values should be imported: " ++ show id)


emitImport id declkind access@(Imported public moduleName importName kind majorVer minorVer) customs
  = assert (declkind==kind) "LvmWrite.emitImport: kinds don't match" $
    do{ idxModule <- emitModule moduleName majorVer minorVer
      ; idxName   <- emitName importName
      ; idxId     <- emitName id
      ; kindenc   <- encodeKind declkind
      ; emitBlockEx (Just id) declkind DeclKindImport 
          (block [encodeIdx idxId, encodeInt (flags access), encodeIdx idxModule
                 , encodeIdx idxName, kindenc]) customs
      }

emitModule name major minor
  = do{ idxName <- emitName name
      ; emitBlock Nothing DeclKindModule (block [encodeIdx idxName,encodeInt major,encodeInt minor]) []
      }

{--------------------------------------------------------------
  emit instructions
--------------------------------------------------------------}
emitInstrs :: [Instr] -> Emit Index
emitInstrs instrs
  = do{ rinstrs <- mapM resolve instrs
      ; let codes = concatMap emit rinstrs
      ; emitBlock Nothing DeclKindCode (block codes) []
      }
{-
emitCode :: (Id,LvmValue) -> Emit Index
emitCode (id,DValue access mbEnc instrs custom)
  = do{ idx     <- findIndex id
      ; rinstrs <- mapM resolve instrs
      ; let codes = concatMap emit rinstrs
      ; emitBlock Nothing recCode (block (idx:codes)) []
      }

emitCode other
  = return 0
-}

{--------------------------------------------------------------
  emit an instruction
--------------------------------------------------------------}
emits instrs
  = concatMap emit instrs

emit :: Instr -> [Int]
emit instr
  = let opcode   = opcodeFromInstr instr
        illegal  = error ("LvmWrite.emit: illegal instruction at this phase: " ++ show (nameFromInstr instr))
        todo     = error ("LvmWrite.emit: todo: " ++ show (nameFromInstr instr))
    in case instr of
      -- pseudo instructions
      VAR id                  -> []
      PARAM id                -> []
      USE id                  -> []
      NOP                     -> illegal
      RESULT is               -> illegal

      -- structured instructions
      MATCH alts              -> [opcode] ++ emitMatch 3 alts
      MATCHCON alts           -> [opcode] ++ emitMatch 2 alts
      MATCHINT alts           -> [opcode] ++ emitMatch 2 alts
      SWITCHCON alts          -> todo

      EVAL d is               -> let scrut = emits is
                                 in  emit (PUSHCONT (length scrut)) ++ scrut

      -- push instructions
      PUSHVAR     var         -> [opcode, offsetFromVar var]
      PUSHINT     n           -> [opcode, n]
      PUSHBYTES   bs c        -> [opcode, c]
      PUSHFLOAT   d           -> todo
      PUSHCODE    global      -> [opcode, indexFromGlobal global]
      PUSHCONT    ofs         -> [opcode, ofs]

      -- stack instructions
      ARGCHK      n           -> [opcode, n]
      SLIDE       n m depth   -> [opcode, n, m]
      STUB        var         -> [opcode, offsetFromVar var]

      -- control
      ENTER                   -> [opcode]
      PUSHCATCH               -> [opcode]
      RAISE                   -> [opcode]
      CALL        global      -> [opcode, indexFromGlobal global, arityFromGlobal global]

      ENTERCODE   global      -> [opcode, indexFromGlobal global]
      EVALVAR     var         -> [opcode, offsetFromVar var]

      RETURN                  -> [opcode]
      RETURNCON   con         -> [opcode, indexFromCon con, arityFromCon con]
      RETURNINT   i           -> [opcode, i]

      -- applications
      ALLOCAP     arity       -> [opcode, arity]
      PACKAP      var arity   -> [opcode, offsetFromVar var, arity]
      PACKNAP     var arity   -> [opcode, offsetFromVar var, arity]
      NEWAP       arity       -> [opcode, arity]
      NEWNAP      arity       -> [opcode, arity]

      -- constructors
      ALLOCCON    con         -> [opcode, indexFromCon con, arityFromCon con]
      PACKCON     con var     -> [opcode, offsetFromVar var, arityFromCon con] --TODO: constant instead of arity

      NEWCON      con         -> [opcode, indexFromCon con, arityFromCon con]                                 

      NEW         arity       -> [opcode, arity]
      PACK        arity v     -> [opcode, arity, offsetFromVar v]
      UNPACK      arity       -> [opcode, arity]

      -- optimized instructions
      PUSHVARS2   v w         -> [opcode, offsetFromVar v, offsetFromVar w]

      NEWCON0 con             -> [opcode, indexFromCon con]
      NEWCON1 con             -> [opcode, indexFromCon con]
      NEWCON2 con             -> [opcode, indexFromCon con]
      NEWCON3 con             -> [opcode, indexFromCon con]

      RETURNCON0 con          -> [opcode, indexFromCon con]

      -- single opcode instructions
      other                   -> [opcode]


emitMatch entrySize alts
  = assert (normalizedAlts alts) "LvmWrite.emitMatch: unnormalized alternatives" $
    let (pats,iss) = unzipAlts alts
        altis      = map emits iss
        start      = 2 + entrySize*(length alts -1)
    in [length alts-1]
       ++ matches start (zip pats (map length altis))
       ++ concat altis       
    where
      matches top []           = []
      matches top ((pat,n):xs)
        = (case pat of
             PatCon con -> [indexFromCon con]
             PatInt i   -> [i]
             PatTag t a -> [t,a]
             PatDefault -> [])
          ++ [if (n==0) then 0 else top] ++ matches (top+n) xs


normalizedAlts alts
  = case alts of
      (Alt PatDefault is:rest) -> True
      other                    -> False

unzipAlts alts
  = unzip (map (\(Alt pat expr) -> (pat,expr)) alts)


{--------------------------------------------------------------
  resolve instructions
--------------------------------------------------------------}
resolves :: ([Instr] -> a) -> [Instr] -> Emit a
resolves f is
  = do{ ris <- mapM resolve is
      ; return (f ris)
      }

resolve :: Instr -> Emit Instr
resolve instr
  = case instr of
      EVAL d is       -> resolves (EVAL d) is
      RESULT is       -> resolves RESULT is
      MATCH    alts   -> resolveAlts MATCH alts
      MATCHCON alts   -> resolveAlts MATCHCON alts
      MATCHINT alts   -> resolveAlts MATCHINT alts
      SWITCHCON alts  -> resolveAlts SWITCHCON alts

      PUSHCODE global -> resolveGlobal PUSHCODE global
      ENTERCODE global-> resolveGlobal ENTERCODE global
      CALL global     -> resolveGlobal CALL global

      RETURNCON con   -> resolveCon RETURNCON con
      ALLOCCON con    -> resolveCon ALLOCCON con
      NEWCON con      -> resolveCon NEWCON con
      PACKCON con var -> resolveCon (\c -> PACKCON c var) con

      PUSHBYTES bs _  -> resolveBytes (PUSHBYTES bs) bs

      -- optimized instructions
      RETURNCON0 con  -> resolveCon RETURNCON0 con
      NEWCON0 con     -> resolveCon NEWCON0 con
      NEWCON1 con     -> resolveCon NEWCON1 con
      NEWCON2 con     -> resolveCon NEWCON2 con
      NEWCON3 con     -> resolveCon NEWCON3 con

      other           -> return instr

resolveAlts f alts
  = do{ alts' <- sequence (map resolveAlt alts); return (f alts') }

resolveAlt :: Alt -> Emit Alt
resolveAlt (Alt pat is)
  = do{ pat' <- resolvePat pat
      ; resolves (Alt pat') is
      }

resolvePat pat
  = case pat of
      PatCon con  -> resolveCon PatCon con
      other       -> return pat

resolveGlobal f (Global id _ arity)
  = do{ idx <- findIndex DeclKindValue id
      ; return (f (Global id idx arity))
      }

resolveCon f (Con id _ arity tag)
  = do{ idx <- findIndex DeclKindCon id
      ; return (f (Con id idx arity tag))
      }

resolveBytes f bs
  = do{ idx <- emitBytes bs
      ; return (f idx)
      }



{--------------------------------------------------------------
  basic entities
--------------------------------------------------------------}
emitNamedBlock :: Id -> DeclKind -> [Int] -> [Custom] -> Emit Index
emitNamedBlock id kind is custom
  = do{ idxName <- emitName id
      ; emitBlock (Just id) kind (block (encodeIdx idxName:is)) custom
      }

emitName :: Id -> Emit Index
emitName id
  = emitNameString (stringFromId id)

emitNameString :: String -> Emit Index
emitNameString s
  = emitBlock Nothing DeclKindName (blockString s) []

emitExternType tp
  = emitBlock Nothing DeclKindExternType (blockString tp) []

emitKind id
  = emitBlock Nothing DeclKindKind (blockString (stringFromId id)) []

emitBytes bs
  = emitBlock Nothing DeclKindBytes (blockBytes bs) []

emitBlock mbId kind bs custom
  = emitBlockEx mbId kind kind bs custom

emitBlockEx mbId kindId kind bs custom
  = do{ bcustom <- emitCustoms custom
      ; kindenc <- encodeKind kind
      ; let bytes = cat bs bcustom
            total = cat (block [kindenc,encodeInt (bytesLength bytes)]) bytes
      ; assert ((bytesLength bytes `mod` 4) == 0) "LvmWrite.emitBlock: unaligned size" $
        emitPrimBlock (maybe Nothing (\id -> Just (id,kindId)) mbId) kind total
      }

encodeKind :: DeclKind -> Emit Int
encodeKind (DeclKindCustom id)
  = do{ idx <- emitKind id
      ; return (encodeIdx idx)
      }

encodeKind kind
  = return (encodeInt (fromEnum kind))


{--------------------------------------------------------------
  custom fields
--------------------------------------------------------------}
emitCustoms :: [Custom] -> Emit Bytes
emitCustoms decls
  = do{ is <- mapM emitCustom decls
      ; return (block is)
      }

emitCustom :: Custom -> Emit Int
emitCustom custom
  = case custom of
      CustomInt i         -> return (encodeInt i)
      CustomNothing       -> return (encodeIdx 0)
      CustomBytes bs      -> do{ idx <- emitBytes bs; return (encodeIdx idx) }
      CustomName id       -> do{ idx <- emitName id; return (encodeIdx idx) }
      CustomLink id kind  -> do{ idx <- findIndex kind id; return (encodeIdx idx) }
      CustomDecl kind cs  -> do{ idx <- emitAnonymousCustom kind cs; return (encodeIdx idx) }

emitAnonymousCustom kind custom
  = emitBlock Nothing kind (block [encodeIdx 0]) custom


{--------------------------------------------------------------
  Emit Monad
--------------------------------------------------------------}
newtype Emit a  = Emit (Env -> State -> (a,State))
data State      = State !Int Env [Bytes]
type Env        = IdMap [(DeclKind,Index)]

instance Functor Emit where
  fmap f (Emit e)   = Emit (\env st -> case e env st of
                                         (x,stx) -> (f x,stx))

instance Monad Emit where
  return x          = Emit (\env st -> (x,st))
  (Emit e) >>= f    = Emit (\env st -> case e env st of
                                         (x,stx) -> case f x of
                                                      Emit ef -> ef env stx)

runEmit :: Emit a -> (a,[Bytes])
runEmit (Emit e)
  = let (x,State _ env bbs) = e env (State 0 emptyMap [])  -- yes, a recursive, lazy, env :-)
    in (x,reverse bbs)

emitPrimBlock :: Maybe (Id,DeclKind) -> DeclKind -> Bytes -> Emit Index
emitPrimBlock x kind bs
  = Emit (\env st@(State count map bbs) ->
            let (index,count',bbs') | sharable kind = case find count bs bbs of   --try to share records
                                                        Nothing  -> (count+1,count+1,bs:bbs)
                                                        Just idx -> (idx,count,bbs)
                                    | otherwise     = (count+1,count+1,bs:bbs)
                map'                = case x of
                                        Just (id,kindid) -> insertMapWith id [(kindid,index)] ((kindid,index):) map
                                        Nothing          -> map
            in (index, State count' map' bbs')
         )
sharable kind
  = False
{-
  = case kind of
      DeclKindBytes       -> True
      DeclKindName        -> True
      DeclKindKind        -> True
      DeclKindExternType  -> True
      DeclKindModule      -> True   -- dubious when custom values are present!
      other               -> False
-}

find n x []       = assert (n==0) "LvmWrite.find: count too large" Nothing
find n x (y:ys)   | x==y       = Just n
                  | otherwise  = strict find (n-1) x ys

-- a nice lazy formulation, we can calculate all indices before writing the bytes.
findIndex :: DeclKind -> Id -> Emit Index
findIndex kind id 
  = Emit (\env st ->
          (case lookupMap id env of
             Nothing  -> error ("LvmWrite.findIndex: undeclared identifier: " ++ show (stringFromId id))
             Just xs  -> case lookup kind xs of
                           Nothing  -> error ("LvmWrite.findIndex: undeclared identifier (with the right declaration kind): " ++ show (stringFromId id))
                           Just idx -> (idx)
          , st))



{--------------------------------------------------------------
  block
--------------------------------------------------------------}
block :: [Int] -> Bytes
block is
  = cats (map bytesFromInt32 is)

blockString :: String -> Bytes
blockString s
  = blockBytes (bytesFromString s)

blockBytes bs
  = let len = bytesLength bs
    in cat (bytesFromInt32 (encodeInt len)) (cat bs (padding len))

padding n
  = let m = (div (n + 3) 4) * 4
    in bytesFromList (replicate (m - n) (byteFromInt8 0))

encodeInt i
  = (2*i)+1

encodeIdx i
  = (2*i)