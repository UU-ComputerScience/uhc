{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: LvmRead.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.LvmRead( lvmReadFile, lvmRead ) where

import Prelude hiding (Read)
import Array
import Lvm.Common.PPrint   ( putDoc )
import Lvm.Common.Standard ( assert, foldlStrict, getLvmPath, searchPath )
import Lvm.Common.Id       ( Id, stringFromId, idFromString, newNameSupply, freshId )
import Lvm.Common.IdMap

import Lvm.Common.Byte   hiding (readByteList)
import qualified Lvm.Common.Byte as Byte
import Lvm.Lvm.Lvm
import Lvm.Lvm.Instr
import Lvm.Lvm.InstrPretty ( instrPretty )
import Lvm.Lvm.ModulePretty( modulePretty )

{--------------------------------------------------------------
  Magic numbers
--------------------------------------------------------------}
lvmMajor, lvmMinor :: Int
lvmMajor  = 15
lvmMinor  = 0

data Record v   = RecDecl       (Decl v)
                
                | RecName       Id
                | RecKind       Id
                | RecBytes      !Bytes
                | RecCode       ![Int]
                | RecModule     Id !Int !Int [Custom]
                | RecExternType !String
                | RecAnon       !DeclKind [Custom]
                
{--------------------------------------------------------------
  read an LVM file
--------------------------------------------------------------}
test src
  = do{ path    <- getLvmPath
      ; source  <- searchPath path ".lvm" src
      ; mod     <- lvmReadFile source
      ; putDoc (modulePretty instrPretty mod)
      }

lvmReadFile :: FilePath -> IO (Module v)
lvmReadFile fname
  = do{ bs <- Byte.readByteList fname
      ; ns <- newNameSupply
      ; return (lvmRead ns fname bs)
      }

lvmRead :: NameSupply -> FilePath -> [Byte] -> (Module v) 
lvmRead ns fname bs
  = runRead readModule ns fname bs

readModule :: Read v (Module v,[Record v])
readModule
  = do{ tag    <- readRaw
      ; readGuard (tag == recHeader) "readHeader" "magic number is incorrect"
      ; len    <- readint
      ; total  <- readint
      ; lvmmajor <- readint
      ; lvmminor <- readint
      ; readGuard (lvmmajor == lvmMajor && lvmminor >= lvmMinor) "readHeader" ("incompatible lvm version " ++ show lvmmajor ++ "." ++ show lvmminor)
      ; count  <- readint
      ; bcount <- readint
      ; ~(id,major,minor)  <- readModuleIdx 
      ; recs   <- readRecords total [] 
      ; readGuard (count == length recs) "readModule" "incorrect record count"
      ; return (Module id major minor [d | RecDecl d <- filter isRecDecl recs],recs)
      }
  where
    isRecDecl (RecDecl d) = True
    isRecDecl other       = False

readFooter :: Read v ()
readFooter
  = return ()

readRecords :: Int -> [Record v] -> Read v [Record v]
readRecords total acc
  = do{ x     <- readRaw
      ; len   <- readint
      ; if (x == recFooter)
         then do{ total' <- readint
                ; readGuard (total==total') "readRecords" "footer doesn't match with header"
                ; return (reverse acc)
                }
        else if (isInt x) 
         then do{ let tag = decodeInt x 
                ; rec <- case tag of
                          0     -> readName len
                          1     -> readKind len
                          2     -> readBytes len
                          3     -> readCode len
                          4     -> readValue len
                          5     -> readCon len
                          6     -> readImport len
                          7     -> readModuleRec len
                          8     -> readExtern len
                          9     -> readExternType len
                          other -> readError "readRecords" ("unknown standard record kind (" ++ show tag ++ ")")
                ; readRecords total (rec:acc)
                }
         else do{ let idx = decodeIdx x
                ; rec <- readDeclCustom idx len 
                ; readRecords total (rec:acc)
                }
      }


{--------------------------------------------------------------
  declarations
--------------------------------------------------------------}
readValue :: Int -> Read v (Record v)
readValue len
  = do{ id     <- readNameIdx "value"
      ; acc    <- readAccess
      ; arity  <- readint
      ; enc    <- readEnclosing
      -- ; code   <- readCodeIdx
      ; readIdx "code"
      ; customs<- readCustoms (len - 20)
      ; return (RecDecl (DeclAbstract id acc arity customs))
      }

readCon len
  = do{ id    <- readNameIdx "constructor"
      ; acc   <- readAccess
      ; arity <- readint
      ; tag   <- readint
      ; customs <- readCustoms (len - 16)
      ; return (RecDecl (DeclCon id acc arity tag customs))
      }

readImport len
  = do{ id    <- readNameIdx "import"
      ; flags <- readint
      ; ~(modid,major,minor) <- readModuleIdx
      ; impid <- readNameIdx "imported"
      ; kind  <- readKindIdx
      ; customs <- readCustoms (len - 20)
      ; return (RecDecl (DeclImport id (Imported (odd flags) modid impid kind major minor) customs))
      }

readKindIdx :: Read v DeclKind
readKindIdx
  = do{ xkind <- readRaw
      ; if (isInt xkind)
         then return (toEnum (decodeInt xkind))
         else do{ kindid <- resolveKindIdx (decodeIdx xkind)
                ; return (DeclKindCustom kindid)
                }
      }

readModuleRec len
  = do{ id    <- readNameIdx "module"
      ; major <- readint
      ; minor <- readint
      ; customs <- readCustoms (len - 12)
      ; return (RecModule id major minor customs)
      }

readDeclCustom :: Index -> Int -> Read v (Record v)
readDeclCustom kindIdx len
  = do{ kindid  <- resolveKindIdx kindIdx
      ; mbId    <- readCustomNameIdx
      ; case mbId of
          Just id  -> do{ acc     <- readAccess
                        ; customs <- readCustoms (len-8)
                        ; return (RecDecl (DeclCustom id acc (DeclKindCustom kindid) customs))
                        }
          Nothing  -> do{ customs <- readCustoms (len-4)
                        ; return (RecAnon (DeclKindCustom kindid) customs)
                        }
      }

readExtern :: Int -> Read v (Record v)
readExtern len
  = do{ id    <- readNameIdx "extern"
      ; acc   <- readAccess
      ; arity <- readint
      ; tp    <- readExternTypeIdx
      ; libname <- readNameStringIdx
      ; xname <- readRaw 
      ; mode  <- readint
      ; link  <- readint
      ; call  <- readint
      ; customs <- readCustoms (len - 9*4)
      ; name  <- case mode of
                   1  -> fmap Decorate  (readNameString (decodeIdx xname))
                   2  -> return (Ordinal (decodeInt xname))
                   _  -> fmap Plain     (readNameString (decodeIdx xname))
      ; let linkMode = case link of
                         1 -> LinkDynamic
                         2 -> LinkRuntime
                         _ -> LinkStatic
            callMode = case call of
                         1 -> CallStd
                         2 -> CallInstr
                         _ -> CallC
      ; return (RecDecl (DeclExtern id acc arity tp linkMode callMode libname name customs))
      }
                   
{--------------------------------------------------------------
  constants
--------------------------------------------------------------}
readCode :: Int -> Read v (Record v)
readCode len
  = do{ ints   <- mapM (const readRaw) [1..div len 4]
      ; return (RecCode ints)
      }

readName :: Int -> Read v (Record v)
readName len
  = do{ bs <- readByteSeq len
      ; return (RecName (idFromString (stringFromByteList bs)))
      }

readKind :: Int -> Read v (Record v)
readKind len
  = do{ bs <- readByteSeq len
      ; return (RecKind (idFromString (stringFromByteList bs)))
      }

readBytes :: Int -> Read v (Record v)
readBytes len
  = do{ bs <- readByteSeq len
      ; return (RecBytes (bytesFromByteList bs))
      }

readExternType len
  = do{ bs <- readByteSeq len
      ; return (RecExternType (stringFromByteList bs))
      }

readByteSeq :: Int -> Read v [Byte]
readByteSeq len
  = do{ blen <- readint
      ; bs   <- readByteList blen
      ; skip (len - 4 - blen)      
      ; return bs
      }

readCustoms :: Int -> Read v [Custom]
readCustoms len
  = mapM (const readCustom) [1..div len 4]
    
readAccess :: Read v Access
readAccess
  = do{ flags <- readint
      ; return (Defined (odd flags))
      }

readCustom :: Read v Custom
readCustom
  = do{ x <- readRaw
      ; if (isInt x) 
         then return (CustomInt (decodeInt x))
        else if (decodeIdx x == 0)
         then return CustomNothing
         else resolve (decodeIdx x) recToCustom
      }
  where
    recToCustom rec
      = case rec of
          RecName id        -> CustomName id
          RecBytes bs       -> CustomBytes bs
          RecDecl d         -> CustomLink (declName d) (declKindFromDecl d)
          RecAnon kind cs   -> CustomDecl kind cs
          other             -> error "LvmRead.readCustom: invalid link"


{--------------------------------------------------------------
  indices
--------------------------------------------------------------}
readNameIdx :: String -> Read v Id
readNameIdx parent
  = do{ idx <- readIdx (parent ++ ".name")
      ; if (idx == 0)
         then readFreshId
         else resolve idx (\rec -> case rec of 
                              RecName id  -> id
                              other       -> error "LvmRead.readName: invalid name index")
      }

readCustomNameIdx :: Read v (Maybe Id)
readCustomNameIdx
  = do{ idx <- readIdx "custom name"
      ; if (idx==0)
         then return Nothing
         else do{ id <- resolve idx (\rec -> case rec of 
                                               RecName id  -> id
                                               other       -> error "LvmRead.readCustomNameIdx: invalid name index")
                ; return (Just id)
                }
      }

resolveKindIdx :: Index -> Read v Id
resolveKindIdx idx
  = do{ resolve idx (\rec -> case rec of 
                              RecKind id  -> id
                              other       -> error "LvmRead.resolveKindIdx: invalid kind index")
      }

readModuleIdx 
  = do{ idx <- readIdx "module descriptor"
      ; resolve idx (\rec -> case rec of
                               RecModule modid major minor cs -> (modid,major,minor)
                               other -> error "LvmRead.readModule: invalid module index")
      }

readExternTypeIdx
  = do{ idx <- readIdx "extern type"
      ; resolve idx (\rec -> case rec of
                               RecExternType tp -> tp
                               other  -> error "LvmRead.readExternType: invalid extern type index")
      }

readNameStringIdx
  = do{ idx <- readIdx "name string"
      ; readNameString idx
      }

readNameString idx 
  = resolve idx (\rec -> case rec of
                           RecName id   -> stringFromId id
                           RecBytes bs  -> stringFromBytes bs
                           other  -> error "LvmRead.readNameString: invalid name index")
              

readCodeIdx
  = do{ idx <- readIdx "code"
      ; resolve idx (\rec -> case rec of
                               RecCode code -> code
                               other        -> error "readCode" "invalid code index")
      }

readEnclosing
  = do{ idx  <- readIdx "enclosing"
      ; if (idx == 0) 
          then return Nothing
          else resolve idx (\rec -> case rec of
                                     RecDecl d  | isDeclValue d || isDeclAbstract d -> Just (declName d)
                                     other            -> error "readEnclosing" "invalid enclosing index"
                          )
      }



readint :: Read v Int
readint 
  = do{ i <- readRaw
      ; readGuard (isInt i) "readint" ("expecting integer but found index")
      ; return (decodeInt i)
      }
readIdx :: String -> Read v Int
readIdx name
  = do{ i <- readRaw
      ; readGuard (isIdx i) "readIdx" ("expecting index but found integer (" ++ name ++ ")")
      ; return (decodeIdx i)
      }

isInt i = odd i
isIdx i = even i

decodeInt i = (i-1) `div` 2
decodeIdx i = i `div` 2

{--------------------------------------------------------------
  Reader monad.
  Note the lazy recursive definition, where resolving
  and reading is done in a single pass (using delayed 
  computations).
--------------------------------------------------------------}
newtype Read v a  = Read (Env v -> State -> Result a)
type    Records v = Array Int (Record v)
data    Result a  = Result a !State
data    Env v     = Env   !FilePath (Records v)
data    State     = State ![Byte] !NameSupply

unRead (Read r)   = r

runRead :: Read v (a,[(Record v)]) -> NameSupply -> FilePath -> [Byte]-> a
runRead (Read r) ns fname bs
  = let (Result (x,rs) st) = r (Env fname (listArray (1,length rs) rs)) (State bs ns)
    in x

instance Functor (Read v) where
  fmap f (Read r) = Read (\env st -> case r env st of
                                       Result x st -> Result (f x) st)
instance Monad (Read v) where
  return x        = Read (\rs bs -> Result x bs)
  (Read r) >>= f  = Read (\rs bs -> case r rs bs of
                                      Result x bsx -> unRead (f x) rs bsx) 


readRaw :: Read v Int
readRaw 
  = Read (\env (State bs ns) -> case int32FromByteList bs of (i,cs) -> Result i (State cs ns))

readByteList :: Int -> Read v [Byte]
readByteList n
  = Read (\env (State bs ns) -> case splitAt n bs of (xs,cs) -> Result xs (State cs ns))

skip n
  = Read (\env (State bs ns) -> Result () (State (drop n bs) ns))

readFreshId :: Read v Id
readFreshId
  = Read (\env (State bs ns) -> let (id,ns') = freshId ns in Result id (State bs ns'))
  
readGuard :: Bool -> String -> String -> Read v ()
readGuard test fun msg
  = if (test) then return () else readError fun msg

readError :: String -> String -> Read v a
readError fun msg
  = Read (\(Env fname rs) st -> error ("LvmRead." ++ fun ++ ": \"" ++ fname ++ "\"\n  " ++ msg))

resolve :: Int -> (Record v -> a) -> Read v a
resolve idx f
  = Read (\(Env fname rs) st -> Result (f (rs ! idx)) st)