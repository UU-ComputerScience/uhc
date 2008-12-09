{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: LvmImport.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.LvmImport( lvmImport, lvmImportDecls ) where


import Monad    ( foldM )
import Lvm.Common.Standard ( foldlStrict)
import Lvm.Common.Id       ( Id, stringFromId )
import Lvm.Common.IdMap    ( IdMap, emptyMap, insertMap, elemMap, updateMap, listFromMap, lookupMap, findMap, mapMap  )

import Lvm.Lvm.Module as Module
import Lvm.Lvm.Lvm
import Lvm.Lvm.LvmRead  ( lvmReadFile )

import Lvm.Lvm.ModulePretty
import Lvm.Lvm.InstrPretty
import Lvm.Common.PPrint

import System

-- Helium
import Lvm.Common.Byte hiding (cat)
import Lvm.Common.Id

{--------------------------------------------------------------
  lvmImport: replace all import declarations with
  abstract declarations or constructors/externs/customs
--------------------------------------------------------------}
lvmImport :: (Id -> IO FilePath) -> (Module v) -> IO (Module v)
lvmImport findModule mod
  = do{ mods <- lvmImportModules findModule mod
      ; let mods0 = lvmExpandModule mods (moduleName mod) 
            mods1 = lvmResolveImports mods0
            mod1  = findMap (moduleName mod) mods1
      ; return mod1{ moduleDecls = filter (not . isDeclImport) (moduleDecls mod1) }
      }

lvmImportDecls :: (Id -> IO FilePath) -> [Decl v] -> IO [[Decl v]]
lvmImportDecls findModule importDecls =
    mapM
        (\importDecl -> do
            mod <- lvmImport findModule $
                Module.Module
                    { Module.moduleName     = idFromString "Main"
                    , Module.moduleMajorVer = 0
                    , Module.moduleMinorVer = 0
                    , Module.moduleDecls    = [importDecl]
                    }
            return (moduleDecls mod)
        )
        importDecls

{--------------------------------------------------------------
  lvmImportModules: 
    recursively read all imported modules
--------------------------------------------------------------}
lvmImportModules :: (Id -> IO FilePath) -> (Module v) -> IO (IdMap (Module v))
lvmImportModules findModule mod
  = readModuleImports findModule emptyMap (moduleName mod) mod
    
readModuleImports :: (Id -> IO FilePath) -> IdMap (Module v) -> Id -> (Module v) -> IO (IdMap (Module v))
readModuleImports findModule loaded id mod
  = foldM (readModule findModule) (insertMap id mod loaded) (imported mod)

readModule :: (Id -> IO FilePath) -> IdMap (Module v) -> Id -> IO (IdMap (Module v))
readModule findModule loaded id
  | elemMap id loaded  = return loaded
  | otherwise          = do{ fname <- findModule id                        
                           ; mod   <- lvmReadFile fname
                           ; readModuleImports findModule loaded id (filterPublic mod)
                           }

imported mod
  = [importModule (declAccess d) | d <- moduleDecls mod, isDeclImport d]

{--------------------------------------------------------------
  lvmExpandModule loaded modname: 
    expand Module import declarations of [modname] 
    into declarations for all items exported from that module.
--------------------------------------------------------------}
lvmExpandModule :: IdMap (Module v) -> Id -> IdMap (Module v)
lvmExpandModule loaded modname
  = mapMap expand loaded
  where
    expand mod  | moduleName mod == modname  = expandModule loaded mod
                | otherwise                  = mod

expandModule :: IdMap (Module v) -> Module v -> Module v
expandModule loaded mod
  = mod{ moduleDecls = concatMap (expandDecl loaded (moduleName mod)) (moduleDecls mod) }

expandDecl loaded modname DeclImport{declAccess = access@(Imported{importModule = imodname,importKind = DeclKindModule})}
  = case lookupMap imodname loaded of
      Nothing   -> error ("LvmImport.expandDecl: import module is not loaded: " ++ stringFromId modname)
      Just imod | moduleName imod == modname 
                -> error ("LvmImport.expandDecl: module imports itself: " ++ stringFromId modname)
      Just imod -> map importDecl (moduleDecls imod)
  where
    importDecl decl
      = decl{ declAccess = access{importName = declName decl, importKind = declKindFromDecl decl} }

expandDecl loaded modname decl
  = [decl]

{---------------------------------------------------------------
lvmResolveImports:
  replaces all "DImport" declarations with the real
  declaration (except the access is Import). This is always
  needed for all modules.
---------------------------------------------------------------}
lvmResolveImports :: IdMap (Module v) -> IdMap (Module v)
lvmResolveImports mods
  = foldlStrict resolveImports mods (listFromMap mods)

resolveImports :: IdMap (Module v) -> (Id,Module v) -> IdMap (Module v)
resolveImports loaded (modid,mod)
  = foldlStrict (resolveImport [] modid) loaded (filter isDeclImport (moduleDecls mod))

resolveImport :: [Id] -> Id -> IdMap (Module v) -> Decl v -> IdMap (Module v)
resolveImport visited modid loaded x@(DeclImport id access@(Imported public imodid impid kind major minor) customs)
  | elem modid visited = error ("LvmImport.resolveImport: circular import chain: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
  | otherwise = 
    let mod = findMap modid loaded in 
    case lookupMap imodid loaded of
      Nothing   -> error ("LvmImport.resolveImport: import module is not loaded: " ++ stringFromId imodid)
      Just imod -> case lookupDecl impid kind (moduleDecls imod) of
                     []   -> notfound imodid impid
                     ds   -> case filter (not . isDeclImport) ds of
                               []  -> case filter isDeclImport ds of
                                        []  -> notfound imodid impid
                                        [d] -> let loaded' = resolveImport (modid:visited) imodid loaded d
                                               in resolveImport (imodid:visited) modid loaded' x
                                        ds  -> ambigious imodid impid
                               [d] -> update mod{ moduleDecls = d{declName=id,declAccess = access} : (moduleDecls mod)}
                               ds  -> ambigious imodid impid
  where
    lookupDecl impid kind decls
      = [d | d <- decls, declName d==impid && declKindFromDecl d == kind]

    update mod'
      = updateMap modid mod' loaded
        
    notfound imodid impid
      = error ("LvmImport.resolveImport: unresolved identifier: " ++ stringFromId imodid ++ "." ++ stringFromId impid)

    ambigious imodid impid
      = error ("LvmImport.resolveImport: ambigious import record: " ++ stringFromId imodid ++ "." ++ stringFromId impid)
