module EH101.ConfigInstall where
import Data.List

ehcDefaultVariant = "101"

gccOpts = "-std=gnu99  -fomit-frame-pointer"

cppOpts = ""

ehcSvnRevision = "js@70b91ece18"

ehcDefaultInplaceInstallDir = "/Users/alessandro/Documents/Uni/uhc/EHC/install"

ehcPkgConfigfileName = "installed-pkg-config"

data WhatInstallFile = USER_PKG | INST_BIN | INST_LIB | INST_LIB_SHARED | INST_INCLUDE | INST_INCLUDE_SHARED | INST_LIB_PKG2 {- | INST_LIB_PKG | INST_LIB_PKG_INCLUDE -} 

mkCLibFilename dirprefix pkg = "" ++ dirprefix ++ "lib" ++ pkg ++ ".a"

mkJarFilename dirprefix pkg = "" ++ dirprefix ++ "lib" ++ pkg ++ ".jar"

mkJavaScriptLibFilename dirprefix pkg = "" ++ dirprefix ++ "lib" ++ pkg ++ ".mjs"

mkInternalPkgFileBase pkg variant target tvariant = "" ++ pkg ++ "/" ++ variant ++ "/" ++ target ++ "/" ++ tvariant ++ ""

mkPkgIncludeDir libdirprefix = "" ++ libdirprefix ++ "include"

mkDirbasedInstallPrefix dir what variant target pkg = case what of
  USER_PKG              -> dir ++ "/" ++ target
  INST_LIB              -> "" ++ dir ++ "/" ++ variant ++ "/lib/" ++ target ++ "/"
  INST_BIN              -> "" ++ dir ++ "/" ++ variant ++ "/bin/"
  INST_INCLUDE          -> "" ++ dir ++ "/" ++ variant ++ "/include/" ++ target ++ "/"
  INST_LIB_SHARED       -> "" ++ dir ++ "/" ++ variant ++ "/shared/lib/"
  INST_INCLUDE_SHARED   -> "" ++ dir ++ "/" ++ variant ++ "/shared/include/"
  INST_LIB_PKG2         -> "" ++ dir ++ "/" ++ variant ++ "/lib/pkg/"
