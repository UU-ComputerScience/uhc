{-| Module      :  Main
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}
module Helium.Compiler.Main where

import Helium.Compiler.Compile(compile)
import Helium.Parser.Parser(parseOnlyImports)

import List(nub, elemIndex, isSuffixOf, intersperse)
import Maybe(fromJust, isNothing)
import Lvm.Common.Standard(searchPathMaybe,getLvmPath, splitPath)
import Directory(doesFileExist, getModificationTime)
import Helium.Compiler.CompileUtils
import Data.IORef
import Helium.Compiler.Args
import Helium.Utils.Utils
import Lvm.Common.TopSort

main :: IO ()
main = do
    args                <- getArgs
    (options, fullName) <- processArgs args
    
    lvmPathFromOptionsOrEnv <- case lvmPathFromOptions options of 
        Nothing -> getLvmPath
        Just s  -> return (splitPath s)
    
    let a@(filePath, moduleName, extension) = splitFilePath fullName
        lvmPath = filter (not.null) . nub 
                $ (if null filePath then "." else filePath) : lvmPathFromOptionsOrEnv

    -- File must exist, this test doesn't use the search path
    fileExists <- doesFileExist fullName
    newFullName <- 
        if fileExists then 
            return fullName
        else do
            let filePlusHS = fullName ++ ".hs"
            filePlusHSExists <- doesFileExist filePlusHS
            when (not filePlusHSExists) $ do
                putStrLn $ "Can't find file " ++ show fullName ++ " or " ++ show filePlusHS
                exitWith (ExitFailure 1)
            return filePlusHS

    -- Libraries must exist somewhere in the search path
    mapM (checkExistence lvmPath) 
        ["Prelude", "PreludePrim", "HeliumLang", "LvmLang", "LvmIO", "LvmException"]

{-
    verticesRef <- newIORef []
    fullNamesRef <- newIORef []
    edgesRef <- newIORef []
    
    buildImportGraph newFullName lvmPath fullNamesRef verticesRef edgesRef
    
    vertices <- readIORef verticesRef
    fullNames <- readIORef fullNamesRef
    edges <- readIORef edgesRef
    
    let groups = generalTopSort vertices edges 
    
    putStrLn ("FullNames " ++ show fullNames)
    putStrLn ("Vertices " ++ show vertices)
    putStrLn ("Edges " ++ show edges)
    putStrLn ("Groups " ++ show groups)
-}

    doneRef <- newIORef []
    make newFullName lvmPath [moduleName] options doneRef
    return ()

{-
generalTopSort :: Eq a => [a] -> [(a, a)] -> [[a]]
generalTopSort vertices edges = 
    let indexOf vertex = fromJust (elemIndex vertex vertices)
        numberedEdges = map (\(v1, v2) -> (indexOf v1, indexOf v2)) edges
    in map (map (vertices !!)) (
        topSort (length vertices - 1) numberedEdges)
    
addIORef :: a -> IORef [a] -> IO ()
addIORef x ioRef = do 
    xs <- readIORef ioRef
    writeIORef ioRef (x:xs)
    
buildImportGraph :: String -> [String] -> 
                    IORef [(String, String)] ->
                    IORef [String] ->
                    IORef [(String, String)] -> IO ()
buildImportGraph fullName lvmPath fullNamesRef verticesRef edgesRef = do
    let (_, moduleName, _) = splitFilePath fullName
    vertices <- readIORef verticesRef
    when (moduleName `notElem` vertices) $ do
        addIORef (moduleName, fullName) fullNamesRef
        addIORef moduleName verticesRef

        imports <- parseOnlyImports fullName
        foreach imports $ \importModuleName -> do
            maybeImportFullName <- resolve lvmPath importModuleName
            
            when (isNothing maybeImportFullName) $ do
                putStrLn $ 
                    "Unable to find module " ++ importModuleName ++  
                    "\nimported from module " ++ fullName ++
                    "\nin the search path\n" ++ showSearchPath lvmPath
                exitWith (ExitFailure 1)

            let Just importFullName = maybeImportFullName

            -- We don't look at imports if we only have an LVM 
            unless (".lvm" `isSuffixOf` importFullName) $ do
                addIORef (moduleName, importModuleName) edgesRef
                buildImportGraph importFullName lvmPath fullNamesRef verticesRef edgesRef    
        return ()
-}

-- fullName = file name including path of ".hs" file that is to be compiled
-- lvmPath = where to look for files
-- chain = chain of imports that led to the current module
-- options = the compiler options
-- doneRef = an IO ref to a list of already compiled files
--                        (their names and whether they were recompiled or not)

-- returns: recompiled or not? (true = recompiled)

make :: String -> [String] -> [String] -> [Option] -> IORef [(String, Bool)] -> IO Bool
make fullName lvmPath chain options doneRef =
    do
        -- If we already compiled this module, return the result we already now
        done <- readIORef doneRef
        case lookup fullName done of 
          Just isRecompiled -> return isRecompiled
          Nothing -> do
            imports <- parseOnlyImports fullName
            
            -- If this module imports a module earlier in the chain, there is a cycle
            case circularityCheck imports chain of
                Just cycle -> do
                    putStrLn $ "Circular import chain: \n\t" ++ showImportChain cycle ++ "\n"
                    exitWith (ExitFailure 1)
                Nothing -> 
                    return ()

            -- Find all imports in the search path
            resolvedImports <- mapM (resolve lvmPath) imports
            
            -- For each of the imports...
            compileResults <- foreach (zip imports resolvedImports) 
              $ \(importModuleName, maybeImportFullName) -> do

                -- Issue error if import can not be found in the search path
                case maybeImportFullName of
                    Nothing -> do
                        putStrLn $ 
                            "Can't find module '" ++ importModuleName ++ "'\n" ++ 
                            "Import chain: \n\t" ++ showImportChain (chain ++ [importModuleName]) ++
                            "\nSearch path:\n" ++ showSearchPath lvmPath
                        exitWith (ExitFailure 1)
                    Just _ -> return ()

                let importFullName = fromJust maybeImportFullName

                -- If we only have an ".lvm" file we do not need to (/can't) recompile 
                if ".lvm" `isSuffixOf` importFullName then
                    return False
                  else
                    make importFullName lvmPath (chain ++ [importModuleName]) options doneRef

            -- Recompile the current module if:
            --  * any of the children was recompiled
            --  * the build all option (-B) was on the command line
            --  * the build one option (-b) was there and we are 
            --      compiling the top-most module (head of chain)
            --  * the module is not up to date (.hs newer than .lvm)
            let (filePath, moduleName, _) = splitFilePath fullName
            upToDate <- upToDateCheck (combinePathAndFile filePath moduleName)
            newDone <- readIORef doneRef
            isRecompiled <- 
                if  any (==True) compileResults || 
                    BuildAll `elem` options || 
                    (BuildOne `elem` options && moduleName == head chain) ||
                    not upToDate 
                    then do
                        compile fullName options lvmPath (map fst newDone)
                        return True
                      else do
                        putStrLn (moduleName ++ " is up to date")
                        return False
            
            -- Remember the fact that we have already been at this module
            writeIORef doneRef ((fullName, isRecompiled):newDone)
            return isRecompiled
            
showImportChain :: [String] -> String
showImportChain = concat . intersperse " imports "

showSearchPath :: [String] -> String
showSearchPath = unlines . map ("\t" ++)

circularityCheck :: [String] -> [String] -> Maybe [String]
circularityCheck (import_:imports) chain =
    case elemIndex import_ chain of
        Just index -> Just (drop index chain ++ [import_])
        Nothing -> circularityCheck imports chain
circularityCheck [] chain = Nothing

foreach = flip mapM

-- | upToDateCheck returns true if the .lvm is newer than the .hs
upToDateCheck :: String -> IO Bool
upToDateCheck basePath = do
    lvmExists <- doesFileExist (basePath ++ ".lvm")
    if lvmExists then do
        t1 <- getModificationTime (basePath ++ ".hs")
        t2 <- getModificationTime (basePath ++ ".lvm")
        return (t1 < t2)
     else
        return False
