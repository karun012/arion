module Arion.Utilities (
    associate,
    dependencies,
    findHaskellFiles
) where

import           Arion.Types
import           Control.Applicative ((<*>))
import           Data.List   (nub, sort, union, isInfixOf)
import           Data.Map    (Map, fromList)
import           System.FilePath.Find      (always, extension, find, (==?),
                                            (||?))

associate :: [SourceFile] -> [TestFile] -> Map FilePath [TestFile]
associate sourceFiles testFiles = let sourcesAndDependencies = dependencies sourceFiles
                                  in fromList $ map (\(source, dependencies) ->
                                                                    let testFilesFor source = filter (\testFile -> moduleName source `elem` imports testFile) testFiles
                                                                        testFilesForSource = testFilesFor source
                                                                        testFilesForDependencies = concatMap testFilesFor dependencies
                                                                    in (sourceFilePath source, testFilesForSource ++ testFilesForDependencies)
                                                    ) sourcesAndDependencies

dependencies :: [SourceFile] -> [(SourceFile, [SourceFile])]
dependencies sourceFiles = map (\file -> let dependencies = transitiveDependencies sourceFiles [] file
                                         in (file, nub $ (filter ((/=) file) dependencies))
                            ) sourceFiles

transitiveDependencies :: [SourceFile] -> [SourceFile] -> SourceFile -> [SourceFile]
transitiveDependencies allSourceFiles sourcesThatIHaveSeenSoFar theSourceFile =
                                                let sourcesThatImportMe = sourcesThatImport allSourceFiles (moduleName theSourceFile)
                                                in case any (\source -> source `elem` sourcesThatIHaveSeenSoFar) sourcesThatImportMe of
                                                     True -> sourcesThatImportMe
                                                     False -> let soFar = sourcesThatIHaveSeenSoFar ++ [theSourceFile]
                                                              in sourcesThatImportMe ++ concatMap (transitiveDependencies allSourceFiles soFar) sourcesThatImportMe


findSourcesByModule :: [SourceFile] -> String -> [SourceFile]
findSourcesByModule sourceFiles theModuleName = filter (\file -> moduleName file == theModuleName) sourceFiles

sourcesThatImport :: [SourceFile] -> String -> [SourceFile]
sourcesThatImport sourceFiles theModuleName = filter (\file -> theModuleName `elem` (importedModules file)) sourceFiles

findHaskellFiles :: String -> IO [String]
findHaskellFiles path = do
    files <- find always (extension ==? ".hs" ||? extension ==? ".lhs") path
    return $ filter (not . isInfixOf ".#") files
