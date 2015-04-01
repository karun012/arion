module Arion.Utilities (
    associate,
    dependencies
) where

import           Arion.Types
import           Data.List   (nub, sort, union)
import           Data.Map    (Map, fromList)

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
