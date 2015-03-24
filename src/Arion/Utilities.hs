module Arion.Utilities where

import Arion.Types
import Data.Map (Map, fromList)
import Data.List

associate :: [SourceFile] -> [TestFile] -> Map FilePath [TestFile]
associate sourceFiles testFiles = let preTransitive = map (createAssociations testFiles) sourceFiles
                                      transitive = map (\(sourceFile, testsAssociatedWithSource) -> let imports = importedModules sourceFile
                                                                                                        testsAssociatedWithImports =
                                                                                                            concatMap snd $ concatMap (\imp -> filter (\(source, tests) -> moduleName source == imp) preTransitive) imports
                                                                                                    in (sourceFilePath sourceFile, testsAssociatedWithSource `union` testsAssociatedWithImports)
                                                       ) preTransitive
                                  in fromList transitive

createAssociations :: [TestFile] -> SourceFile -> (SourceFile, [TestFile])
createAssociations testFiles sourceFile = let associatedTestFiles = filter (\testFile -> moduleName sourceFile `elem` imports testFile) testFiles
                                          in (sourceFile, associatedTestFiles)

