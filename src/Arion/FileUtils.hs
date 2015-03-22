module Arion.FileUtils where

import Arion.Types
import Data.Map (Map, fromList)

associate :: [SourceFile] -> [TestFile] -> Map FilePath [TestFile]
associate sourceFiles testFiles = fromList $ map (createAssociations testFiles) sourceFiles

createAssociations :: [TestFile] -> SourceFile -> (FilePath, [TestFile])
createAssociations testFiles sourceFile = let associatedTestFiles = filter (\testFile -> moduleName sourceFile `elem` imports testFile) testFiles
                                          in (sourceFilePath sourceFile, associatedTestFiles)
