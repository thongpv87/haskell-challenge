-- module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name-> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

simpleFind :: (FilePath->Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return $ filter p names

main :: IO()
main = do
  paths <- getRecursiveContents "."
  paths2 <- simpleFind (\name-> name `elem`  map ("./"++) ["default.nix", "shell.nix"]) "."
  putStrLn $ show paths2
