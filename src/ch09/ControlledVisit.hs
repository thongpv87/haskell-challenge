import System.Directory (doesDirectoryExist
                        , getDirectoryContents
                        , getPermissions
                        , getModificationTime
                        , Permissions)
import System.FilePath ((</>))
import Control.Monad (forM, mapM, liftM)
import Data.List
import Data.Maybe
import Data.Time.Clock
import System.IO
import Control.Exception(handle, bracket, SomeException)

data Info = Info {
  infoPath :: FilePath
  , inforPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (oder contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then Main.traverse order (infoPath info)
      else return [info]
  return contents

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handler (Just `liftM` act)
  where handler :: SomeException->IO (Maybe a)
        handler _ = return Nothing

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

main = do
  names <- Main.traverse id "."
  putStrLn . intercalate "\n" . fmap show $  names

