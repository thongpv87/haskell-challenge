import System.Directory
import System.Directory
import System.FilePath ((</>))
import Control.Monad (forM, mapM, liftM)
import Data.List
import Data.Maybe
import Data.Time.Clock
import System.IO

data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >= walk seed
