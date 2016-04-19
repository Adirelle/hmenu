module Xdg.Directories (
    listDirectories,
    findDirectories,
    findFile,
    findFileWith,
    DirectoryType(..)
) where

import           Control.Monad      (filterM, liftM2)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import qualified System.Directory   as D
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>))

data DirectoryType = DataHome | ConfigHome | CacheHome | RuntimeDir | DataDirs | ConfigDirs
                    deriving (Show, Eq)

findFileWith :: DirectoryType -> (FilePath -> IO Bool) -> FilePath -> IO FilePath
findFileWith dirType f name = fmap head (listDirectories dirType >>= filterM fileFilter . map (</> name))
                                where fileFilter p = (&&) <$> D.doesFileExist p <*> f p

findDirectories :: DirectoryType -> FilePath -> IO [FilePath]
findDirectories dirType name = listDirectories dirType >>= filterM D.doesDirectoryExist . map (</> name)

findFile :: DirectoryType -> FilePath -> IO FilePath
findFile t = findFileWith t (\_ -> return True)

listDirectories :: DirectoryType -> IO [FilePath]
listDirectories DataHome   = singleDir DataHome
listDirectories ConfigHome = singleDir ConfigHome
listDirectories CacheHome  = singleDir CacheHome
listDirectories RuntimeDir = singleDir RuntimeDir
listDirectories DataDirs   = liftM2 (++) (singleDir DataHome)   (multiDirs DataDirs)
listDirectories ConfigDirs = liftM2 (++) (singleDir ConfigHome) (multiDirs ConfigDirs)

singleDir :: DirectoryType -> IO [FilePath]
singleDir t = do
    d <- envOrDefault t
    filterM D.doesDirectoryExist [d]

multiDirs :: DirectoryType -> IO [FilePath]
multiDirs t = do
    d <- envOrDefault t
    filterM D.doesDirectoryExist $ splitOn ":" d

envOrDefault :: DirectoryType -> IO String
envOrDefault t = do
    v <- lookupEnv $ variableFor t
    case v of
        Nothing -> getDefault t
        Just  p -> return p

variableFor :: DirectoryType -> String
variableFor DataHome   = "XDG_DATA_HOME"
variableFor ConfigHome = "XDG_CONFIG_HOME"
variableFor CacheHome  = "XDG_CACHE_HOME"
variableFor RuntimeDir = "XDG_RUNTIME_DIR"
variableFor DataDirs   = "XDG_DATA_DIRS"
variableFor ConfigDirs = "XDG_CONFIG_DIRS"

getDefault :: DirectoryType -> IO FilePath
getDefault DataHome   = fmap (</> ".local/share") D.getHomeDirectory
getDefault ConfigHome = fmap (</> ".config") D.getHomeDirectory
getDefault CacheHome  = fmap (</> ".cache") D.getHomeDirectory
getDefault DataDirs   = return "/usr/local/share:/usr/share"
getDefault ConfigDirs = return "/etc/xdg"
getDefault t          = error $ "No default for " ++ show t
