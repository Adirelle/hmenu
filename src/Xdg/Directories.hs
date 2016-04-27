{-# LANGUAGE NoImplicitPrelude #-}

module Xdg.Directories (
    listDirectories,
    findDirectories,
    findFile,
    findFileWith,
    DirectoryType(..)
) where

import           ClassyPrelude
import           Data.List.Split
import           System.Directory   (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Environment

data DirectoryType = DataHome | ConfigHome | CacheHome | RuntimeDir | DataDirs | ConfigDirs
                    deriving (Show, Eq)

findFileWith :: DirectoryType -> (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
findFileWith dirType f name = do
    dirs <- listDirectories dirType
    let paths = map (</> name) dirs
    files <- filterM fileFilter paths
    return $ headMay files
    where
        fileFilter :: FilePath -> IO Bool
        fileFilter p = (&&) <$> doesFileExist p <*> f p

findDirectories :: DirectoryType -> FilePath -> IO [FilePath]
findDirectories dirType name = listDirectories dirType >>= filterM doesDirectoryExist . map (</> name)

findFile :: DirectoryType -> FilePath -> IO (Maybe FilePath)
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
    filterM doesDirectoryExist [d]

multiDirs :: DirectoryType -> IO [FilePath]
multiDirs t = do
    d <- envOrDefault t
    filterM doesDirectoryExist $ splitOn ":" d

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
getDefault DataHome   = fmap (</> ".local/share") getHomeDirectory
getDefault ConfigHome = fmap (</> ".config") getHomeDirectory
getDefault CacheHome  = fmap (</> ".cache") getHomeDirectory
getDefault DataDirs   = return "/usr/local/share:/usr/share"
getDefault ConfigDirs = return "/etc/xdg"
getDefault t          = error $ "No default for " ++ show t
