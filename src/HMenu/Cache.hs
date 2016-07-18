{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Cache where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Binary
import           Data.Hashable
import           System.Directory
import           System.FilePath

import           XDG.Directories

readCache :: Binary a => FilePath -> IO (Maybe a)
readCache p = do
    r <- try (decodeFile p :: Binary a => IO a)
    return $ either onError Just r
    where
        onError :: SomeException -> Maybe a
        onError _ = Nothing

writeCache :: Binary a => FilePath -> a -> IO ()
writeCache p d = do
    ensureParent p
    encodeFile p d
    where
        ensureParent :: FilePath -> IO ()
        ensureParent "/" = return ()
        ensureParent "." = return ()
        ensureParent path = do
            let parent = takeDirectory path
            ensureParent parent
            unlessM (doesDirectoryExist parent) $ createDirectory parent

cacheFilePath :: FilePath -> IO FilePath
cacheFilePath n = do
    cacheDirs <- listDirectories CacheHome
    let cacheDir = headEx cacheDirs
    return $ cacheDir </> "hmenu" </> n <.> ".bin"
