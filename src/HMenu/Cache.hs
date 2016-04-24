module HMenu.Cache (cacheFetch, cacheStore) where

import           Data.Binary
import           Data.Hashable
import           System.Directory
import           System.FilePath

import           Xdg.Directories

cacheStore :: Binary a => FilePath -> Int -> a -> IO ()
cacheStore n h d = do
    filepath <- cacheFilePath n
    ensureParent filepath
    encodeFile filepath (h, d)

cacheFetch :: Binary a => FilePath -> IO (Maybe (Int, a))
cacheFetch n = do
    filepath <- cacheFilePath n
    exists <- doesFileExist filepath
    if exists then do
        result <- decodeFileOrFail filepath
        return $ case result of
            Right d@(_, _) -> Just d
            _              -> Nothing
    else
        return Nothing

cacheFilePath :: FilePath -> IO FilePath
cacheFilePath n = do
    cacheDirs <- listDirectories CacheHome
    let cacheDir = headEx cacheDirs
    return $ cacheDir </> "hmenu" </> n <.> ".bin"

ensureParent :: FilePath -> IO ()
ensureParent "/" = return ()
ensureParent "." = return ()
ensureParent path = do
    let parent = takeDirectory path
    ensureParent parent
    unlessM (doesDirectoryExist parent) $ createDirectory parent
