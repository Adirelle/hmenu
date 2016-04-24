{-# LANGUAGE DeriveGeneric #-}

module HMenu.Cache (setupCache) where

import           Control.Concurrent.Async
import           Control.DeepSeq
import           Data.Binary
import           Data.Hashable
import           System.Directory
import           System.FilePath

import           Xdg.Directories

data CachedData a = CachedData Int a
    deriving (Eq, Show, Generic)

instance Binary a => Binary (CachedData a)

setupCache :: (Hashable b, Binary a, NFData a) => FilePath -> b -> (b -> a) -> IO (IO a)
setupCache name input func = do
    path <- cacheFilePath name
    cache <- newEmptyMVar
    setupCache' path cache input func
    return $ readMVar cache

setupCache' :: (Hashable b, Binary a, NFData a) => FilePath -> MVar a -> b -> (b -> a) -> IO ()
setupCache' path var input func = do
    aHash <- async calcHash
    aCachedHash <- async retrieve
    void $ async $ refresh aHash aCachedHash
    where
        calcHash = do
            let value = hash input
            value `seq` return value
        retrieve = do
            input <- readCache path
            case input of
                Just (CachedData h value) -> do
                    putMVar var value
                    return $ Just h
                _ ->
                    return Nothing
        refresh aHash aCachedHash = do
            (current, maybeOld) <- waitBoth aHash aCachedHash
            case maybeOld of
                Just old | current == old ->
                    return ()
                Nothing -> do
                    _ <- tryReadMVar var
                    let value = func input
                    value `deepseq` putMVar var value
                    writeCache path $ CachedData current value

writeCache :: Binary a => FilePath -> CachedData a -> IO ()
writeCache path dataToCache = ensureParent path >> encodeFile path dataToCache

readCache :: Binary a => FilePath -> IO (Maybe (CachedData a))
readCache path = do
    exists <- doesFileExist path
    if exists then do
        result <- decodeFileOrFail path
        return $ case result of
            Right v -> Just v
            _       -> Nothing
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
