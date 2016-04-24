{-# LANGUAGE DeriveGeneric #-}

module HMenu.Cache (setupCache) where

import           Control.DeepSeq
import           Data.Binary
import           Data.Hashable
import           System.Directory
import           System.FilePath

import           Xdg.Directories

type Cache a = MVar (CachedData a)

data CachedData a = CachedData Int a
    deriving (Eq, Show, Generic)

instance Binary a => Binary (CachedData a)

setupCache :: (Hashable b, Binary a, NFData a) => FilePath -> b -> (b -> a) -> IO (IO a)
setupCache name input func = do
    path <- cacheFilePath name
    cache <- newEmptyMVar
    chan <- newEmptyMVar
    fork $ retrieve cache path chan
    fork $ refresh cache chan path input func
    return $ reader cache
    where
        reader :: Cache a -> IO a
        reader cache = do
            CachedData h value <- readMVar cache
            return value

retrieve :: Binary a => Cache a -> FilePath -> MVar (Maybe (CachedData a)) -> IO ()
retrieve cache path chan = do
    maybeData <- readCache path
    case maybeData of
        Just v -> putMVar cache v
        _      -> return ()
    putMVar chan maybeData

refresh :: (Hashable b, Binary a, NFData a) => Cache a -> MVar (Maybe (CachedData a)) -> FilePath -> b -> (b -> a) -> IO ()
refresh cache chan path input func = do
    let h = hash input
    maybeData <- h `seq` readMVar chan
    case maybeData of
        Just (CachedData h' _) | h == h' ->
            return ()
        _ -> do
            _ <- tryTakeMVar cache
            let value = func input
                freshData = value `deepseq` CachedData h value
            putMVar cache freshData
            writeCache path freshData

writeCache :: Binary a => FilePath ->CachedData a -> IO ()
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
