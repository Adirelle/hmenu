{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.ScanDirs (
    scanDirs
) where

import           Control.Monad.State (StateT (..), execStateT, modify')
import           ClassyPrelude             hiding (Map)
import           System.Directory    (doesDirectoryExist, doesFileExist, getDirectoryContents)

import           HMenu.Types

scanDirs :: (FilePath -> IO Bool) -> (FilePath -> IO [Entry]) -> [FilePath] -> IO [Entry]
scanDirs fileFilter converter dirs = do
    files <- listFiles fileFilter dirs
    entries <- mapM converter files
    return $ concat entries

type FileMap = HashMap FilePath FilePath

listFiles :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
listFiles fileFilter dirs = do
    files <- execStateT go mempty
    return $ toList files
    where
        go :: StateT FileMap IO ()
        go = forM_ dirs listDir
        listDir :: FilePath -> StateT FileMap IO ()
        listDir dir = do
            files <- liftIO $ getDirectoryContents dir
            forM_ files $ checkFile dir
        checkFile :: FilePath -> FilePath -> StateT FileMap IO ()
        checkFile dir name = do
            let path = dir </> name
            isValid <- liftIO $ actualFilter path
            when isValid $ modify' $ insertMap name path
        actualFilter :: FilePath -> IO Bool
        actualFilter p = (&&) <$> doesFileExist p <*> fileFilter p
