{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.ScanDirs (
    scanDirs
) where

import           ClassyPrelude       hiding (Map)
import           Control.Monad.State (StateT (..), execStateT, modify')
import           System.Directory    (doesDirectoryExist, doesFileExist, getDirectoryContents)

import           HMenu.Types

type FileMap = HashMap FilePath FilePath

scanDirs :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
scanDirs fileFilter dirs = do
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
