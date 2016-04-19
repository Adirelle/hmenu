{-# LANGUAGE OverloadedStrings #-}

module HMenu.ScanDirs (
    scanDirs
) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           System.Directory
import           System.FilePath

import           HMenu.Types

scanDirs :: (FilePath -> IO Bool) -> (FilePath -> IO [Entry]) -> [FilePath] -> IO [Entry]
scanDirs fileFilter converter dirs = do
    files <- listFiles fileFilter dirs
    entries <- mapM converter files
    return $ concat entries

type FileMap = M.Map FilePath FilePath

listFiles :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
listFiles fileFilter dirs = do
    files <- execStateT go M.empty
    return $ M.elems files
    where
        go = forM_ dirs $ \dir -> do
            files <- liftIO $ getDirectoryContents dir
            forM_ files $ \name -> do
                let path = dir </> name
                exists <- liftIO $ doesFileExist path
                when exists $ do
                    valid  <- liftIO $ fileFilter path
                    when valid $ modify' $ M.insert name path
