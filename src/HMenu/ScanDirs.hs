{-# LANGUAGE OverloadedStrings #-}

module HMenu.ScanDirs (
    scanDirs
) where

import           Control.Monad    (filterM, liftM2, mapM)
import           HMenu.Types      (Entry)
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath  ((</>))

scanDirs :: (FilePath -> IO Bool) -> (FilePath -> IO [Entry]) -> [FilePath] -> IO [Entry]
scanDirs fileFilter converter dirs = do
    existingDirs <- filterM doesDirectoryExist dirs
    files <- concat <$> mapM listDirFiles existingDirs
    concat <$> mapM converter files
    where
        listDirFiles path = do
            names <- map (path </>) <$> listDirectory path
            filterM keepFile names
        keepFile path = doesFileExist path <&&> fileFilter path
        (<&&>) = liftM2 (&&)
