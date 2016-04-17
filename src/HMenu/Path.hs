{-# LANGUAGE OverloadedStrings #-}

module HMenu.Path (
    listPathEntries
) where

import           Data.Text        (pack)
import           HMenu.ScanDirs   (scanDirs)
import           HMenu.Types      (Entry (..))
import           System.Directory (executable, getPermissions)
import           System.FilePath  (getSearchPath, takeBaseName)

listPathEntries :: IO [Entry]
listPathEntries = do
    paths <- getSearchPath
    scanDirs isExecutable createEntry paths
    where isExecutable path = executable <$> getPermissions path

createEntry :: FilePath -> IO [Entry]
createEntry filepath = do
    let packedPath = pack filepath
        name      = pack $ takeBaseName filepath
    return [Entry {
                command = packedPath,
                title   = name,
                comment = Just packedPath,
                icon    = Nothing
           }]
