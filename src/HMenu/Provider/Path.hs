{-# LANGUAGE OverloadedStrings #-}

module HMenu.Provider.Path (
    listPathEntries
) where

import           System.Directory
import           System.FilePath

import           HMenu.ScanDirs
import           HMenu.Types

listPathEntries :: IO [Entry]
listPathEntries =
    getSearchPath >>= filterM doesDirectoryExist >>= scanDirs isExecutable createEntry
    where isExecutable path = executable <$> getPermissions path

createEntry :: FilePath -> IO [Entry]
createEntry filepath = do
    let packedPath = pack filepath
        name       = pack $ takeBaseName filepath
    return [Entry {
                command = packedPath,
                title   = name,
                comment = Just packedPath,
                icon    = Nothing
           }]
