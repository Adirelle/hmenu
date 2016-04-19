{-# LANGUAGE OverloadedStrings #-}

module HMenu.Path (
    listPathEntries
) where

import           Control.Monad
import           Data.Text        (pack)
import           HMenu.ScanDirs
import           HMenu.Types
import           System.Directory
import           System.FilePath

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
