{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Provider.Path (
    listPathEntries
) where

import           ClassyPrelude
import           System.Directory
import           System.FilePath

import           HMenu.Command
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
                eCommand = ShellCommand packedPath,
                eTitle   = name,
                eComment = Just packedPath,
                eIcon    = Nothing
           }]
