{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Provider.Path (
    pathProvider
) where

import           ClassyPrelude
import           Data.Hashable
import           System.Directory
import           System.FilePath

import           HMenu.Command
import           HMenu.Provider.Types
import           HMenu.ScanDirs
import           HMenu.Types

pathProvider :: IO EntryProvider
pathProvider = do
    dirs <- filterM doesDirectoryExist =<< getSearchPath
    fileBasedProvider dirs isExecutable hashFilePaths createEntry
    where
        isExecutable path = catchAny
            (executable <$> getPermissions path)
            (const (return False))

hashFilePaths :: [FilePath] -> IO Int
hashFilePaths ps = return $ foldr (flip hashWithSalt) 0 ps

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
