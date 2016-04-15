{-# LANGUAGE OverloadedStrings #-}

module HMenu.Path (
    listPathEntries
) where

import           Control.Monad
import           Data.Text          (pack)
import           HMenu.Types
import           System.Directory
import           System.Environment
import           System.FilePath

listPathEntries :: IO [Entry]
listPathEntries = do
    paths <- getSearchPath >>= filterM doesDirectoryExist
    entries <- mapM listExecutables paths
    return $ Prelude.concat entries

listExecutables :: FilePath -> IO [Entry]
listExecutables path =
    map wrapBinary `fmap` (listDirectory path >>= filterM isExecutable)
    where
        isExecutable name = do
            let filepath = path </> name
            exists <- doesFileExist filepath
            perms <- getPermissions filepath
            return $ exists && executable perms
        wrapBinary name = Entry {
                            command = pack (path </> name),
                            title = pack name,
                            comment = Nothing,
                            icon = Nothing
                          }
