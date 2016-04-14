{-# LANGUAGE OverloadedStrings #-}

module Application.FreeDesktop where

import           Application.FreeDesktop.Parser
import           Application.Types
import           Control.Monad
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text.IO                   as DTI
import           Data.Time
import           System.Directory
import           System.FilePath

listDesktopEntries :: IO [Entry]
listDesktopEntries = do
    userApps <- getXdgDirectory XdgData "applications"
    paths <- filterM doesDirectoryExist ["/usr/share/applications", userApps]
    scanEntries paths

scanEntries :: [FilePath] -> IO [Entry]
scanEntries dirs = do
    files <- concat <$> mapM listFiles dirs
    let desktopFiles = filter isDesktopFile files
    catMaybes <$> mapM readEntry desktopFiles

listFiles :: FilePath -> IO [FilePath]
listFiles dir =
    listDirectory dir >>=
    filterM doesFileExist . map (dir </>)

isDesktopFile :: FilePath -> Bool
isDesktopFile = (".desktop" ==) . takeExtension

readEntry :: FilePath -> IO (Maybe Entry)
readEntry path = parseEntry <$> DTI.readFile path

parseEntry :: Text -> Maybe Entry
parseEntry content = do
    desktopEntry <- parseDesktopEntry content
    group        <- lookupGroup "Desktop Entry" desktopEntry
    command      <- lookupText "Exec" group
    title        <- lookupText "Name" group
    let comment   = lookupText "Comment" group
        icon      = lookupText "Icon" group
    return $ Entry command title comment icon
