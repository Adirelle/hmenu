{-# LANGUAGE OverloadedStrings #-}

module Application.FreeDesktop where

import           Application.FreeDesktop.Parser
import           Application.Types
import           Control.Monad
import           Data.Maybe
import           Data.Text                      (Text, isPrefixOf)
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
    concat <$> mapM readEntry desktopFiles
    where
        isDesktopFile = (".desktop" ==) . takeExtension

listFiles :: FilePath -> IO [FilePath]
listFiles dir =
    listDirectory dir >>=
    filterM doesFileExist . map (dir </>)

readEntry :: FilePath -> IO [Entry]
readEntry path = fmap parseEntry (DTI.readFile path)

parseEntry :: Text -> [Entry]
parseEntry content =
    case parseDesktopEntry content of
        Just groups -> mapMaybe parseGroup groups
        Nothing     -> []
    where
        parseGroup (key, values) | isActionKey key = parseAction values
                                 | otherwise       = Nothing
        isActionKey k = "Desktop Entry" == k
                        || "Desktop Action " `isPrefixOf` k

parseAction :: Group -> Maybe Entry
parseAction group = do
    command      <- lookupText "Exec" group
    title        <- lookupText "Name" group
    let comment   = lookupText "Comment" group
        icon      = lookupText "Icon" group
    return $ Entry command title comment icon
