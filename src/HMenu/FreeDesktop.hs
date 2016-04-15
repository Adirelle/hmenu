{-# LANGUAGE OverloadedStrings #-}

module HMenu.FreeDesktop (
    listDesktopEntries
) where

import           HMenu.FreeDesktop.Parser
import           HMenu.FreeDesktop.Types
import           HMenu.Types
import           Control.Monad
import           Data.Locale
import           Data.Maybe
import           Data.Text                      (Text, isPrefixOf)
import qualified Data.Text.IO                   as DTI
import           System.Directory
import           System.Environment
import           System.FilePath

listDesktopEntries :: IO [Entry]
listDesktopEntries = do
    userApps <- getXdgDirectory XdgData "applications"
    paths <- filterM doesDirectoryExist ["/usr/share/applications", userApps]
    lang <- lookupEnv "LANG"
    let l = locale $ fromMaybe "C" lang
    scanEntries paths l

scanEntries :: [FilePath] -> Locale -> IO [Entry]
scanEntries dirs locale = do
    files <- concat <$> mapM listFiles dirs
    let desktopFiles = filter isDesktopFile files
    concat <$> mapM (readEntry locale) desktopFiles
    where
        isDesktopFile = (".desktop" ==) . takeExtension

listFiles :: FilePath -> IO [FilePath]
listFiles dir =
    listDirectory dir >>=
    filterM doesFileExist . map (dir </>)

readEntry :: Locale -> FilePath -> IO [Entry]
readEntry locale path = fmap (parseEntry locale) (DTI.readFile path)

parseEntry :: Locale -> Text -> [Entry]
parseEntry locale content =
    case parseDesktopEntry content of
        Just groups -> mapMaybe parseGroup groups
        Nothing     -> []
    where
        parseGroup (key, values) | isActionKey key = parseAction locale values
                                 | otherwise       = Nothing
        isActionKey k = "Desktop Entry" == k
                        || "Desktop Action " `isPrefixOf` k

parseAction :: Locale -> Group -> Maybe Entry
parseAction locale group = do
    command      <- lookup "Exec"
    title        <- lookup "Name"
    let comment   = lookup "Comment"
        icon      = lookup "Icon"
    return $ Entry command title comment icon
    where lookup k = lookupValue k (Just locale) group
