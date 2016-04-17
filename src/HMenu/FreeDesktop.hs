{-# LANGUAGE OverloadedStrings #-}

module HMenu.FreeDesktop (
    listDesktopEntries
) where

import           Control.Applicative      ((<|>))
import           Control.Monad            ((>=>))
import           Data.Locale              (Locale, locale)
import           Data.Maybe               (fromMaybe, mapMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as DTI
import           HMenu.FreeDesktop.Parser (parseDesktopEntry)
import           HMenu.FreeDesktop.Types  (DesktopEntry, Group, lookupValue)
import           HMenu.ScanDirs           (scanDirs)
import           HMenu.Types              (Entry (..))
import           System.Directory         (XdgDirectory (..), getXdgDirectory)
import           System.Environment       (lookupEnv)
import           System.FilePath          (takeExtension)

listDesktopEntries :: IO [Entry]
listDesktopEntries = do
    userApps <- getXdgDirectory XdgData "applications"
    lang <- lookupEnv "LC_MESSAGES" <|> lookupEnv "LC_ALL" <|> lookupEnv "LANG"
    let paths = ["/usr/share/applications", userApps]
        l = locale $ fromMaybe "C" lang
    scanDirs isDesktopFile (readEntry l) paths
    where
        isDesktopFile = return . (".desktop" ==) . takeExtension

readEntry :: Locale -> FilePath -> IO [Entry]
readEntry locale path = do
    content <- DTI.readFile path
    return $ maybe [] (parseEntry locale) $ parseDesktopEntry content

parseEntry :: Locale -> DesktopEntry -> [Entry]
parseEntry locale desktopEntry =
    maybe [] parseActions getMainAndActions
    where
        getMainAndActions = do
            group <- lookup "Desktop Entry" desktopEntry
            main  <- parseAction locale group
            let value   = lookupValue "Actions" Nothing group
                actions = maybe [] (T.splitOn ";") value
                names   = map ("Desktop Action " `T.append`) actions
            return (main, names)
        parseActions (main, names) =
            let enhance e = e {
                    title   = T.concat [title main, ": ", title e],
                    icon    = icon e <|>icon main,
                    comment = comment e <|> comment main
                }
                actions = mapMaybe (lookupGroup >=> groupToAction) names
            in main : map enhance actions
        lookupGroup   = flip lookup desktopEntry
        groupToAction = parseAction locale

parseAction :: Locale -> Group -> Maybe Entry
parseAction locale group = do
    command      <- lookup "Exec"
    title        <- lookup "Name"
    let comment   = lookup "Comment"
        icon      = lookup "Icon"
    return $ Entry command title comment icon
    where lookup k = lookupValue k (Just locale) group
