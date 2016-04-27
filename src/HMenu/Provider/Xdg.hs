{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Provider.Xdg (
    listDesktopEntries
) where

import ClassyPrelude
import           Data.Maybe
import           Data.Text          (splitOn)
import qualified Data.Text.IO       as DTI
import           System.Environment
import           System.FilePath

import           Data.Locale
import           HMenu.ScanDirs
import           HMenu.Types
import           Xdg.Directories
import           Xdg.Parser
import           Xdg.Types

listDesktopEntries :: IO [Entry]
listDesktopEntries = do
    directories <- findDirectories DataDirs "applications"
    lang <- resolveLocale
    let l = locale $ fromJust lang
    scanDirs isDesktopFile (readEntry l) directories
    where
        isDesktopFile = return . (".desktop" ==) . takeExtension
        resolveLocale = do
            a <- lookupEnv "LC_MESSAGES"
            b <- lookupEnv "LC_ALL"
            c <- lookupEnv "LANG"
            return $ a <|> b <|> c <|> Just "C"

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
                actions = maybe [] (splitOn ";") value
                names   = map ("Desktop Action " ++) actions
            return (main, names)
        parseActions (main, names) =
            let enhance e = e {
                    title   = concat [title main, ": ", title e],
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
