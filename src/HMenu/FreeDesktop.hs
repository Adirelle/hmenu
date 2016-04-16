{-# LANGUAGE OverloadedStrings #-}

module HMenu.FreeDesktop --(
    --listDesktopEntries
--)
where

import           Control.Monad
import           Data.Locale
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.IO             as DTI
import           HMenu.FreeDesktop.Parser
import           HMenu.FreeDesktop.Types
import           HMenu.Types
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
                    icon    = icon e `mplus` icon main,
                    comment = comment e `mplus` comment main
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
