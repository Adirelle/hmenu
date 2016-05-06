{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Provider.XDG (
    listDesktopEntries
) where

import           ClassyPrelude
import           Data.Text          (splitOn)
import qualified Data.Text.IO       as DTI
import           System.Environment
import           System.FilePath
import           Text.Printf

import           Data.Locale
import           HMenu.ScanDirs
import           HMenu.Types
import           XDG.DesktopEntry
import           XDG.Directories

listDesktopEntries :: IO [Entry]
listDesktopEntries = do
    directories <- findDirectories DataDirs "applications"
    lang <- resolveLocale
    let l = either (const Default) id $ locale $ maybe "" pack lang
    scanDirs isDesktopFile (readEntry l) directories
    where
        isDesktopFile = return . (".desktop" ==) . takeExtension
        resolveLocale = do
            a <- lookupEnv "LC_MESSAGES"
            b <- lookupEnv "LC_ALL"
            c <- lookupEnv "LANG"
            return $ a <|> b <|> c <|> Just "C"

readEntry :: Locale -> FilePath -> IO [Entry]
readEntry l p = do
    c <- DTI.readFile p
    let r = analyse c
    case r of
        Left e  -> do
            printf "Error in %s: %s\n" p e
            return []
        Right e -> return $ convert l e

convert :: Locale -> DesktopEntry -> [Entry]
convert l DesktopEntry { deName = n, deNoDisplay = False, deComment = cmt, deIcon = i, deHidden = False, sub = Application { appExec = Just cmd, appActions = as }} =
    let n'   = fromMaybe cmd $ loc n
        cmt' = loc =<< cmt
        i'   = loc =<< i
        e    = Entry { command = cmd, title = n', comment = cmt', icon = i' }
        es   = maybe [] (mapMaybe (convertAction n' i')) as
    in e : es
    where
        convertAction :: Text -> Maybe Text -> Action -> Maybe Entry
        convertAction n' i' (Action n i (Just cmd)) =
            let n''    = fromMaybe cmd (loc n)
                i''    = (loc =<< i) <|> i'
            in Just $ Entry cmd n'' (Just n') i''
        convertAction _ _ _ = Nothing
        loc :: Localized a -> Maybe a
        loc = getLocalized l
convert _ _ = []
