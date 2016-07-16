{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Provider.XDG (
    xdgProvider
) where

import           ClassyPrelude
import           Data.Hashable
import           Data.Text            (splitOn)
import qualified Data.Text.IO         as DTI
import           System.Directory     (getModificationTime)
import           System.Environment
import           System.FilePath
import           System.IO            (IOMode (ReadMode), hFileSize, withFile)
import           Text.Printf

import           Data.Locale
import           HMenu.Command
import           HMenu.Provider.Types
import           HMenu.ScanDirs
import           HMenu.Types
import           XDG.DesktopEntry
import           XDG.Directories

xdgProvider :: IO EntryProvider
xdgProvider = do
    dirs <- findDirectories DataDirs "applications"
    l <- messageLocale
    let converter = readDesktopFile l
    fileBasedProvider dirs isDesktopFile hashDesktopFiles converter
    where
        isDesktopFile = return . (".desktop" ==) . takeExtension

messageLocale :: IO Locale
messageLocale = do
    a <- lookupEnv "LC_MESSAGES"
    b <- lookupEnv "LC_ALL"
    c <- lookupEnv "LANG"
    return $ case a <|> b <|> c of
        Nothing -> Default
        Just s  -> case locale $ pack s of
            Left _  -> Default
            Right l -> l

hashDesktopFiles :: [FilePath] -> IO Int
hashDesktopFiles = foldM go 0
    where
        go x f = do
            m <- hashableDateTime <$> getModificationTime f
            s <- withFile f ReadMode hFileSize
            return (x `hashWithSalt` m `hashWithSalt` s)
        hashableDateTime :: UTCTime -> String
        hashableDateTime = formatTime defaultTimeLocale "%s"

readDesktopFile :: Locale -> FilePath -> IO [Entry]
readDesktopFile l p = do
    de <- readDesktopEntry l p
    case de of
        Left e  -> do
            printf "Error in %s: %s\n" p e
            return []
        Right e -> return $ convert l e

convert :: Locale -> DesktopEntry -> [Entry]
convert l DesktopEntry { deName = n
                       , deNoDisplay = False
                       , deComment = cmt
                       , deIcon = i
                       , deHidden = False
                       , sub = Application { appExec = Just cmd
                                           , appActions = as
                                           , appTerminal = t
                                           }
                       } =
    let n'   = fromMaybe cmd $ loc n
        cmt' = loc =<< cmt
        i'   = loc =<< i
        cmd' = createCommand t cmd
        e    = Entry { eCommand = cmd', eTitle = n', eComment = cmt', eIcon = i' }
        es   = maybe [] (mapMaybe (convertAction n' i')) as
    in e : es
    where
        convertAction :: Text -> Maybe Text -> Action -> Maybe Entry
        convertAction n' i' (Action n i (Just cmd)) =
            let n''    = fromMaybe cmd (loc n)
                i''    = (loc =<< i) <|> i'
                cmd'   = createCommand t cmd
            in Just $ Entry cmd' n'' (Just n') i''
        convertAction _ _ _ = Nothing
        loc :: Localized a -> Maybe a
        loc = getLocalized l
        createCommand :: Bool -> Text -> Command
        createCommand True  = ShellCommand
        createCommand False = GraphicalCommand

convert _ _ = []
