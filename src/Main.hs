{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ClassyPrelude        hiding (Index)
import qualified Graphics.UI.Gtk      as G
import           System.Posix.Signals

import           HMenu.Cache
import           HMenu.Command
import           HMenu.GUI
import           HMenu.Provider.Path
import           HMenu.Provider.XDG
import           HMenu.Search
import           HMenu.Types

main :: IO ()
main = do
    getIndex <- prepareIndex
    c <- runGUI instalSignalHandlers (handler getIndex)
    maybe (return ()) execute c
    where
        handler :: IO (Index Entry) -> ResultHandler -> Text -> IO ()
        handler getIndex callback text = do
            index <- getIndex
            let results = if null text then [] else take 10 $ search index text
            callback $ results

prepareIndex :: IO (IO (Index Entry))
prepareIndex = do
    entries <- (++) <$> listDesktopEntries <*> listPathEntries
    setupCache "index" entries createIndex

instalSignalHandlers :: G.Window -> IO ()
instalSignalHandlers main = do
    let handler = Catch G.mainQuit
    installHandler keyboardSignal handler Nothing
    installHandler softwareTermination handler Nothing
    return ()
