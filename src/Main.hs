{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Graphics.UI.Gtk
import           ClassyPrelude              hiding (Index)
import           System.Posix.Signals

import           HMenu.Cache
import           HMenu.GUI
import           HMenu.Provider.Path
import           HMenu.Provider.Xdg
import           HMenu.Search
import           HMenu.Types

main :: IO ()
main = do
    getIndex <- prepareIndex
    runGUI instalSignalHandlers (handler getIndex)
    where
        handler :: IO Index -> ResultHandler -> Text -> IO ()
        handler _ _ t | trace ("Searching for " ++ show t) False = error "Shouldn't happen"
        handler getIndex callback text = do
            index <- getIndex
            let results = if null text then [] else take 10 $ search index text
            callback $ trace ("Found " ++ show (length results) ++ " results") results

prepareIndex :: IO (IO Index)
prepareIndex = do
    entries <- (++) <$> listDesktopEntries <*> listPathEntries
    setupCache "index" entries createIndex

instalSignalHandlers :: Window -> IO ()
instalSignalHandlers main = do
    let byeBye = Catch $ widgetDestroy main
    _ <- installHandler keyboardSignal byeBye Nothing
    _ <- installHandler softwareTermination byeBye Nothing
    return ()
