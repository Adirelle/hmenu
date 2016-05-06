{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ClassyPrelude        hiding (Index)
import qualified Graphics.UI.Gtk      as G
import           System.Posix.Signals

import           HMenu.Cache
import           HMenu.GUI
import           HMenu.Provider.Path
import           HMenu.Provider.XDG
import           HMenu.Search
import           HMenu.Types

main :: IO ()
main = do
    getIndex <- prepareIndex
    runGUI instalSignalHandlers (handler getIndex)
    where
        handler :: IO (Index Entry) -> ResultHandler -> Text -> IO ()
        handler _ _ t | trace ("Searching for " ++ show t) False = error "Shouldn't happen"
        handler getIndex callback text = do
            index <- getIndex
            let results = if null text then [] else take 10 $ search index text
            callback $ trace ("Found " ++ show (length results) ++ " results") results

prepareIndex :: IO (IO (Index Entry))
prepareIndex = do
    entries <- (++) <$> listDesktopEntries <*> listPathEntries
    setupCache "index" entries createIndex

instalSignalHandlers :: G.Window -> IO ()
instalSignalHandlers main = do
    let byeBye = Catch $ G.widgetDestroy main
    _ <- installHandler keyboardSignal byeBye Nothing
    _ <- installHandler softwareTermination byeBye Nothing
    return ()
