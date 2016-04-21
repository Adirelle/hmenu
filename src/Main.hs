{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.DeepSeq
import           Graphics.UI.Gtk
import           Prelude              hiding (Index)
import           System.Environment
import           System.Posix.Signals

import           HMenu.GUI
import           HMenu.Provider.Path
import           HMenu.Provider.Xdg
import           HMenu.Search
import           HMenu.Types

main :: IO ()
main = do
    indexMVar <- inBackground startBackend
    startGUI $ handler indexMVar
    where
        handler :: MVar Index -> ResultHandler -> Text -> IO ()
        handler _ _ t| trace ("Searching for " ++ show t) False = undefined
        handler indexMVar callback text = do
            index <- readMVar indexMVar
            let results = if null text then [] else take 10 $ search index text
            callback $ trace ("Found " ++ show (length results) ++ " results") results

startBackend :: IO Index
startBackend = do
    entries <- inParallel [listDesktopEntries, listPathEntries]
    return $ createIndex $ concat entries

startGUI :: SearchHandler -> IO ()
startGUI handler = do
    initGUI
    -- Needed to run Haskell sparks
    timeoutAddFull (yieldThread >> return True) priorityDefaultIdle 100
    main <- newMainWindow handler
    instalSignalHandlers main
    mainGUI

instalSignalHandlers :: Window -> IO ()
instalSignalHandlers main = do
    let byeBye = Catch $ widgetDestroy main
    _ <- installHandler keyboardSignal byeBye Nothing
    _ <- installHandler softwareTermination byeBye Nothing
    return ()

inBackground :: NFData a => IO a -> IO (MVar a)
inBackground _ | trace "Starting a function in background" False = undefined
inBackground f = do
    mvar <- newEmptyMVar
    forkFinally f $ atEnd mvar
    return mvar
    where
        atEnd :: NFData a => MVar a -> Either SomeException a -> IO ()
        atEnd _    (Left e)  = return $ trace ("Got error: " ++ show e) ()
        atEnd mvar (Right a) = a `deepseq` putMVar mvar $ trace ("Got value") a

inParallel :: NFData a => [IO a] -> IO [a]
inParallel actions = do
    vars <- mapM inBackground actions
    vars `seq` mapM takeMVar vars
