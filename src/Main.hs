{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Monad
import           Data.Maybe
import qualified Data.Text               as T
import qualified Graphics.UI.Gtk         as G
import           System.Environment
import           System.Posix.Signals
import           Text.Printf

import           HMenu.GUI
import           HMenu.Provider.Path
import           HMenu.Provider.Xdg
import           HMenu.Search
import           HMenu.Types

main :: IO ()
main = inBackground startBackend >>= startGUI

startBackend :: IO Index
startBackend = do
    putStrLn "Starting backend"
    entries <- inParallel [listDesktopEntries, listPathEntries]
    return $ createIndex $ concat entries

startGUI :: MVar Index -> IO ()
startGUI mVar = do
    G.initGUI
    -- Needed to run Haskell sparks
    G.timeoutAddFull (yield >> return True) G.priorityDefaultIdle 100
    main <- mainWindow
    instalSignalHandlers main
    G.mainGUI

instalSignalHandlers :: G.Window -> IO ()
instalSignalHandlers main =
    setHandler [keyboardSignal, softwareTermination] $ G.widgetDestroy main
    where
        setHandler s h = do
            let h' = Catch h
            forM_ s $ \s' -> installHandler s' h' Nothing

inBackground :: NFData a => IO a -> IO (MVar a)
inBackground f = do
    putStrLn "Starting function in backgronud"
    mvar <- newEmptyMVar
    forkFinally f $ atEnd mvar
    return mvar
    where
        atEnd _    (Left e)  = putStrLn $ "Got error: " ++ show e
        atEnd mvar (Right a) = a `deepseq` do { putStrLn "Got value"; putMVar mvar a }

inParallel :: NFData a => [IO a] -> IO [a]
inParallel actions = do
    vars <- mapM inBackground actions
    vars `seq` mapM takeMVar vars
