{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ClassyPrelude        hiding (Index)
import           Control.Concurrent   (forkIO)
import           Control.DeepSeq
import qualified Graphics.UI.Gtk      as G
import           System.Posix.Signals

import           HMenu.Cache
import           HMenu.Command
import           HMenu.GUI
import           HMenu.Provider
import           HMenu.Search
import           HMenu.Types

main :: IO ()
main = do
    s <- initSearchEngine
    c <- runGUI instalSignalHandlers s
    maybe (return ()) execute c

instalSignalHandlers :: G.Window -> IO ()
instalSignalHandlers main = do
    let handler = Catch G.mainQuit
    installHandler keyboardSignal handler Nothing
    installHandler softwareTermination handler Nothing
    return ()

initSearchEngine :: IO SearchHandler
initSearchEngine = do
    v <- newEmptyMVar
    f <- cacheFilePath "index"
    forkIO $ do
        x <- xdgProvider
        p <- pathProvider
        let ps = metaProvider [x, p]
            h  = toHash ps
        c <- readCache f
        i <- case c of
            Just (h', i) | h' == h -> return i
            _ -> newIndex f h ps
        i `deepseq` putMVar v i
    return $ handler v
    where
        handler :: MVar (Index Entry) -> ResultHandler -> Text -> IO ()
        handler v h t = do
            i <- readMVar v
            h $ search i t
        newIndex :: FilePath -> Hash -> EntryProvider -> IO (Index Entry)
        newIndex f h ps = do
            es <- toEntries ps
            let i = createIndex es
            writeCache f (h, i)
            return i
