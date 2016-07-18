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
    (s, w) <- initSearchEngine
    c <- runGUI instalSignalHandlers s
    w
    maybe (return ()) execute c

instalSignalHandlers :: G.Window -> IO ()
instalSignalHandlers main = do
    let handler = Catch G.mainQuit
    installHandler keyboardSignal handler Nothing
    installHandler softwareTermination handler Nothing
    return ()

initSearchEngine :: IO (SearchHandler, IO ())
initSearchEngine = do
    v <- newEmptyMVar
    v2 <- newEmptyMVar
    f <- cacheFilePath "index"
    forkIO $ do
        x <- xdgProvider
        p <- pathProvider
        let ps = metaProvider [x, p]
            h  = toHash ps
        c <- readCache f
        case c of
            Just (h', i) | h' == h -> i `deepseq` putMVar v i
            _                      -> do
                                        es <- toEntries ps
                                        let i = createIndex es
                                        i `deepseq` putMVar v i
                                        writeCache f (h, i)
        putMVar v2 ()
    return (handler v, void $ readMVar v2)
    where
        handler :: MVar (Index Entry) -> ResultHandler -> Text -> IO ()
        handler v h t = do
            i <- readMVar v
            h $ search i t
