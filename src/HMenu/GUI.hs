module HMenu.GUI (
    mainWindow
) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.EventM

mainWindow :: IO Window
mainWindow = do
    window <- windowNew
    vbox <- vBoxNew True 5
    input <- searchEntry
    set window [ windowTitle := "HMenu",-- :: U.,
                 windowDefaultWidth := 200,
                 windowDefaultHeight := 100,
                 windowTypeHint := WindowTypeHintDialog,
                 windowSkipTaskbarHint := True,
                 windowSkipPagerHint := True,
                 containerBorderWidth := 10,
                 containerChild := vbox ]
    windowStick window
    windowSetPosition window WinPosCenterAlways
    boxPackStart vbox input PackGrow 0
    widgetShowAll window
    onDestroy window bailOut
    on window keyPressEvent $ do
        key <- eventKeyVal
        liftIO $ when (key == 0xff1b) bailOut -- Leave on Escape
        return False
    return window

bailOut :: IO ()
bailOut = debug "Bye bye !" >> mainQuit

searchEntry :: IO Entry
searchEntry = do
    entry <- entryNew
    handler <- delayed 700000 $ entryChanged entry
    onEditableChanged entry handler
    return entry

entryChanged :: Entry -> IO ()
entryChanged e = do
    text <- entryGetText e
    debug $ "changed: " ++ text

delayed :: Int -> IO () -> IO (IO ())
delayed delay action = do
    tidVar <- newEmptyMVar
    return $ go tidVar
    where
        go tidVar = do
            maybeTid <- tryReadMVar tidVar
            mapM_ killThread maybeTid
            newTid <- forkFinally delayedAction $ \_ -> void $ takeMVar tidVar
            putMVar tidVar newTid
        delayedAction = do
            threadDelay delay
            action

debug str = do
    tid <- myThreadId
    putStrLn $ show tid ++ ' ' : str
