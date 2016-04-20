module HMenu.GUI (
    mainWindow,
    ResultHandler,
    SearchHandler
) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text                  as T
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.EventM
import           Text.Printf

import qualified HMenu.Types                as H

type ResultHandler = [H.Entry] -> IO ()
type SearchHandler = ResultHandler -> Text -> IO ()

mainWindow :: SearchHandler -> IO Window
mainWindow handler = do
    window <- windowNew
    vbox <- vBoxNew True 5
    input <- searchEntry (handler resultHandler)
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
    where
        resultHandler :: [H.Entry] -> IO ()
        resultHandler entries =
            forM_ entries $ \(H.Entry cmd title cmt _) -> do
                let sTitle = unpack title
                    sCmd   = unpack cmd
                    sCmt   = maybe "" unpack cmt
                printf "%s - %s - %s\n" sTitle sCmt sCmd

bailOut :: IO ()
bailOut = debug "Bye bye !" >> mainQuit

searchEntry :: (Text -> IO ()) -> IO Entry
searchEntry handler = do
    entry <- entryNew
    handler <- delayed 700000 $ entryChanged entry handler
    onEditableChanged entry handler
    return entry

entryChanged :: Entry -> (T.Text -> IO ()) -> IO ()
entryChanged entry handler = do
    text <- T.pack <$> entryGetText entry
    debug $ "changed: " ++ show text
    handler text

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
