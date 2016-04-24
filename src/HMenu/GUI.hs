module HMenu.GUI (
    newMainWindow,
    ResultHandler,
    SearchHandler
) where

import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.EventM
import           Prelude                    hiding (on)

import qualified HMenu.Types                as H

type ResultHandler = [H.Entry] -> IO ()
type SearchHandler = ResultHandler -> Text -> IO ()

newMainWindow :: SearchHandler -> IO Window
newMainWindow search = do
    vbox <- vBoxNew True 5
    widgetShow vbox
    set vbox [ boxHomogeneous := False ]

    buttonVBox <- vBoxNew True 0
    resultHandler <- newResultHandler buttonVBox
    searchEntry <- newSearchEntry (search resultHandler)
    widgetShow searchEntry

    boxPackStart vbox searchEntry PackNatural 0
    boxPackStart vbox buttonVBox PackNatural 0

    window <- windowNew
    set window [
            windowTitle           := asString "HMenu",
            windowDecorated       := False,
            windowDefaultWidth    := 400,
            windowTypeHint        := WindowTypeHintDialog,
            windowSkipTaskbarHint := True,
            windowSkipPagerHint   := True,
            windowWindowPosition  := WinPosCenterAlways,
            windowFocusOnMap      := True,
            containerBorderWidth  := 10,
            containerChild        := vbox
        ]
    windowStick window
    window `onDestroy` mainQuit
    window `on` keyPressEvent $ handleKeyPress searchEntry resultHandler

    widgetShow window
    return window

handleKeyPress e h =
    eventKeyVal >>= liftIO . handleKey >> return False
    where
        handleKey k = when (k == 0xff1b) escapePressed
        escapePressed = do
            t <- asText <$> entryGetText e
            if null t then
                mainQuit
            else do
                entrySetText e $ asText ""
                h []

newResultHandler :: BoxClass c => c -> IO ResultHandler
newResultHandler container = do
    buttonMvar <- newMVar []
    return $ showEntries buttonMvar
    where
        showEntries :: MVar [Button] -> [H.Entry] -> IO ()
        showEntries v [] = widgetHide container
        showEntries v e  = widgetShow container >> modifyMVar_ v (updateButtons e)
        updateButtons :: [H.Entry] -> [Button] -> IO [Button]
        updateButtons [] [] = return []
        updateButtons [] bs = do
            forM_ bs doHide
            return bs
        updateButtons e [] = do
            b <- newResultButton
            updateButtons e [b]
        updateButtons (e:es) (b:bs) = do
            updateButton e b
            bs' <- updateButtons es bs
            return $ b : bs'
        updateButton :: H.Entry -> Button -> IO ()
        updateButton e b = do
            let title = unpack $ H.title e
            buttonSetLabel b title
            doShow b
        doShow b = do
            widgetShow b
            parent <- widgetGetParent b
            case parent of
                Just p -> return ()
                Nothing -> boxPackStart container b PackGrow 0
        doHide b = do
            widgetHide b
            parent <- widgetGetParent b
            case parent of
                Just p -> containerRemove container b
                Nothing -> return ()

newResultButton :: IO Button
newResultButton = do
    button <- buttonNew
    set button [
            buttonFocusOnClick := False,
            buttonRelief       := ReliefHalf,
            buttonXalign       := 0
        ]
    return button

newSearchEntry :: (Text -> IO ()) -> IO Entry
newSearchEntry search = do
    entry <- entryNew
    handleChange <- newDelayedHandler 350000 $ handleEntryChange entry search
    entry `onEditableChanged` handleChange
    widgetGrabFocus entry
    widgetGrabDefault entry
    return entry

handleEntryChange :: Entry -> (Text -> IO ()) -> IO ()
handleEntryChange entry search =
    entryGetText entry >>= search . pack

newDelayedHandler :: Int -> IO () -> IO (IO ())
newDelayedHandler delay action = do
    tidVar <- newEmptyMVar
    return $ startDelay tidVar
    where
        startDelay v = do
            maybeTid <- tryReadMVar v
            mapM_ killThread maybeTid
            newTid <- forkFinally delayedAction $ \_ -> void $ takeMVar v
            putMVar v newTid
        delayedAction = do
            threadDelay delay
            action
