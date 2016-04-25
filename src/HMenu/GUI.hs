module HMenu.GUI (
    runGUI,
    ResultHandler,
    SearchHandler
) where

import           Control.Concurrent.Async
import           Graphics.UI.Gtk
import           Prelude                  hiding (on)

import qualified HMenu.Types              as H

type ResultHandler = [H.Entry] -> IO ()
type SearchHandler = ResultHandler -> Text -> IO ()

data GUI = GUI {
        main      :: Window,
        input     :: Entry,
        search    :: SearchHandler,
        resultBox :: VBox,
        buttons   :: MVar [Button],
        timer     :: MVar (Async ())
    }

runGUI :: (Window -> IO ()) -> SearchHandler -> IO ()
runGUI setup search = do
    initGUI
    timeoutAddFull (yieldThread >> return True) priorityDefaultIdle 100
    gui <- newGUI search
    setup $ main gui
    mainGUI

newGUI :: SearchHandler -> IO GUI
newGUI search = do
    main <- windowNew
    input <- entryNew
    resultBox <- vBoxNew True 0
    buttons <- newMVar []
    asyncNoop <- async $ return ()
    timer <- newMVar asyncNoop

    let gui = GUI main input search resultBox buttons timer

    vbox <- vBoxNew True 5
    set vbox [ boxHomogeneous := False ]
    boxPackStart vbox input PackNatural 0
    boxPackStart vbox resultBox PackNatural 0

    set main [
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
    windowStick main
    main `onDestroy` mainQuit

    main `on` keyPressEvent $ handleKeyPress gui
    input `on` editableChanged $ handleChange gui

    widgetShow input
    widgetShow vbox
    widgetShow main
    return gui

handleKeyPress gui =
    eventKeyVal >>= liftIO . handleKey >> return False
    where
        handleKey k = when (k == 0xff1b) escapePressed
        escapePressed = do
            t <- asText <$> entryGetText (input gui)
            if null t then
                mainQuit
            else do
                entrySetText (input gui) $ asText ""
                showResults gui []

showResults :: GUI -> [H.Entry] -> IO ()
showResults gui [] =
    widgetHide (resultBox gui)
showResults gui entries = do
    modifyMVar_ (buttons gui) $ updateButtons entries
    widgetShow (resultBox gui)
    where
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
                Nothing -> boxPackStart (resultBox gui) b PackGrow 0
        doHide b = do
            widgetHide b
            parent <- widgetGetParent b
            case parent of
                Just p -> containerRemove (resultBox gui) b
                Nothing -> return ()

newResultButton = do
    button <- buttonNew
    set button [
            buttonFocusOnClick := False,
            buttonRelief       := ReliefHalf,
            buttonXalign       := 0
        ]
    return button

handleChange gui = liftIO $ modifyMVar_ (timer gui) restartTimer
    where
        restartTimer prev = do
            cancel prev
            async $ waitAndSearch (input gui) (search gui) (showResults gui)
        waitAndSearch input search showResults = do
            threadDelay 500000
            text <- entryGetText input
            search showResults text
