module HMenu.GUI (
    runGUI,
    ResultHandler,
    SearchHandler
) where

import           Control.Concurrent.Async
import           Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.General.Enums as E
import           Graphics.UI.Gtk.Layout.Grid
import           Prelude                       hiding (on)

import qualified HMenu.Types                   as H

type ResultHandler = [H.Entry] -> IO ()
type SearchHandler = ResultHandler -> Text -> IO ()

data ResultButton = RB { bButton :: Button
                       , bLabel  :: Label
                       , bIcon   :: Image }

data GUI = GUI { main      :: Window
               , input     :: Entry
               , search    :: SearchHandler
               , resultBox :: VBox
               , buttons   :: MVar [ResultButton]
               , timer     :: MVar (Async ()) }

runGUI :: (Window -> IO ()) -> SearchHandler -> IO ()
runGUI setup search = do
    initGUI
    timeoutAddFull (yieldThread >> return True) priorityDefaultIdle 100
    gui <- newGUI search
    setup $ main gui
    mainGUI

newGUI :: SearchHandler -> IO GUI
newGUI search = do
    main      <- windowNew
    input     <- entryNew
    resultBox <- vBoxNew True 0
    buttons   <- newMVar []
    asyncNoop <- async $ return ()
    timer     <- newMVar asyncNoop

    let gui = GUI main input search resultBox buttons timer

    vbox <- vBoxNew True 5
    set vbox [ boxHomogeneous := False ]
    boxPackStart vbox input PackNatural 0
    boxPackStart vbox resultBox PackNatural 0

    set main [ windowTitle           := asString "HMenu"
             , windowDecorated       := False
             , windowDefaultWidth    := 400
             , windowTypeHint        := WindowTypeHintDialog
             , windowSkipTaskbarHint := True
             , windowSkipPagerHint   := True
             , windowWindowPosition  := WinPosCenterAlways
             , windowFocusOnMap      := True
             , containerBorderWidth  := 10
             , containerChild        := vbox ]
    windowStick main

    main `on` destroyEvent $ liftIO mainQuit >> return False
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
showResults gui [] = do
    widgetHide (resultBox gui)
    containerResizeChildren (main gui)
showResults gui entries = do
    modifyMVar_ (buttons gui) $ updateButtons entries
    widgetShow (resultBox gui)
    where
        updateButtons :: [H.Entry] -> [ResultButton] -> IO [ResultButton]
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

        updateButton :: H.Entry -> ResultButton -> IO ()
        updateButton e b = do
            labelSetMarkup (bLabel b) $ "<b>" ++ H.title e ++ "</b>" ++ commentLine (H.comment e)
            case H.icon e of
                Just i -> do
                    imageSetFromIconName (bIcon b) i IconSizeDialog
                    widgetShow (bIcon b)
                Nothing ->
                    widgetHide (bIcon b)
            doShow b
            where
                commentLine :: Maybe Text -> Text
                commentLine Nothing   = ""
                commentLine (Just c ) = "\n<span size=\"smaller\">" ++ c ++ "</span>"

        doShow :: ResultButton -> IO ()
        doShow b = let w = bButton b in do
            widgetShow w
            parent <- widgetGetParent w
            case parent of
                Just p -> return ()
                Nothing -> boxPackStart (resultBox gui) w PackGrow 0

        doHide :: ResultButton -> IO ()
        doHide b = let w = bButton b in do
            widgetHide w
            parent <- widgetGetParent w
            case parent of
                Just p -> containerRemove (resultBox gui) w
                Nothing -> return ()

newResultButton :: IO ResultButton
newResultButton = do
    button  <- buttonNew
    label   <- labelNew (Nothing :: Maybe Text)
    icon    <- imageNew
    layout  <- hBoxNew False 0

    set button [ buttonFocusOnClick := False
               , buttonRelief       := ReliefHalf
               , containerChild     := layout ]

    labelSetUseMarkup label True
    widgetShow label

    boxPackStart layout icon  PackNatural 0
    boxPackStart layout label PackNatural 5
    widgetShow layout

    return $ RB button label icon

handleChange gui = liftIO $ modifyMVar_ (timer gui) restartTimer
    where
        restartTimer prev = do
            cancel prev
            async $ waitAndSearch (input gui) (search gui) (showResults gui)
        waitAndSearch input search showResults = do
            threadDelay 500000
            text <- entryGetText input
            if null text then
                showResults []
            else
                search showResults text
