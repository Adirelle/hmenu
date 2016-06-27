{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.GUI (
    runGUI,
    ResultHandler,
    SearchHandler
) where

import           ClassyPrelude                 hiding (on)
import           Control.Concurrent.Async
import           Graphics.UI.Gtk               as G
import qualified Graphics.UI.Gtk.General.Enums as E
import           Graphics.UI.Gtk.Layout.Grid

import           HMenu.Command                 (commandLine)
import           HMenu.Types                   as H

type ResultHandler = [H.Entry] -> IO ()
type SearchHandler = ResultHandler -> Text -> IO ()

data ResultButton = RB { bButton :: Button
                       , bLabel  :: Label
                       , bIcon   :: Image }

data GUI = GUI { main        :: Window
               , layout      :: VBox
               , input       :: G.Entry
               , search      :: SearchHandler
               , resultBox   :: VBox
               , buttons     :: MVar [ResultButton]
               , timer       :: MVar (Async ())
               , iconSGroup  :: SizeGroup
               , labelSGroup :: SizeGroup }

runGUI :: (Window -> IO ()) -> SearchHandler -> IO ()
runGUI setup search = do
    initGUI
    timeoutAddFull (yieldThread >> return True) priorityDefaultIdle 100
    gui <- newGUI search
    setup $ main gui
    mainGUI

newGUI :: SearchHandler -> IO GUI
newGUI search = do
    main        <- windowNew
    layout      <- vBoxNew False 10
    input       <- entryNew
    resultBox   <- vBoxNew True 0
    buttons     <- newMVar []
    asyncNoop   <- async $ return ()
    timer       <- newMVar asyncNoop
    iconSGroup  <- sizeGroupNew SizeGroupHorizontal
    labelSGroup <- sizeGroupNew SizeGroupHorizontal

    let gui = GUI main layout input search resultBox buttons timer iconSGroup labelSGroup

    sizeGroupSetIgnoreHidden iconSGroup True
    sizeGroupSetIgnoreHidden labelSGroup True

    boxPackStart layout input PackNatural 0

    set main [ windowTitle           := asString "HMenu"
             , windowResizable       := False
             , windowHasResizeGrip   := False
             , windowTypeHint        := WindowTypeHintDialog
             , windowSkipTaskbarHint := True
             , windowSkipPagerHint   := True
             , windowOpacity         := 0.5
             , windowWindowPosition  := WinPosCenterAlways
             , windowFocusOnMap      := True
             , containerBorderWidth  := 10
             , containerChild        := layout ]
    widgetSetSizeRequest main 400 0
    windowStick main

    main `on` destroyEvent $ liftIO mainQuit >> return False
    main `on` keyPressEvent $ handleKeyPress gui
    input `on` editableChanged $ handleChange gui

    widgetShow input
    widgetShow layout
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
    let b = resultBox gui
    p <- widgetGetParent b
    when (isJust p) $ containerRemove (layout gui) b
    widgetHide b
showResults gui entries = do
    let b = resultBox gui
    modifyMVar_ (buttons gui) $ updateButtons entries
    boxPackStart (layout gui) b PackNatural 0
    widgetShow b
    where
        updateButtons :: [H.Entry] -> [ResultButton] -> IO [ResultButton]
        updateButtons [] [] = return []
        updateButtons [] bs = do
            forM_ bs doHide
            return bs
        updateButtons e [] = do
            b <- newResultButton gui
            updateButtons e [b]
        updateButtons (e:es) (b:bs) = do
            updateButton e b
            bs' <- updateButtons es bs
            return $ b : bs'

        updateButton :: H.Entry -> ResultButton -> IO ()
        updateButton e b = do
            labelSetMarkup (bLabel b) $ "<b>" ++ eTitle e ++ "</b>" ++ commentLine (Just $ commandLine $ eCommand e)
            set (bIcon b) [ imageIconName  := fromMaybe "system-run" $ eIcon e
                          , imagePixelSize := 48 ]
            doShow b
            where
                commentLine :: Maybe Text -> Text
                commentLine Nothing   = ""
                commentLine (Just c ) = "\n<span size=\"smaller\">" ++ c ++ "</span>"

        doShow :: ResultButton -> IO ()
        doShow b = let w = bButton b in do
            widgetShow w
            p <- widgetGetParent w
            unless (isJust p) $ boxPackStart (resultBox gui) w PackGrow 0

        doHide :: ResultButton -> IO ()
        doHide b = let w = bButton b in do
            widgetHide w
            p <- widgetGetParent w
            when (isJust p) $ containerRemove (resultBox gui) w

newResultButton :: GUI -> IO ResultButton
newResultButton gui = do
    button  <- buttonNew
    label   <- labelNew (Nothing :: Maybe Text)
    icon    <- imageNew
    layout  <- hBoxNew False 0

    sizeGroupAddWidget (iconSGroup gui)  icon
    sizeGroupAddWidget (labelSGroup gui) label

    set button [ buttonFocusOnClick := False
               , buttonRelief       := ReliefHalf
               , containerChild     := layout ]

    labelSetUseMarkup label True
    miscSetAlignment label 0.0 0.5
    widgetShow label

    boxPackStart layout icon  PackNatural 0
    boxPackStart layout label PackNatural 5
    widgetShow layout

    widgetShow icon

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
