{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.GUI (
    runGUI,
    ResultHandler,
    SearchHandler
) where

import           ClassyPrelude                 hiding (on)
import           Control.Concurrent.Async
import           Control.Monad                 (zipWithM_)
import           Graphics.UI.Gtk               as G
import qualified Graphics.UI.Gtk.General.Enums as E
import           Graphics.UI.Gtk.Layout.Grid

import           HMenu.Command                 (Command, commandLine)
import           HMenu.Types                   as H

type ResultHandler = [H.Entry] -> IO ()
type SearchHandler = ResultHandler -> Text -> IO ()

data GUI = GUI { main        :: Window
               , layout      :: VBox
               , input       :: G.Entry
               , search      :: SearchHandler
               , resultBox   :: VBox
               , buttons     :: MVar [ResultButton]
               , timer       :: MVar (Async ())
               , iconSGroup  :: SizeGroup
               , labelSGroup :: SizeGroup
               , selection   :: MVar (Maybe Command)
               }

data ResultButton = RB { bButton :: Button
                       , bLabel  :: Label
                       , bIcon   :: Image
                       , bCmdVar :: MVar Command }

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

showResults :: GUI -> [H.Entry] -> IO ()
showResults gui [] = hideAndOrphan $ resultBox gui
showResults gui@GUI { layout = l , resultBox = box, buttons = bs } es = do
    modifyMVar_ bs $ updateButtons es
    showAndPack box l PackNatural
    where
        updateButtons :: [H.Entry] -> [ResultButton] -> IO [ResultButton]
        updateButtons es bs = do
            bs' <- case compare le lb of
                EQ -> return bs
                LT -> do
                    mapM_ clearEntry $ drop le bs
                    return bs
                GT -> do
                    bs' <- replicateM (le-lb) (newResultButton gui)
                    return $ bs ++ bs'
            zipWithM_ (setEntry box) bs' es
            return bs'
            where
                le = length es
                lb = length bs

newResultButton :: GUI -> IO ResultButton
newResultButton gui = do
    button   <- buttonNew
    label    <- labelNew (Nothing :: Maybe Text)
    icon     <- imageNew
    layout   <- hBoxNew False 0
    entryVar <- newEmptyMVar

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

    return $ RB button label icon entryVar

setEntry :: BoxClass a => a -> ResultButton -> H.Entry -> IO ()
setEntry cnt (RB w l i v) (H.Entry c t _ ic) = do
    labelSetMarkup l $ "<b>" ++ t ++ "</b>\n<span size=\"smaller\">" ++ commandLine c ++ "</span>"
    set i [ imageIconName := fromMaybe "system-run" ic , imagePixelSize := 48 ]

    tryTakeMVar v
    putMVar v c

    showAndPack w cnt PackGrow

clearEntry :: ResultButton -> IO ()
clearEntry (RB w _ _ v)  = do
    tryTakeMVar v
    hideAndOrphan w

showAndPack :: (WidgetClass a, BoxClass b) => a -> b -> Packing -> IO ()
showAndPack w c p = do
    orphan w
    widgetShow w
    boxPackStart c w p 0

hideAndOrphan :: WidgetClass a => a -> IO ()
hideAndOrphan w = do
    widgetHide w
    orphan w

orphan :: WidgetClass a => a -> IO ()
orphan w = do
    p <- widgetGetParent w
    case p of
        Just p' -> containerRemove (castToContainer p') w
        Nothing -> return ()
