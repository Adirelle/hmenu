{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Command (
    Command(..),
    execute,
    commandLine
) where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Binary
import           GHC.Generics         (Generic)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Posix.Process

data Command = ShellCommand Text
             | GraphicalCommand Text
             deriving (Show, Eq, Ord, Generic)

instance NFData Command
instance Binary Command
instance Hashable Command

commandLine :: Command -> Text
commandLine (ShellCommand     c) = c
commandLine (GraphicalCommand c) = c

execute :: Command -> IO ()
execute c = do
    (cmd, args) <- toGraphical c
    executeFile (unpack cmd) True (map unpack args) Nothing

toGraphical :: Command -> IO (Text, [Text])
toGraphical (ShellCommand c) = do
    t <- findTerminal
    case t of
        Nothing -> error "Cannot find a terminal emulator"
        Just t' -> return (t', ["-e", c])
toGraphical (GraphicalCommand c) = do
    s <- findShell
    return (s, ["-c", c])

findShell :: IO Text
findShell = do
    s <- lookupEnv "SHELL"
    case s of
        Just s' -> return $ pack s'
        Nothing -> error "Empty $SHELL !"

findTerminal :: IO (Maybe Text)
findTerminal = do
    t <- lookupEnv "TERMINAL"
    firstExec $ case t of
        Just t' -> pack t' : defaultTerminals
        Nothing -> defaultTerminals
    where
        firstExec :: [Text] -> IO (Maybe Text)
        firstExec [] = return Nothing
        firstExec (f:fs) = do
            ex <- findExecutable $ unpack f
            case ex of
                Nothing -> firstExec fs
                Just f' -> return $ Just (pack f')

defaultTerminals :: [Text]
defaultTerminals = [ "x-terminal-emulator"
                   , "urxvt"
                   , "rxvt"
                   , "termit"
                   , "terminator"
                   , "Eterm"
                   , "aterm"
                   , "xterm"
                   , "gnome-terminal"
                   , "roxterm"
                   , "xfce4-terminal"
                   , "termite"
                   , "lxterminal"
                   , "mate-terminal"
                   , "terminology"
                   , "st"
                   , "qterminal"
                   ]
