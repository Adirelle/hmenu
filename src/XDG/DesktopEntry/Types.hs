{-# LANGUAGE NoImplicitPrelude #-}

module XDG.DesktopEntry.Types where

import           ClassyPrelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Locale         as L

data DesktopEntry =
    DesktopEntry {
        deName        :: Localized Text,
        deGenericName :: Maybe (Localized Text),
        deNoDisplay   :: Bool,
        deComment     :: Maybe (Localized Text),
        deIcon        :: Maybe (Localized Text),
        deHidden      :: Bool,
        sub           :: EntryKind
    } deriving (Show, Eq)

data EntryKind =
    Application {
        appDBusActivatable :: Bool,
        appTryExec         :: Maybe Text,
        appExec            :: Maybe Text,
        appPath            :: Maybe Text,
        appTerminal        :: Bool,
        appActions         :: Maybe [Action],
        appKeywords        :: Maybe (Localized [Text])
    }
    | Link { lURL :: Text }
    | Folder
    deriving (Show, Eq)

data Action =
    Action {
        aName :: Localized Text,
        aIcon :: Maybe (Localized Text),
        aExec :: Maybe Text
    }  deriving (Show, Eq)

newtype Localized a = Localized (HM.HashMap L.Locale a) deriving (Show, Eq)

union :: Localized a -> Localized a -> Localized a
union (Localized a) (Localized b) = Localized $ HM.union a b

insert :: L.Locale -> a -> Localized a -> Localized a
insert l x (Localized m) = Localized $ HM.insert l x m

getLocalized :: L.Locale -> Localized a -> Maybe a
getLocalized l (Localized m) = L.lookup (`HM.lookup` m) l
