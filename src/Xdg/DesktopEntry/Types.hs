{-# LANGUAGE NoImplicitPrelude #-}

module Xdg.DesktopEntry.Types where

import           ClassyPrelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Locale         as L

newtype Localized a = Localized (HM.HashMap L.Locale a)
                      deriving (Show, Eq)

data CommonDesc = CommonDesc { deName        :: Localized Text
                             , deGenericName :: Maybe (Localized Text)
                             , deNoDisplay   :: Bool
                             , deComment     :: Maybe (Localized Text)
                             , deIcon        :: Maybe (Localized Text)
                             , deHidden      :: Bool }
                  deriving (Show, Eq)

data DesktopEntry = Application { common             :: CommonDesc
                                , appDBusActivatable :: Bool
                                , appTryExec         :: Maybe Text
                                , appExec            :: Maybe Text
                                , appPath            :: Maybe Text
                                , appTerminal        :: Bool
                                , appActions         :: [Action]
                                , appKeywords        :: Localized [Text] }
                  | Link { common :: CommonDesc
                         , lURL   :: Text }
                  | Folder { common :: CommonDesc }
                    deriving (Show, Eq)

data Action = Action { aName :: Localized Text
                     , aIcon :: Maybe (Localized Text)
                     , aExec :: Maybe Text }
              deriving (Show, Eq)

getLocalized :: L.Locale -> Localized a -> Maybe a
getLocalized l (Localized m) = L.lookup (`HM.lookup` m) l
