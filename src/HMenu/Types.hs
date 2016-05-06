{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module HMenu.Types (
    Entry(..)
) where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Binary
import           Data.Text       (Text, unpack)
import           GHC.Generics    (Generic)

import           HMenu.Search

data Entry = Entry {
    command :: Text,
    title   :: Text,
    comment :: Maybe Text,
    icon    :: Maybe Text
} deriving (Show, Eq, Ord, Generic)

instance NFData Entry
instance Binary Entry
instance Hashable Entry

instance Indexable Entry where
    data IndexableField Entry = Title | Comment | Command
                                deriving (Eq, Ord, Show, Enum, Bounded)

    fieldWeight Title = 1.0
    fieldWeight Comment = 0.8
    fieldWeight Command = 0.6

    fieldValue Title = Just . title
    fieldValue Comment = comment
    fieldValue Command = Just . command

    fieldList = [Title, Comment, Command]
