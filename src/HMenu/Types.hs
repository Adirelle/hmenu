{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module HMenu.Types where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Binary
import           Data.Text       (Text, unpack)
import           GHC.Generics    (Generic)

import           HMenu.Search

data Entry = Entry {
    eCommand :: Text,
    eTitle   :: Text,
    eComment :: Maybe Text,
    eIcon    :: Maybe Text
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

    fieldValue Title = Just . eTitle
    fieldValue Comment = eComment
    fieldValue Command = Just . eCommand

    fieldList = [Title, Comment, Command]
