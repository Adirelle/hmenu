{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMenu.Types (
    Entry(..)
) where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Binary
import           Data.Text       (Text, unpack)
import           GHC.Generics    (Generic)

data Entry = Entry {
    command :: Text,
    title   :: Text,
    comment :: Maybe Text,
    icon    :: Maybe Text
} deriving (Show, Eq, Ord, Generic)

instance NFData Entry
instance Binary Entry
instance Hashable Entry
