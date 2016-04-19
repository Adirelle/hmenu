{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HMenu.Types (
    Entry(..)
) where

import           Control.DeepSeq
import           Data.Text       (Text, unpack)
import           GHC.Generics    (Generic)

data Entry = Entry {
    command :: Text,
    title   :: Text,
    comment :: Maybe Text,
    icon    :: Maybe Text
} deriving (Show, Eq, Ord, Generic)

instance NFData Entry
