{-# LANGUAGE OverloadedStrings #-}

module HMenu.Types (
    Entry(..)
) where

import           Data.Text (Text, unpack)

data Entry = Entry {
    command :: Text,
    title   :: Text,
    comment :: Maybe Text,
    icon    :: Maybe Text
} deriving (Show, Eq, Ord)
