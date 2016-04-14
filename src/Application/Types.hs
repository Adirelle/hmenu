module Application.Types where

import           Data.Text (Text)

data Entry = Entry {
    command :: Text,
    title   :: Text,
    comment :: Maybe Text,
    icon    :: Maybe Text
} deriving (Show, Eq)
