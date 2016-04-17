module HMenu.FreeDesktop.Types where

import           Control.Applicative
import qualified Data.Char           as DC
import           Data.List
import qualified Data.Locale         as L
import           Data.Maybe
import           Data.Ord
import           Data.Text           (Text)

type DesktopEntry = [(Text, Group)]

type Group = [(Key, Text)]

data Key = Key Text (Maybe L.Locale)
    deriving (Ord, Eq, Show)

lookupGroup :: Text -> DesktopEntry -> Maybe Group
lookupGroup = lookup

lookupValue :: Text -> Maybe L.Locale -> Group -> Maybe Text
lookupValue k Nothing g = lookup (Key k Nothing) g
lookupValue k (Just (L.Locale l c _ m)) g =
    doLookup l c       m       <|>
    doLookup l Nothing m       <|>
    doLookup l c       Nothing <|>
    doLookup l Nothing Nothing <|>
    lookupValue k Nothing g
    where
        doLookup l c m = let locale = L.Locale l c Nothing m
                         in lookup (Key k (Just locale)) g
