module HMenu.FreeDesktop.Types where

import qualified Data.Char   as DC
import           Data.List
import qualified Data.Locale as L
import           Data.Maybe
import           Data.Ord
import           Data.Text   (Text)

type DesktopEntry = [(Text, Group)]

type Group = [(Key, Text)]

data Key = Key Text (Maybe L.Locale)
    deriving (Ord, Eq, Show)

lookupGroup :: Text -> DesktopEntry -> Maybe Group
lookupGroup = lookup

lookupValue :: Text -> Maybe L.Locale -> Group -> Maybe Text
lookupValue key Nothing values = lookup (Key key Nothing) values
lookupValue key (Just locale) values =
    snd $ foldr keepBest (-1, Nothing) values
    where
        keepBest (Key k _       , _) a            | k /= key   = a
        keepBest (Key _ Nothing , v) (_, Nothing)              = (0, Just v)
        keepBest (Key _ Nothing , _) a                         = a
        keepBest (Key _ (Just l), _) a@(d, _)     | dist l < d = a
        keepBest (Key _ (Just l), v) _                         = (dist l, Just v)
        dist = L.dist locale
