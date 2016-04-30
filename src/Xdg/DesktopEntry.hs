{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Xdg.DesktopEntry where

import           ClassyPrelude
import           Control.DeepSeq

type Failure = Text
type Faillible = Either Failure
type GroupReader = ReaderT Group Faillible

withGroup :: DesktopEntry -> Text -> GroupReader a -> Faillible a
withGroup e n r =
    case lookup n e of
        Nothing -> Left $ "Group not found: " ++ n
        Just g  -> runReaderT r g

attribute :: Text -> GroupReader (Maybe Text)
attribute k = do
    g <- ask
    return $ lookup (Key k Nothing) g

required :: (Text -> Faillible a) -> Text -> GroupReader a
required f n = do
    v <- attribute n
    lift $ case v of
        Nothing -> Left $ "Required value not found: " ++ n
        Just x  -> f x

boolean :: Text -> Faillible Bool
boolean "true"  = Right True
boolean "false" = Right False
boolean x       = Left $ "Invalid boolean: " ++ x

string :: Text -> Faillible Text
string = Right
