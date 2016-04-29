{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Locale (
    Locale(..),
    locale,
    localeParser
) where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Attoparsec.Text

data Locale = Locale Text (Maybe Text) (Maybe Text) (Maybe Text)
            | Default
            deriving (Ord, Eq, Show, Generic)

instance NFData Locale

locale :: String -> Locale
locale ""    = Default
locale input =
    case parseOnly (localeParser <* endOfInput) (pack input) of
        Right locale -> locale
        Left err     -> error err

localeParser :: Parser Locale
localeParser = do
    lang     <- identifier <?> "lang"
    country  <- optional (char '_' *> identifier) <?> "country"
    encoding <- optional (char '.' *> identifier) <?> "encoding"
    modifier <- optional (char '@' *> identifier) <?> "modifier"
    return $ Locale lang country encoding modifier
    <?> "locale"

identifier :: Parser Text
identifier = takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

lookupOrder :: Locale -> [Locale]
lookupOrder Default = [Default]
lookupOrder (Locale l c _ m) = go l c m
    where
        go l c m = Locale l c Nothing m : more l c m
        more l c@(Just _) m@(Just _) = Locale l Nothing Nothing m : go l c Nothing
        more l Nothing    m@(Just _) = go l Nothing Nothing
        more l c@(Just _) Nothing    = go l Nothing Nothing
        more _ _          _          = [Default]
