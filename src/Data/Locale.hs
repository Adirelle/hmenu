{-# LANGUAGE DeriveGeneric #-}

module Data.Locale (
    Locale(..),
    locale,
    localeParser
) where

import           Control.DeepSeq
import           Data.Attoparsec.Text

data Locale = Locale Text (Maybe Text) (Maybe Text) (Maybe Text)
              deriving (Ord, Eq, Show, Generic)

instance NFData Locale

locale :: String -> Locale
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
