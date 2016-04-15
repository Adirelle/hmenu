{-# LANGUAGE OverloadedStrings #-}

module Data.Locale (
    Locale,
    locale,
    localeParser,
    dist
) where

import           Data.Attoparsec.Text
import           Data.String
import           Data.Text            (Text, pack, unpack)

data Locale = Locale Text (Maybe Text) (Maybe Text) (Maybe Text)
              deriving (Ord, Eq, Show)

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

optional :: Parser a -> Parser (Maybe a)
optional p = option Nothing (fmap Just p)

identifier :: Parser Text
identifier = takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

dist :: Locale -> Locale -> Int
dist (Locale al ac ae am) (Locale bl bc be bm) =
    if al == bl then
        if ac == bc then
            if ae == be then
                if am == bm then 4
                else 3
            else 2
        else 1
    else 0
