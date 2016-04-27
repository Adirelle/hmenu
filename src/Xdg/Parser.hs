{-# LANGUAGE NoImplicitPrelude #-}

module Xdg.Parser (
    parseDesktopEntry
) where

import           Data.Attoparsec.Text
import           ClassyPrelude              hiding (group)

import           Data.Locale
import           Xdg.Types

parseDesktopEntry :: Text -> Maybe DesktopEntry
parseDesktopEntry content =
    case parseOnly desktopEntry content of
        Left  _ -> Nothing
        Right x -> Just x

desktopEntry :: Parser DesktopEntry
desktopEntry = manyTill group endOfInput <* endOfInput <?> "desktop entry"

group :: Parser (Text, Group)
group = do
    key <- header
    values <- many' keyValuePair
    return (key, values)
    <?> "group"

header :: Parser Text
header = do
    skipMany comment
    char '['
    identifier <- takeWhile1 notSquareBracket
    char ']'
    skipHorizontalSpace
    endOfLine
    return identifier
    <?> "header"
    where
        notSquareBracket '[' = False
        notSquareBracket ']' = False
        notSquareBracket _ = True

keyValuePair :: Parser (Key, Text)
keyValuePair = do
    skipMany comment
    key <- identifier <?> "key"
    locale <- optional (char '[' *> localeParser <* char ']')
    skipHorizontalSpace
    char '='
    skipHorizontalSpace
    value <- takeTill isEndOfLine <?> "value"
    skipHorizontalSpace
    endOfLine
    return (Key key locale, value)
    <?> "value line"

identifier :: Parser Text
identifier = takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

comment :: Parser String
comment = choice [
            char '#' *> manyTill anyChar endOfLine <?> "comment",
            manyTill space endOfLine               <?> "blank line"
          ]

skipHorizontalSpace = skipWhile isHorizontalSpace
