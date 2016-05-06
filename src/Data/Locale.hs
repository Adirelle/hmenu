{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
module Data.Locale (
    Locale(Default),
    locale,
    defaultLocale,
    lookup,
    localeParser
) where

import           ClassyPrelude        hiding (lookup)
import           Control.DeepSeq
import           Data.Attoparsec.Text
import           Data.Hashable

-- | A locale.
data Locale = Locale -- ^ Locale specifier.
                Text         -- ^ Lang.
                (Maybe Text) -- ^ Optional country.
                (Maybe Text) -- ^ Optional encoding.
                (Maybe Text) -- ^ Optional modifier.
            | Default -- ^ Empty locale specifier, e.g. use system locale.
            deriving (Ord, Eq, Generic)

instance NFData Locale
instance Hashable Locale
instance IsString Locale where
    fromString s = either error id $ locale (pack s)

instance Show Locale where
    show (Locale l c e m) = show $ l ++ maybe "" ('_' `cons`) c ++ maybe "" ('.' `cons`) e ++ maybe "" ('@' `cons`) m
    show Default          = show "C"

-- | Construct a Locale from a Text, returning either an error String or a Locale.
locale :: Text -> Either String Locale
locale = parseOnly (localeParser <* endOfInput)

defaultLocale :: Locale
defaultLocale = Default

-- | An Attoparsec parser for locales.
localeParser :: Parser Locale
localeParser = choice [ fullLocale, defaultLocale ] <?> "locale"
    where
        fullLocale = do
            lang     <- identifier <?> "lang"
            country  <- optional (char '_' *> identifier) <?> "country"
            encoding <- optional (char '.' *> identifier) <?> "encoding"
            modifier <- optional (char '@' *> identifier) <?> "modifier"
            return $ Locale lang country encoding modifier
        defaultLocale = string "C" *> return Default

identifier :: Parser Text
identifier = takeWhile1 (inClass "a-zA-Z0-9-") <?> "identifier"

-- | lookup f l tries f with different variations of l until one yields a Just value, or returns Nothing.
lookup :: (Locale -> Maybe a) -> Locale -> Maybe a
lookup f Default = f Default
lookup f (Locale l c _ m) = try l c m
    where
        try l c m = f (Locale l c Nothing m) <|> orElse l c m
        orElse l c@(Just _) m@(Just _) = f (Locale l Nothing Nothing m) <|> try l c Nothing
        orElse l Nothing    m@(Just _) = try l Nothing Nothing
        orElse l c@(Just _) Nothing    = try l Nothing Nothing
        orElse _ _          _          = f Default
