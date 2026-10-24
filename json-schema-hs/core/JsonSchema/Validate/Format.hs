{-# LANGUAGE DerivingVia #-}

module JsonSchema.Validate.Format
  ( Format (..)
  , FormatPredicate (..)
  , mkFormatPredicate
  , FormatAssertion (..)
  , assertedFormat
  , builtinFormats
  , dateParser
  , timeParser
  , dateTimeParser
  , durationParser
  , emailParser
  , idnEmailParser
  , hostnameParser
  , idnHostnameParser
  , ipv4Parser
  , ipv6Parser
  , uriParser
  , uriReferenceParser
  , uriTemplateParser
  , jsonPointerParser
  , relativeJsonPointerParser
  , regexParser
  , uuidParser
  ) where

import Control.Applicative ((<|>))
import Control.Monad qualified as Monad
import Data.Attoparsec.Text qualified as Atto
import Data.Char qualified as Char
import Data.Either qualified as Either
import Data.Fixed qualified
import Data.Functor qualified as Functor
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ratio qualified
import Data.Semigroup qualified as Semigroup
import Data.String qualified as String
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.UUID.Types qualified as UUID
import Net.IPv4 qualified as IPv4
import Net.IPv6 qualified as IPv6
import Text.Regex.Ecma262 qualified as Ecma262

import JsonSchema.Validate.Uri qualified as Uri

newtype Format = Format {formatToText :: Text.Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (String.IsString)

newtype FormatPredicate = FormatPredicate {runFormatPredicate :: Text.Text -> Bool}

mkFormatPredicate :: Atto.Parser a -> FormatPredicate
mkFormatPredicate = FormatPredicate . runParser

runParser :: Atto.Parser a -> Text.Text -> Bool
runParser p = Either.isRight . Atto.parseOnly (p <* Atto.endOfInput)

data FormatAssertion
  = FormatAssertionByVocabulary
  | FormatAssertionAlways
  deriving stock (Bounded, Eq, Ord, Show)
  deriving (Monoid, Semigroup) via Semigroup.Max FormatAssertion

assertedFormat :: Map.Map Format FormatPredicate -> Bool -> Text.Text -> Maybe FormatPredicate
assertedFormat registeredFormats assertBuiltinFormats fmt =
  Map.lookup (Format fmt) registeredFormats
    <|> (Monad.guard assertBuiltinFormats *> Map.lookup (Format fmt) builtinFormats)

builtinFormats :: Map.Map Format FormatPredicate
builtinFormats =
  Map.fromList
    [ (Format "date", mkFormatPredicate dateParser)
    , (Format "time", mkFormatPredicate timeParser)
    , (Format "date-time", mkFormatPredicate dateTimeParser)
    , (Format "duration", mkFormatPredicate durationParser)
    , (Format "email", mkFormatPredicate emailParser)
    , (Format "hostname", mkFormatPredicate hostnameParser)
    , (Format "ipv4", mkFormatPredicate ipv4Parser)
    , (Format "ipv6", mkFormatPredicate ipv6Parser)
    , (Format "uri", mkFormatPredicate uriParser)
    , (Format "uri-reference", mkFormatPredicate uriReferenceParser)
    , (Format "uri-template", mkFormatPredicate uriTemplateParser)
    , (Format "json-pointer", mkFormatPredicate jsonPointerParser)
    , (Format "relative-json-pointer", mkFormatPredicate relativeJsonPointerParser)
    , (Format "regex", mkFormatPredicate regexParser)
    , (Format "uuid", mkFormatPredicate uuidParser)
    , (Format "idn-email", mkFormatPredicate idnEmailParser)
    , (Format "idn-hostname", mkFormatPredicate idnHostnameParser)
    ]

dateParser :: Atto.Parser Time.Day
dateParser = do
  year <- fourDigits
  _ <- Atto.char '-'
  month <- twoDigits
  _ <- Atto.char '-'
  day <- twoDigits
  maybe (fail "invalid date") pure (Time.fromGregorianValid (toInteger year) month day)

fourDigits :: Atto.Parser Int
fourDigits = do
  a <- twoDigits
  b <- twoDigits
  pure (a * 100 + b)

twoDigits :: Atto.Parser Int
twoDigits = do
  a <- Atto.digit
  b <- Atto.digit
  pure (Char.digitToInt a * 10 + Char.digitToInt b)

timeParser :: Atto.Parser Time.TimeOfDay
timeParser = timeWithLeapSecond

timeWithLeapSecond :: Atto.Parser Time.TimeOfDay
timeWithLeapSecond = do
  h <- twoDigits
  _ <- Atto.char ':'
  m <- twoDigits
  _ <- Atto.char ':'
  sWhole <- twoDigits
  sFrac <- Atto.option 0 (Atto.char '.' *> fractional)
  offsetMin <- timeZoneOffset
  let
    sec = fromIntegral sWhole + sFrac :: Data.Fixed.Pico
    utcMin = mod ((h * 60 + m) - offsetMin) (24 * 60)
  Monad.when (h > 23 || m > 59 || sWhole > 60) (fail "invalid time components")
  Monad.when (sWhole == 60 && utcMin /= 23 * 60 + 59) (fail "leap second not at 23:59 UTC")
  pure (Time.TimeOfDay h m sec)

fractional :: Atto.Parser Data.Fixed.Pico
fractional = do
  digits <- Atto.takeWhile1 Char.isDigit
  let
    num = Text.foldl' (\acc d -> acc * 10 + toInteger (Char.digitToInt d)) (0 :: Integer) digits
    denom = 10 ^ Text.length digits :: Integer
  pure . fromRational $ num Data.Ratio.% denom

timeZoneOffset :: Atto.Parser Int
timeZoneOffset = do
  c <- Atto.satisfy (\ch -> ch == 'Z' || ch == 'z' || ch == '+' || ch == '-')
  case c of
    'Z' -> pure 0
    'z' -> pure 0
    _ -> do
      oh <- twoDigits
      _ <- Atto.char ':'
      om <- twoDigits
      if oh > 23 || om > 59
        then fail "invalid timezone offset"
        else
          let
            total = oh * 60 + om
           in
            pure (if c == '-' then negate total else total)

dateTimeParser :: Atto.Parser (Time.Day, Time.TimeOfDay)
dateTimeParser = do
  c <- Atto.peekChar'
  if Char.isDigit c
    then do
      d <- dateParser
      _ <- Atto.satisfy (\ch -> ch == 'T' || ch == 't')
      t <- timeWithLeapSecond
      pure (d, t)
    else fail "date-time must start with a digit"

durationParser :: Atto.Parser ()
durationParser = do
  _ <- Atto.char 'P'
  Atto.choice
    [ do
        _ <- Atto.decimal @Integer
        _ <- Atto.char 'W'
        pure ()
    , do
        hasDate <- dateComponents
        hasTime <- Atto.option False $ do
          _ <- Atto.char 'T'
          durationTimeComponents
        if hasDate || hasTime
          then pure ()
          else fail "empty duration"
    ]
  where
    dateComponents = do
      y <- Atto.option False ((Atto.decimal @Integer *> Atto.char 'Y') Functor.$> True)
      m <- Atto.option False ((Atto.decimal @Integer *> Atto.char 'M') Functor.$> True)
      d <- Atto.option False ((Atto.decimal @Integer *> Atto.char 'D') Functor.$> True)
      if y && d && not m
        then fail "years and days require months"
        else pure (y || m || d)
    durationTimeComponents = do
      h <- Atto.option False ((Atto.decimal @Integer *> Atto.char 'H') Functor.$> True)
      m <- Atto.option False ((Atto.decimal @Integer *> Atto.char 'M') Functor.$> True)
      s <- Atto.option False ((Atto.decimal @Integer *> Atto.char 'S') Functor.$> True)
      if h && s && not m
        then fail "hours and seconds require minutes"
        else
          if h || m || s
            then pure True
            else fail "empty time component"

emailParser :: Atto.Parser ()
emailParser = do
  _ <- localPart
  _ <- Atto.char '@'
  _ <- domainPart
  pure ()
  where
    localPart = Atto.choice [quotedLocal, dotAtom]
    dotAtom = do
      _ <- Atto.takeWhile1 isAtomChar
      Atto.skipMany (Atto.char '.' *> Atto.takeWhile1 isAtomChar)
    isAtomChar c =
      Char.isAsciiLower c
        || Char.isAsciiUpper c
        || Char.isDigit c
        || elem c ("!#$%&'*+/=?^_`{|}~-" :: [Char])
    quotedLocal = do
      _ <- Atto.char '"'
      Atto.skipMany
        (Atto.choice [Atto.char '\\' *> Atto.anyChar, Atto.satisfy (\c -> c /= '"' && c /= '\\')])
      _ <- Atto.char '"'
      pure ()
    domainPart = Atto.choice [ipLiteral, hostnameText]
    ipLiteral = do
      _ <- Atto.char '['
      content <- Atto.takeWhile1 (/= ']')
      _ <- Atto.char ']'
      if isValidIpv4 content
        then pure ()
        else case Text.stripPrefix "IPv6:" content of
          Just rest ->
            if isValidIpv6 rest then pure () else fail "invalid IPv6"
          Nothing ->
            if isValidIpv6 content then pure () else fail "invalid IP literal"
    hostnameText = do
      domain <- Atto.takeWhile1 (/= ' ')
      if isValidHostname domain
        then pure ()
        else fail "invalid domain"

isValidIpv4 :: Text.Text -> Bool
isValidIpv4 = Maybe.isJust . IPv4.decode

isValidIpv6 :: Text.Text -> Bool
isValidIpv6 = runParser ipv6Parser

ipv6Parser :: Atto.Parser IPv6.IPv6
ipv6Parser = do
  input <- Atto.takeText
  case IPv6.decode input of
    Nothing -> fail "invalid IPv6"
    Just addr ->
      if all validIpv6Group (Text.splitOn ":" (fst (Text.breakOn "%" input)))
        then pure addr
        else fail "IPv6 group exceeds 4 hex digits"
  where
    validIpv6Group g =
      Text.null g || Text.any (== '.') g || Text.length g <= 4

isValidHostname :: Text.Text -> Bool
isValidHostname s =
  not (Text.null s)
    && Text.length s <= 253
    && all isValidHostnameLabel (Text.splitOn "." s)

isValidHostnameLabel :: Text.Text -> Bool
isValidHostnameLabel label =
  not (Text.null label)
    && Text.length label <= 63
    && Text.all isAsciiHostnameChar label
    && not (Text.isPrefixOf "-" label)
    && not (Text.isSuffixOf "-" label)

isAsciiHostnameChar :: Char -> Bool
isAsciiHostnameChar c =
  Char.isAsciiLower c
    || Char.isAsciiUpper c
    || Char.isDigit c
    || c == '-'

hostnameParser :: Atto.Parser ()
hostnameParser = do
  s <- Atto.takeText
  if isValidHostname s
    then pure ()
    else fail "invalid hostname"

ipv4Parser :: Atto.Parser IPv4.IPv4
ipv4Parser = IPv4.parser

uriParser :: Atto.Parser ()
uriParser = do
  s <- Atto.takeText
  Monad.unless (Uri.hasUriScheme s) . fail $ "uri must start with a scheme"

uriReferenceParser :: Atto.Parser ()
uriReferenceParser = do
  s <- Atto.takeText
  Monad.when (Text.null s) . fail $ "uri-reference must be non-empty"

uriTemplateParser :: Atto.Parser ()
uriTemplateParser = do
  s <- Atto.takeText
  if not (Text.null s) && validBraces s
    then pure ()
    else fail "invalid URI template"
  where
    validBraces t = case Text.uncons t of
      Nothing -> True
      Just ('{', rest) -> case Text.breakOn "}" rest of
        (_, after) | not (Text.null after) -> validBraces (Text.drop 1 after)
        _ -> False
      Just ('}', _) -> False
      Just (_, rest) -> validBraces rest

jsonPointerParser :: Atto.Parser ()
jsonPointerParser = do
  s <- Atto.takeText
  if Text.null s
    || (Text.isPrefixOf "/" s && all validJsonPointerToken (Text.splitOn "/" . Text.drop 1 $ s))
    then pure ()
    else fail "invalid JSON pointer"
  where
    validJsonPointerToken =
      all (\escaped -> Text.isPrefixOf "0" escaped || Text.isPrefixOf "1" escaped) . drop 1 . Text.splitOn "~"

relativeJsonPointerParser :: Atto.Parser ()
relativeJsonPointerParser = do
  first <- Atto.satisfy Char.isDigit
  if first == '0'
    then Atto.choice [Atto.char '#' Functor.$> (), jsonPointerSuffix]
    else do
      _ <- Atto.takeWhile Char.isDigit
      Atto.choice [Atto.char '#' Functor.$> (), jsonPointerSuffix]
  where
    jsonPointerSuffix = do
      rest <- Atto.takeText
      if isValidJsonPointer rest
        then pure ()
        else fail "invalid json pointer suffix"
    isValidJsonPointer = runParser jsonPointerParser

regexParser :: Atto.Parser ()
regexParser = do
  s <- Atto.takeText
  Monad.unless (Ecma262.valid s) (fail "invalid regex")

uuidParser :: Atto.Parser UUID.UUID
uuidParser = do
  s <- Atto.takeText
  case UUID.fromText s of
    Just uuid -> pure uuid
    Nothing -> fail "invalid UUID"

idnEmailParser :: Atto.Parser ()
idnEmailParser = emailParser

idnHostnameParser :: Atto.Parser ()
idnHostnameParser = hostnameParser
