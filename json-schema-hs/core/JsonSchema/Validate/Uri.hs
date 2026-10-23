module JsonSchema.Validate.Uri
  ( resolveUri
  , hasUriScheme
  , stripFragment
  ) where

import Data.Char qualified as Char
import Data.Text qualified as Text

resolveUri :: Maybe Text.Text -> Text.Text -> Text.Text
resolveUri mBase relative = case mBase of
  Nothing -> relative
  Just base
    | hasUriScheme relative -> relative
    | otherwise ->
        let
          (relativePath, relativeQuery, relativeFragment) = splitReference relative
          (scheme, afterScheme) = splitSchemePrefix (stripFragment base)
          (authority, basePathAndQuery) = splitAuthorityPrefix afterScheme
          (basePath, baseQuery) = Text.breakOn "?" basePathAndQuery
          targetPathAndQuery
            | Text.isPrefixOf "//" relativePath =
                let
                  (relativeAuthority, path) = splitAuthorityPrefix relativePath
                 in
                  relativeAuthority <> removeDotSegments path <> relativeQuery
            | Text.null relativePath =
                authority <> basePath <> (if Text.null relativeQuery then baseQuery else relativeQuery)
            | Text.isPrefixOf "/" relativePath = authority <> removeDotSegments relativePath <> relativeQuery
            | otherwise = authority <> removeDotSegments (mergePaths authority basePath relativePath) <> relativeQuery
         in
          scheme <> targetPathAndQuery <> relativeFragment

hasUriScheme :: Text.Text -> Bool
hasUriScheme t =
  let
    isSchemeChar ch =
      Char.isAsciiUpper ch
        || Char.isAsciiLower ch
        || Char.isDigit ch
        || ch == '+'
        || ch == '-'
        || ch == '.'
   in
    case Text.uncons t of
      Just (c, rest)
        | Char.isAsciiUpper c || Char.isAsciiLower c ->
            case Text.uncons (Text.dropWhile isSchemeChar rest) of
              Just (':', _) -> True
              _ -> False
      _ -> False

splitReference :: Text.Text -> (Text.Text, Text.Text, Text.Text)
splitReference reference =
  let
    (beforeFragment, fragment) = Text.breakOn "#" reference
    (path, query) = Text.breakOn "?" beforeFragment
   in
    (path, query, fragment)

splitSchemePrefix :: Text.Text -> (Text.Text, Text.Text)
splitSchemePrefix uri
  | hasUriScheme uri =
      let
        (scheme, rest) = Text.break (== ':') uri
       in
        (scheme <> Text.singleton ':', Text.drop 1 rest)
  | otherwise = (Text.empty, uri)

stripFragment :: Text.Text -> Text.Text
stripFragment = Text.takeWhile (/= '#')

splitAuthorityPrefix :: Text.Text -> (Text.Text, Text.Text)
splitAuthorityPrefix rest = case Text.stripPrefix "//" rest of
  Just afterSlashes ->
    let
      (authority, path) = Text.break (== '/') afterSlashes
     in
      ("//" <> authority, path)
  Nothing -> (Text.empty, rest)

removeDotSegments :: Text.Text -> Text.Text
removeDotSegments = go Text.empty
  where
    go out inp
      | Text.null inp = out
      | Just rest <- Text.stripPrefix "../" inp = go out rest
      | Just rest <- Text.stripPrefix "./" inp = go out rest
      | Just rest <- Text.stripPrefix "/./" inp = go out (Text.cons '/' rest)
      | inp == "/." = go out "/"
      | Just rest <- Text.stripPrefix "/../" inp = go (dropLastSegment out) (Text.cons '/' rest)
      | inp == "/.." = go (dropLastSegment out) "/"
      | inp == "." || inp == ".." = out
      | otherwise =
          let
            (segment, rest) = takeFirstSegment inp
           in
            go (out <> segment) rest

    takeFirstSegment inp =
      let
        (lead, body) =
          if Text.isPrefixOf "/" inp
            then (Text.singleton '/', Text.drop 1 inp)
            else (Text.empty, inp)
        (segment, rest) = Text.break (== '/') body
       in
        (lead <> segment, rest)

    dropLastSegment out = Text.dropEnd 1 (fst (Text.breakOnEnd "/" out))

mergePaths :: Text.Text -> Text.Text -> Text.Text -> Text.Text
mergePaths authority basePath relative
  | not (Text.null authority) && Text.null basePath = Text.cons '/' relative
  | otherwise = fst (Text.breakOnEnd "/" basePath) <> relative
