{- |
ECMA-262 regular expressions with the semantics JSON Schema gives the
@source@ and @patternProperties@ keywords and the @regex@ format.

A source has no flags. It is read with the grammar of the u flag and, when
that grammar refuses it, with the Annex B grammar of a source with no flag,
which accepts what many schemas in the wild contain, such as @\\&@ or a lone
brace. A source of the u flag grammar matches by code point. A source of the
Annex B grammar matches by UTF-16 code unit, as ECMA-262 matches a source
with no u flag.

Matching is case sensitive, @^@ and @$@ match only at the ends of the text, and
@.@ excludes the four line terminators. A modifier group such as @(?i:...)@
changes that for part of a source.

A source with nested quantifiers can take exponential time on a text it does
not match, as in any backtracking engine. A match is pure and allocates, so
'System.Timeout.timeout' can interrupt one that is forced inside it.
-}
module Text.Regex.Ecma262
  ( Regex.Regex
  , Regex.compile
  , Regex.CompileError (..)
  , Parse.Limit (..)
  , Regex.renderCompileError
  , Regex.isMatch
  , Regex.valid
  ) where

import Text.Regex.Ecma262.Internal.Parse qualified as Parse
import Text.Regex.Ecma262.Internal.Regex qualified as Regex
