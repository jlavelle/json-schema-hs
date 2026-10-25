---
name: json-schema-hs
description: >-
  Validate JSON against JSON Schema draft 2020-12 in Haskell with
  json-schema-hs, and decode, encode, or generate schemas through
  json-fleece with json-schema-hs-fleece. Use when reading or writing `.hs`
  code that imports `JsonSchema`, `JsonSchema.Validate`, `JsonSchema.Fleece`,
  `JsonSchema.Generate`, or `JsonSchema.OrderedDecoder`; when the user wants
  to validate an `Aeson.Value` against a schema, resolve `$ref`s to other
  documents, register custom formats, or produce a JSON Schema from a Fleece
  schema; or on any of `compile`, `validate`, `validateOn`, `isValid`,
  `isValidOn`, `Validator`, `Registry`, `emptyRegistry`, `addSchema`,
  `insertResource`, `withFormat`, `withFormatAssertion`, `checkSchema`,
  `dereference`, `ValidationError`, `ValidationErrorKind`, `errorContext`,
  `renderErrors`, `renderJsonPointer`, `SchemaObjectF`, `emptySchema`,
  `schemaFleeceSchema`, or `toJsonSchema`. Out of scope — writing Fleece
  schemas (`json-fleece`), the `Union` machinery underneath
  (`shrubbery`).
---

# json-schema-hs

A `Schema` is the decoded JSON Schema document. `compile` turns it and a `Registry` of other documents into a `Validator`, and `validate` runs the validator against an `Aeson.Value`, returning a flat list of errors. An empty list means valid. Only draft 2020-12 is supported, whatever `$schema` says.

```haskell
schema <- either fail pure (Aeson.eitherDecode @Schema.Schema bytes)
let validator = Validate.compile Validate.emptyRegistry schema
Validate.validate validator value -- [ValidationError]
Validate.renderErrors (Validate.validate validator value) -- one line per error
```

Worked examples for every pattern below live in [`references/Example.hs`](references/Example.hs).

## Provisioning

Not on Hackage. Depend on the `json-schema-hs` package for `JsonSchema` and `JsonSchema.Validate`. Depend on the `json-schema-hs-fleece` package for the `JsonSchema.Fleece`, `JsonSchema.Generate`, and `JsonSchema.OrderedDecoder` modules. Only `json-schema-hs-fleece` needs the `json-fleece`, `shrubbery`, and `bounded-text` pins from the `json-fleece` skill. List the subdirectory of each package you use.

```cabal
source-repository-package
  type: git
  location: https://github.com/flipstone/json-schema-hs.git
  tag: <main SHA>
  subdir:
    json-schema-hs
    json-schema-hs-fleece
```

## API Reference

```haskell
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate


-- Validation. compile never fails: problems in the schema surface as
-- errors from validate. Compile once and reuse the Validator.

compile :: Registry -> Schema -> Validator
validate :: Validator -> Aeson.Value -> [ValidationError]
isValid :: Validator -> Aeson.Value -> Bool
validateOn :: Schema -> Aeson.Value -> [ValidationError] -- compile emptyRegistry, then validate
isValidOn :: Schema -> Aeson.Value -> Bool


-- Errors. Paths are lists of segments. renderJsonPointer turns one into
-- "/a/0/b"; the root renders as "".

data ValidationError = ValidationError
  { errorInstancePath :: [Text] -- where in the value
  , errorSchemaPath :: [Text] -- where in the schema, ending in the keyword
  , errorKind :: ValidationErrorKind
  , errorContext :: [ValidationError] -- branch errors, for anyOf and for oneOf with zero matches only
  }

errorKeyword :: ValidationError -> Maybe Text -- Just "required", Just "type", ...; Nothing for a false schema
errorMessage :: ValidationError -> Text -- the message without the paths
renderValidationError :: ValidationError -> Text -- "/title [/properties/title/minLength] string shorter than minLength 1"
renderErrors :: [ValidationError] -> Text -- newline-separated, no trailing newline
renderJsonPointer :: [Text] -> Text

-- One constructor per keyword, with the limit and the actual value where
-- there is one: RequiredMissing property, TypeMismatch expected actual,
-- PatternFailed pattern actual, FormatFailed format actual, AnyOfFailed,
-- OneOfFailed matchCount, RefUnresolved keyword ref, ... `:browse` lists them all.
data ValidationErrorKind


-- Registry. Holds the documents that $ref can reach outside the schema
-- being compiled, plus format settings. A registration replaces any
-- resource already registered under the same URI.

emptyRegistry :: Registry
addSchema :: Schema -> Registry -> Registry -- keyed by every $id inside
insertResource :: Text -> Schema -> Registry -> Registry -- keyed by the URI and every $id inside
withFormat :: Format -> FormatPredicate -> Registry -> Registry
withFormatAssertion :: FormatAssertion -> Registry -> Registry

newtype Format = Format {formatToText :: Text}
newtype FormatPredicate = FormatPredicate {runFormatPredicate :: Text -> Bool}
data FormatAssertion = FormatAssertionByVocabulary | FormatAssertionAlways -- combined with Max: Always cannot be undone


-- Static checks and rewriting.

checkSchema :: Registry -> Schema -> Either SchemaError () -- first problem only
renderSchemaError :: SchemaError -> Text
dereference :: Registry -> Schema -> Schema -- inlines $refs; a recursive one stays a $ref


-- Schema construction (module JsonSchema).

data Schema = BoolSchema Bool | ObjectSchema SchemaObject
type SchemaObject = SchemaObjectF Schema -- one Maybe field per keyword: schemaType, schemaProperties, schemaRequired, ...
emptySchema :: SchemaObject -- every keyword absent
```

## Patterns

Compile once, validate many. `compile` resolves references, builds the dialect, and compiles every pattern. `validateOn` and `isValidOn` repeat all of that on each call, so keep them for one-off checks.

```haskell
taskValidator :: Validate.Validator
taskValidator = Validate.compile Validate.emptyRegistry taskJsonSchema
```

Branch on validity with `NonEmpty`, so the invalid case cannot hold an empty list:

```haskell
checkTask :: Aeson.Value -> ValidationOutcome
checkTask =
  maybe Valid Invalid . NEL.nonEmpty . Validate.validate taskValidator
```

A `$ref` to another document — register that document. `addSchema` keys it by its `$id`. Use `insertResource uri` when the document has no `$id`, or is referenced by a URI other than its `$id`.

```haskell
let registry = Validate.addSchema address Validate.emptyRegistry
Validate.compile registry person -- person has {"$ref": "https://example.com/address"}
```

Formats annotate by default. `{"format": "email"}` accepts `"nope"` unless the registry asserts formats:

```haskell
Validate.withFormatAssertion Validate.FormatAssertionAlways Validate.emptyRegistry
```

`FormatAssertionByVocabulary`, the default, asserts builtin formats only when the schema's metaschema enables the format-assertion vocabulary. A custom format from `withFormat` always asserts, and registering a builtin name replaces that builtin and makes it assert. Unknown format names always pass.

```haskell
Validate.withFormat (Validate.Format "team-code") (Validate.FormatPredicate isTeamCode) Validate.emptyRegistry
```

User-facing errors — match on `errorKind` and recurse into `errorContext` for `anyOf`:

```haskell
explainFailure err =
  let location = Validate.renderJsonPointer (Validate.errorInstancePath err)
   in case Validate.errorKind err of
        Validate.RequiredMissing field -> location <> " is missing " <> field
        Validate.AnyOfFailed -> location <> " matched no branch: " <> T.intercalate "; " (fmap explainFailure (Validate.errorContext err))
        _ -> Validate.renderValidationError err
```

Reject a broken schema up front. `compile` accepts a dangling `$ref` and every value then fails with `RefUnresolved`. When the schema comes from a user or a config file, run `checkSchema` with the same registry first:

```haskell
case Validate.checkSchema registry schema of
  Left err -> Left (Validate.renderSchemaError err)
  Right () -> Right (Validate.compile registry schema)
```

`checkSchema` is not a metaschema check. The 2020-12 metaschemas are built in, so a schema that refers to one validates schema documents with an empty registry:

```haskell
-- metaschema = {"$ref": "https://json-schema.org/draft/2020-12/schema"}
Validate.validate (Validate.compile Validate.emptyRegistry metaschema) schemaDocumentValue
```

A schema built in Haskell — record update on `emptySchema`:

```haskell
Schema.ObjectSchema
  Schema.emptySchema
    { Schema.schemaType = Just (NEL.singleton Schema.TypeObject)
    , Schema.schemaProperties = Just (OMap.fromList [("name", stringSchema)])
    , Schema.schemaRequired = Just (OSet.fromList ["name"])
    }
```

`schemaProperties`, `schemaDefs`, and the other keyword maps are `OMap`s from `ordered-containers`, and `schemaRequired` is an `OSet`, so both keep their document order. Keywords this library does not model land in `schemaExtraKeywords` and round-trip unchanged.

## Fleece Support

`JsonSchema.Fleece` describes the schema document itself as a Fleece schema, so any Fleece backend can read or write schemas:

```haskell
FA.encode SchemaFleece.schemaFleeceSchema schema
OrderedDecoder.decode SchemaFleece.schemaFleeceSchema bytes -- keeps document key order
```

Decode with `OrderedDecoder.decode` (or `decodeStrict`) when key order matters, for example to show `properties` in the order the author wrote them. The aeson `FromJSON` instance and `FA.decode` both go through an aeson `Object`, which sorts keys: `{"b": .., "a": ..}` comes back as `a, b`. Encoding keeps the order of the `OMap` either way.

`JsonSchema.Generate` is a Fleece backend that produces a JSON Schema, so a value's Fleece schema can validate JSON from outside before decoding it:

```haskell
taskJsonSchema :: Schema.Schema
taskJsonSchema = Generate.toJsonSchema taskSchema
```

What the generated schema says, beyond the obvious mapping:

- Objects allow unknown keys unless the Fleece object has `additionalFields`, which becomes `additionalProperties`.
- `nullable s` becomes `anyOf [s, {"type": "null"}]`. `union` becomes `anyOf`. `taggedUnion` becomes `oneOf` with a `const` tag property first in each member, so structured output writes the tag before the fields.
- A named schema that refers to itself is hoisted into `$defs` under its Fleece name. Nothing else goes into `$defs`.
- `describe` text becomes `description`. The outermost `describe` wins, and a `$ref` to a recursive schema keeps the `description` of its use site.
- The checks inside `validate` and the inner shape of `jsonString` are dropped. A value can pass the generated schema and still fail `FA.decode`.

## Anti-Patterns

- `validateOn schema` inside a loop or a request handler: recompiles the schema on every call. Compile once and share the `Validator`.
- Trusting `format` for `iri` or `iri-reference`: they are not builtin, so they pass unless registered with `withFormat`. The `hostname`, `uri`, and `idn-*` validators are permissive.
- A hand-written JSON Schema next to a Fleece schema for the same type: they drift. Generate it with `toJsonSchema`.
- `Aeson.decode` for a schema an editor or a diff will display: the keyword maps come back sorted. Use `OrderedDecoder.decode`.
