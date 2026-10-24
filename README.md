# json-schema-hs

A Haskell implementation of JSON Schema draft 2020-12, in four packages:

- **regex-ecma262**: a pure Haskell ECMA-262 regex engine for `pattern`, `patternProperties`, and `format: regex`. It is a port of the OCaml `ecmaregex` engine of [Corvus.JsonSchema](https://github.com/corvus-dotnet/Corvus.JsonSchema) by Endjin, and is licensed, like Corvus.JsonSchema, under the Apache License 2.0. A pattern that the `u` flag grammar refuses is read with the Annex B grammar, and `format: regex` accepts only the `u` flag grammar. Its tests are Corvus.JsonSchema's V8 conformance data.
- **json-schema-hs**: the validator and an aeson codec for schemas. Internals are in its public `json-schema-hs:core` sublibrary.
- **json-schema-hs-fleece**: a [json-fleece](https://github.com/flipstone/json-fleece) codec for schemas, a Fleece decoder that keeps the key order of `properties`, `$defs` and the other keyword maps, and a Fleece backend that generates schemas. Only dependents of this package need json-fleece.
- **json-schema-hs-test**: the test suites of both packages, and the helpers they share. It is not meant for release. The test suites live outside the packages they test because cabal cannot solve a test suite that depends, through the shared helpers, on its own package.

## Quickstart

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson qualified as Aeson
import Data.Text.IO qualified as TextIO
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate

main :: IO ()
main = do
  schema <- either fail pure (Aeson.eitherDecode @Schema.Schema "{\"type\": \"integer\"}")
  let validator = Validate.compile Validate.emptyRegistry schema
  print (Validate.isValid validator (Aeson.Number 1))
  TextIO.putStrLn (Validate.renderErrors (Validate.validate validator (Aeson.String "x")))
  print (Validate.isValidOn schema Aeson.Null)
  print (null (Validate.validateOn schema (Aeson.Number 2)))
```

## Coming from another library

| You know | Here |
| --- | --- |
| `ajv.compile` / Rust `validator_for` | `compile registry schema` |
| `validate(data)` / `Validator::validate` | `validate validator value` |
| `jsonschema.validate(instance, schema)` | `validateOn schema value` |
| `is_valid` | `isValid` / `isValidOn` |
| `error.keyword` / `ValidationErrorKind` | `errorKeyword` / `errorKind` |
| `instancePath` | `renderJsonPointer (errorInstancePath e)` |
| `err.context` | `errorContext` |
| `ajv.addSchema(obj, uri)` | `insertResource uri schema` |
| `ajv.addFormat(name, pred)` | `withFormat name pred` |
| `check_schema` / `validateSchema` | `checkSchema` |
| `dereference` | `dereference` |
| `Draft` | resolved from `$schema` |

## Building

Fetch the official test suite once:

```shell
git submodule update --init
```

Then format, lint, and build:

```shell
scripts/build
```

`scripts/test` runs the tests, and `scripts/bench` saves or compares benchmark baselines.

`cabal.project` enables each package's `ci` flag, which turns on `-Weverything -Werror` and the [henforcer](https://github.com/flipstone/henforcer) import rules. The flag is off by default, so packages that depend on these build without either.

## Exclusions

Conformance is tested against the official [JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite). These parts are excluded:

- Drafts other than 2020-12 (`optional/cross-draft`).
- Full RFC grammars for the `hostname`, `uri`, and `uri-reference` formats. `uri` only checks for a scheme, and `uri-reference` only rejects the empty string.
- The `iri` and `iri-reference` formats. Unknown formats are ignored.
- IDN decoding for the `idn-email` and `idn-hostname` formats. They accept ASCII only.
