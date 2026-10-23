module JsonSchema.Fleece
  ( JsonSchemaFleece
  , schemaFleeceSchema
  , schemaObjectFleeceSchema
  , schemaTypeFleeceSchema
  , valueSchema
  ) where

import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Set.Ordered qualified as OSet
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC
import JsonSchema qualified as Schema
import Shrubbery qualified

import JsonSchema.OrderedProperties qualified as OrderedProperties

type JsonSchemaFleece f =
  ( FC.Fleece f
  , OrderedProperties.OrderedProperties f
  )

schemaFleeceSchema :: (JsonSchemaFleece f) => FC.Schema f Schema.Schema
schemaFleeceSchema =
  FC.describe "A JSON Schema, either a boolean or a schema object"
    . FC.transform schemaToUnion unionToSchema
    . FC.unionNamed (FC.unqualifiedName "Schema")
    $ FC.unionMember FC.boolean
      FC.#| FC.unionMember schemaObjectFleeceSchema

schemaToUnion :: Schema.Schema -> Shrubbery.Union '[Bool, Schema.SchemaObject]
schemaToUnion schema = case schema of
  Schema.BoolSchema b -> Shrubbery.unify b
  Schema.ObjectSchema obj -> Shrubbery.unify obj

unionToSchema :: Shrubbery.Union '[Bool, Schema.SchemaObject] -> Schema.Schema
unionToSchema =
  Shrubbery.dissectUnion
    ( Shrubbery.branchBuild
        . Shrubbery.branch @Bool Schema.BoolSchema
        . Shrubbery.branch @Schema.SchemaObject Schema.ObjectSchema
        $ Shrubbery.branchEnd
    )

schemaObjectFleeceSchema :: (JsonSchemaFleece f) => FC.Schema f Schema.SchemaObject
schemaObjectFleeceSchema =
  FC.object
    $ FC.constructor Schema.SchemaObjectF
      FC.#+ FC.optional
        "type"
        Schema.schemaType
        (FC.describe "Restricts the instance to one or more primitive types" schemaTypesFleeceSchema)
      FC.#+ FC.optional
        "enum"
        Schema.schemaEnum
        (FC.describe "Restricts the instance to a fixed set of values" (FC.list valueSchema))
      FC.#+ FC.optional "const" Schema.schemaConst (FC.describe "Restricts the instance to a single value" valueSchema)
      FC.#+ FC.optional
        "properties"
        Schema.schemaProperties
        ( FC.describe "Validation rules for known object properties, matched by exact name"
            $ OrderedProperties.orderedProperties schemaFleeceSchema
        )
      FC.#+ FC.optional
        "required"
        Schema.schemaRequired
        (FC.describe "Properties that must be present in the object instance" requiredFleeceSchema)
      FC.#+ FC.optional
        "additionalProperties"
        Schema.schemaAdditionalProperties
        (FC.describe "Schema for properties not matched by 'properties' or 'patternProperties'" schemaFleeceSchema)
      FC.#+ FC.optional
        "patternProperties"
        Schema.schemaPatternProperties
        ( FC.describe "Validation rules for object properties matched by regular expression"
            $ OrderedProperties.orderedProperties schemaFleeceSchema
        )
      FC.#+ FC.optional
        "propertyNames"
        Schema.schemaPropertyNames
        (FC.describe "Schema that all property names must validate against" schemaFleeceSchema)
      FC.#+ FC.optional
        "minProperties"
        Schema.schemaMinProperties
        (FC.describe "Minimum number of properties in the object instance" FC.word)
      FC.#+ FC.optional
        "maxProperties"
        Schema.schemaMaxProperties
        (FC.describe "Maximum number of properties in the object instance" FC.word)
      FC.#+ FC.optional
        "items"
        Schema.schemaItems
        (FC.describe "Schema for array items not covered by 'prefixItems'" schemaFleeceSchema)
      FC.#+ FC.optional
        "prefixItems"
        Schema.schemaPrefixItems
        (FC.describe "Positional schemas for validating array items by index" (FC.nonEmpty schemaFleeceSchema))
      FC.#+ FC.optional
        "contains"
        Schema.schemaContains
        (FC.describe "Requires at least one array item to validate against this schema" schemaFleeceSchema)
      FC.#+ FC.optional
        "minContains"
        Schema.schemaMinContains
        (FC.describe "Minimum number of array items that must validate against 'contains'" FC.word)
      FC.#+ FC.optional
        "maxContains"
        Schema.schemaMaxContains
        (FC.describe "Maximum number of array items that may validate against 'contains'" FC.word)
      FC.#+ FC.optional "minItems" Schema.schemaMinItems (FC.describe "Minimum number of items in the array instance" FC.word)
      FC.#+ FC.optional "maxItems" Schema.schemaMaxItems (FC.describe "Maximum number of items in the array instance" FC.word)
      FC.#+ FC.optional
        "uniqueItems"
        Schema.schemaUniqueItems
        (FC.describe "When true, all array items must be distinct" FC.boolean)
      FC.#+ FC.optional "minLength" Schema.schemaMinLength (FC.describe "Minimum length of the string instance" FC.word)
      FC.#+ FC.optional "maxLength" Schema.schemaMaxLength (FC.describe "Maximum length of the string instance" FC.word)
      FC.#+ FC.optional
        "pattern"
        Schema.schemaPattern
        (FC.describe "ECMA-262 regular expression that the string instance must match" FC.text)
      FC.#+ FC.optional
        "format"
        Schema.schemaFormat
        (FC.describe "Semantic format identifier for the string instance (e.g. 'date-time', 'email')" FC.text)
      FC.#+ FC.optional
        "minimum"
        Schema.schemaMinimum
        (FC.describe "Inclusive lower bound for the numeric instance" FC.number)
      FC.#+ FC.optional
        "maximum"
        Schema.schemaMaximum
        (FC.describe "Inclusive upper bound for the numeric instance" FC.number)
      FC.#+ FC.optional
        "exclusiveMinimum"
        Schema.schemaExclusiveMinimum
        (FC.describe "Exclusive lower bound for the numeric instance" FC.number)
      FC.#+ FC.optional
        "exclusiveMaximum"
        Schema.schemaExclusiveMaximum
        (FC.describe "Exclusive upper bound for the numeric instance" FC.number)
      FC.#+ FC.optional
        "multipleOf"
        Schema.schemaMultipleOf
        ( FC.describe
            "The numeric instance must be a multiple of this value (strictly positive)"
            multipleOfFleeceSchema
        )
      FC.#+ FC.optional
        "allOf"
        Schema.schemaAllOf
        (FC.describe "The instance must validate against all of these schemas" (FC.nonEmpty schemaFleeceSchema))
      FC.#+ FC.optional
        "anyOf"
        Schema.schemaAnyOf
        (FC.describe "The instance must validate against at least one of these schemas" (FC.nonEmpty schemaFleeceSchema))
      FC.#+ FC.optional
        "oneOf"
        Schema.schemaOneOf
        (FC.describe "The instance must validate against exactly one of these schemas" (FC.nonEmpty schemaFleeceSchema))
      FC.#+ FC.optional
        "not"
        Schema.schemaNot
        (FC.describe "The instance must not validate against this schema" schemaFleeceSchema)
      FC.#+ FC.optional
        "if"
        Schema.schemaIf
        (FC.describe "Condition schema for conditional validation with 'then' and 'else'" schemaFleeceSchema)
      FC.#+ FC.optional
        "then"
        Schema.schemaThen
        (FC.describe "Schema applied when 'if' validates successfully" schemaFleeceSchema)
      FC.#+ FC.optional
        "else"
        Schema.schemaElse
        (FC.describe "Schema applied when 'if' does not validate successfully" schemaFleeceSchema)
      FC.#+ FC.optional
        "dependentSchemas"
        Schema.schemaDependentSchemas
        ( FC.describe "Schemas that apply when their corresponding property is present"
            $ OrderedProperties.orderedProperties schemaFleeceSchema
        )
      FC.#+ FC.optional
        "dependentRequired"
        Schema.schemaDependentRequired
        ( FC.describe "Property names that must be present when their corresponding property is present"
            $ OrderedProperties.orderedProperties (FC.list FC.text)
        )
      FC.#+ FC.optional
        "$id"
        Schema.schemaId
        (FC.describe "Declares a base URI for the schema" schemaIdFleeceSchema)
      FC.#+ FC.optional
        "$anchor"
        Schema.schemaAnchor
        ( FC.describe "Declares a plain name fragment for referencing with $ref"
            $ anchorFleeceSchema Schema.anchorToText Schema.Anchor
        )
      FC.#+ FC.optional
        "$dynamicAnchor"
        Schema.schemaDynamicAnchor
        ( FC.describe "Declares a dynamic anchor for resolution with $dynamicRef"
            $ anchorFleeceSchema Schema.dynamicAnchorToText Schema.DynamicAnchor
        )
      FC.#+ FC.optional
        "$ref"
        Schema.schemaRef
        (FC.describe "References another schema by URI" (FC.coerceSchemaAnonymous FC.text))
      FC.#+ FC.optional
        "$dynamicRef"
        Schema.schemaDynamicRef
        ( FC.describe "Dynamically references another schema, resolved via the dynamic scope"
            $ FC.coerceSchemaAnonymous FC.text
        )
      FC.#+ FC.optional
        "$defs"
        Schema.schemaDefs
        ( FC.describe "Reusable schema definitions, referenced via $ref"
            $ OrderedProperties.orderedProperties schemaFleeceSchema
        )
      FC.#+ FC.optional
        "unevaluatedProperties"
        Schema.schemaUnevaluatedProperties
        ( FC.describe
            "Schema for properties not evaluated by adjacent 'properties', 'patternProperties', or 'additionalProperties'"
            schemaFleeceSchema
        )
      FC.#+ FC.optional
        "unevaluatedItems"
        Schema.schemaUnevaluatedItems
        ( FC.describe
            "Schema for array items not evaluated by adjacent 'prefixItems', 'items', or 'contains'"
            schemaFleeceSchema
        )
      FC.#+ FC.optional
        "dependencies"
        Schema.schemaDependencies
        ( FC.describe "Legacy keyword combining dependentSchemas and dependentRequired"
            $ OrderedProperties.orderedProperties dependencyFleeceSchema
        )
      FC.#+ FC.optional "title" Schema.schemaTitle (FC.describe "Short human-readable label for the schema" FC.text)
      FC.#+ FC.optional
        "description"
        Schema.schemaDescription
        (FC.describe "Detailed human-readable explanation of the schema's purpose" FC.text)
      FC.#+ FC.optional
        "$schema"
        Schema.schemaMetaschema
        (FC.describe "URI of the metaschema (dialect) for this schema" (FC.coerceSchemaAnonymous FC.text))
      FC.#+ FC.optional
        "$vocabulary"
        Schema.schemaVocabulary
        ( FC.describe
            "Vocabularies declared by a metaschema, with required flags"
            vocabularyFleeceSchema
        )
      FC.#* FC.additionalFields Schema.schemaExtraKeywords valueSchema

schemaTypesFleeceSchema :: (FC.Fleece f) => FC.Schema f (NEL.NonEmpty Schema.SchemaType)
schemaTypesFleeceSchema =
  let
    typesToEither types = case types of
      single NEL.:| [] -> Left single
      _ -> Right types
   in
    FC.describe "A single type name or array of type names"
      . FC.transform typesToEither (either NEL.singleton id)
      $ FC.eitherOf schemaTypeFleeceSchema (FC.nonEmpty schemaTypeFleeceSchema)

schemaTypeFleeceSchema :: (FC.Fleece f) => FC.Schema f Schema.SchemaType
schemaTypeFleeceSchema =
  FC.describe "A JSON Schema primitive type name"
    $ FC.boundedEnum Schema.schemaTypeToText

valueSchema :: (FC.Fleece f) => FC.Schema f Aeson.Value
valueSchema =
  FC.describe "An arbitrary JSON value" FA.aesonValue

requiredFleeceSchema :: (FC.Fleece f) => FC.Schema f (OSet.OSet Text.Text)
requiredFleeceSchema =
  FC.validate Foldable.toList Schema.validateRequired (FC.list FC.text)

multipleOfFleeceSchema :: (FC.Fleece f) => FC.Schema f Schema.PositiveScientific
multipleOfFleeceSchema =
  FC.validate Schema.positiveScientificToScientific Schema.validateMultipleOf FC.number

schemaIdFleeceSchema :: (FC.Fleece f) => FC.Schema f Schema.SchemaId
schemaIdFleeceSchema =
  FC.validate Schema.schemaIdToText Schema.validateSchemaId FC.text

anchorFleeceSchema :: (FC.Fleece f, Typeable.Typeable a) => (a -> Text.Text) -> (Text.Text -> a) -> FC.Schema f a
anchorFleeceSchema toText fromText =
  FC.validate toText (fmap fromText . Schema.validateAnchorName) FC.text

dependencyFleeceSchema :: (JsonSchemaFleece f) => FC.Schema f Schema.Dependency
dependencyFleeceSchema =
  FC.describe "A dependency, either a list of required property names or a schema"
    . FC.transform dependencyToUnion unionToDependency
    . FC.unionNamed (FC.unqualifiedName "Dependency")
    $ FC.unionMember (FC.list FC.text)
      FC.#| FC.unionMember schemaFleeceSchema

dependencyToUnion :: Schema.Dependency -> Shrubbery.Union '[[Text.Text], Schema.Schema]
dependencyToUnion dep = case dep of
  Schema.PropertyDependency keys -> Shrubbery.unify keys
  Schema.SchemaDependency schema -> Shrubbery.unify schema

unionToDependency :: Shrubbery.Union '[[Text.Text], Schema.Schema] -> Schema.Dependency
unionToDependency =
  Shrubbery.dissectUnion
    ( Shrubbery.branchBuild
        . Shrubbery.branch @[Text.Text] Schema.PropertyDependency
        . Shrubbery.branch @Schema.Schema Schema.SchemaDependency
        $ Shrubbery.branchEnd
    )

vocabularyFleeceSchema :: (FC.Fleece f) => FC.Schema f (Map.Map Text.Text Bool)
vocabularyFleeceSchema =
  FC.map FC.boolean
