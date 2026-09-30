module JsonSchema.Validate.Composition
  ( validateComposition
  ) where

import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable

import JsonSchema qualified as Schema
import JsonSchema.Validate.Error qualified as Error
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Path qualified as Path
import JsonSchema.Validate.Registry qualified as Registry

validateComposition ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Aeson.Value ->
  Evaluation.Evaluation
validateComposition ip sp scope obj value =
  let
    runAll keyword =
      zipWith
        (\i v -> Node.runNodeValidator v ip (sp <> Path.schemaSegment keyword <> Path.schemaIndex i) scope value)
        [0 ..]
        . Foldable.toList
   in
    Foldable.fold
      [ foldMap (Foldable.fold . runAll "allOf") (Schema.schemaAllOf obj)
      , foldMap
          ( \validators ->
              let
                results = runAll "anyOf" validators
                passing = filter Evaluation.isValid results
                cycles = foldMap Evaluation.cycleErrorsOnly results
               in
                case passing of
                  [] ->
                    Evaluation.errorResult
                      (Error.validationErrorWithContext ip sp Error.AnyOfFailed (foldMap Evaluation.validationErrors results))
                      <> cycles
                  _ -> Foldable.fold passing <> cycles
          )
          (Schema.schemaAnyOf obj)
      , foldMap
          ( \validators ->
              let
                results = runAll "oneOf" validators
                cycles = foldMap Evaluation.cycleErrorsOnly results
               in
                (<> cycles) $ case filter Evaluation.isValid results of
                  [single] -> single
                  [] ->
                    Evaluation.errorResult
                      (Error.validationErrorWithContext ip sp (Error.OneOfFailed 0) (foldMap Evaluation.validationErrors results))
                  passing -> Evaluation.errorResult (Error.validationError ip sp (Error.OneOfFailed (length passing)))
          )
          (Schema.schemaOneOf obj)
      , foldMap
          ( \notValidator ->
              let
                notResult = Node.runNodeValidator notValidator ip (sp <> Path.schemaSegment "not") scope value
               in
                if Evaluation.isValid notResult
                  then Evaluation.errorResult (Error.validationError ip sp Error.NotMatched)
                  else Evaluation.cycleErrorsOnly notResult
          )
          (Schema.schemaNot obj)
      ]
