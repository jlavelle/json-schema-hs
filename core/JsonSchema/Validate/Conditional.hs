module JsonSchema.Validate.Conditional
  ( validateConditional
  ) where

import Data.Aeson qualified as Aeson

import JsonSchema qualified as Schema
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Path qualified as Path
import JsonSchema.Validate.Registry qualified as Registry

validateConditional ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Aeson.Value ->
  Evaluation.Evaluation
validateConditional ip sp scope obj value =
  foldMap
    ( \ifValidator ->
        let
          ifResult = Node.runNodeValidator ifValidator ip (sp <> Path.schemaSegment "if") scope value
          ifAnnotations = Evaluation.annotationsOnly ifResult
         in
          if Evaluation.isValid ifResult
            then
              ifAnnotations
                <> foldMap
                  (\thenValidator -> Node.runNodeValidator thenValidator ip (sp <> Path.schemaSegment "then") scope value)
                  (Schema.schemaThen obj)
            else
              Evaluation.cycleErrorsOnly ifResult
                <> foldMap
                  (\elseValidator -> Node.runNodeValidator elseValidator ip (sp <> Path.schemaSegment "else") scope value)
                  (Schema.schemaElse obj)
    )
    (Schema.schemaIf obj)
