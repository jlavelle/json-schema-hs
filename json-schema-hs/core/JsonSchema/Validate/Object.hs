module JsonSchema.Validate.Object
  ( validateObjectWithApplicators
  , validateObjectConstraints
  , validateUnevaluatedProperties
  , validateUnevaluatedItems
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.Map.Ordered qualified as OMap
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import JsonSchema qualified as Schema
import JsonSchema.Validate.Array qualified as Array
import JsonSchema.Validate.Atomic qualified as Atomic
import JsonSchema.Validate.Composition qualified as Composition
import JsonSchema.Validate.Conditional qualified as Conditional
import JsonSchema.Validate.Dialect qualified as Dialect
import JsonSchema.Validate.Error qualified as Error
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Path qualified as Path
import JsonSchema.Validate.Registry qualified as Registry

validateObjectWithApplicators ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Node.CompiledObj ->
  Aeson.Value ->
  Evaluation.Evaluation ->
  Evaluation.Evaluation
validateObjectWithApplicators ip sp scope obj compiled value applicatorResult =
  let
    validationVocab = Dialect.vocabularyFlagsValidation (Node.compiledVocabularyFlags compiled)
    applicatorVocab = Dialect.vocabularyFlagsApplicator (Node.compiledVocabularyFlags compiled)
    unevaluatedVocab = Dialect.vocabularyFlagsUnevaluated (Node.compiledVocabularyFlags compiled)
    common =
      applicatorResult
        <> if validationVocab
          then
            Atomic.validateType ip sp obj value
              <> Atomic.validateEnum ip sp obj value
              <> Atomic.validateConst ip sp obj value
          else Evaluation.emptyResult
    typeSpecific = case value of
      Aeson.Number _
        | validationVocab -> Atomic.validateNumeric ip sp obj (Node.compiledMultipleOf compiled) value
        | otherwise -> Evaluation.emptyResult
      Aeson.String _ ->
        ( if validationVocab
            then Atomic.validateString ip sp obj (Node.compiledStringPattern compiled) value
            else Evaluation.emptyResult
        )
          <> Atomic.validateFormat ip sp (Node.compiledFormat compiled) value
      Aeson.Array _ -> Array.validateArray ip sp scope obj compiled value
      Aeson.Object _ -> validateObjectConstraints ip sp scope obj compiled value
      _ -> Evaluation.emptyResult
    baseResult =
      common
        <> typeSpecific
        <> if applicatorVocab
          then
            Composition.validateComposition ip sp scope obj value
              <> Conditional.validateConditional ip sp scope obj value
          else Evaluation.emptyResult
   in
    if unevaluatedVocab
      then
        validateUnevaluatedItems ip sp scope obj value
          $ validateUnevaluatedProperties ip sp scope obj value baseResult
      else baseResult

validateObjectConstraints ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Node.CompiledObj ->
  Aeson.Value ->
  Evaluation.Evaluation
validateObjectConstraints ip sp scope obj compiled value = case value of
  Aeson.Object kvs ->
    let
      validationVocab = Dialect.vocabularyFlagsValidation (Node.compiledVocabularyFlags compiled)
      applicatorVocab = Dialect.vocabularyFlagsApplicator (Node.compiledVocabularyFlags compiled)
      numProps = KeyMap.size kvs
      propValidators = Node.compiledPropValidators compiled
      localCompiledPatternProps = Node.compiledPatternProperties compiled
     in
      Foldable.fold
        [ if validationVocab
            then
              foldMap
                ( foldMap
                    ( \k ->
                        if KeyMap.member (Key.fromText k) kvs
                          then Evaluation.emptyResult
                          else Evaluation.errorResult (Error.validationError ip sp (Error.RequiredMissing k))
                    )
                )
                (Schema.schemaRequired obj)
            else Evaluation.emptyResult
        , if validationVocab
            then
              Evaluation.checkBound
                ip
                sp
                (fromIntegral numProps >=)
                (`Error.MinPropertiesFailed` numProps)
                (Schema.schemaMinProperties obj)
            else Evaluation.emptyResult
        , if validationVocab
            then
              Evaluation.checkBound
                ip
                sp
                (fromIntegral numProps <=)
                (`Error.MaxPropertiesFailed` numProps)
                (Schema.schemaMaxProperties obj)
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              let
                runProperty k propValidator v =
                  let
                    name = Key.toText k
                   in
                    Evaluation.evaluateKey name
                      $ Node.runNodeValidator
                        propValidator
                        (ip <> Path.instanceSegment name)
                        (sp <> Path.schemaSegment "properties" <> Path.schemaSegment name)
                        scope
                        v
               in
                if Node.compiledPropValidatorCount compiled <= numProps
                  then
                    KeyMap.foldMapWithKey (\k propValidator -> foldMap (runProperty k propValidator) (KeyMap.lookup k kvs)) propValidators
                  else
                    KeyMap.foldMapWithKey
                      (\k v -> foldMap (\propValidator -> runProperty k propValidator v) (KeyMap.lookup k propValidators))
                      kvs
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              foldMap
                ( \compiledValidators ->
                    let
                      brokenPatternErrors =
                        foldMap
                          ( \(cp, _) -> case cp of
                              Node.Uncompilable src ->
                                Evaluation.errorResult
                                  (Error.validationError ip sp (Error.PatternCompileFailed "patternProperties" src))
                              Node.Compiled _ _ -> Evaluation.emptyResult
                          )
                          compiledValidators
                      perKey =
                        KeyMap.foldMapWithKey
                          ( \k v ->
                              foldMap
                                ( \(cp, patValidator) -> case cp of
                                    Node.Uncompilable _ -> Evaluation.emptyResult
                                    Node.Compiled src matches ->
                                      if matches . Key.toText $ k
                                        then
                                          Evaluation.evaluateKey (Key.toText k)
                                            $ Node.runNodeValidator
                                              patValidator
                                              (ip <> Path.instanceSegment (Key.toText k))
                                              (sp <> Path.schemaSegment "patternProperties" <> Path.schemaSegment src)
                                              scope
                                              v
                                        else Evaluation.emptyResult
                                )
                                compiledValidators
                          )
                          kvs
                     in
                      brokenPatternErrors <> perKey
                )
                localCompiledPatternProps
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              foldMap
                ( \additionalValidator ->
                    let
                      compiledPatternMatchers = Node.compiledAdditionalPatternMatchers compiled
                     in
                      KeyMap.foldMapWithKey
                        ( \k v ->
                            if isPropertyKnown propValidators compiledPatternMatchers k
                              then Evaluation.emptyResult
                              else
                                Evaluation.evaluateKey (Key.toText k)
                                  $ Node.runNodeValidator
                                    additionalValidator
                                    (ip <> Path.instanceSegment (Key.toText k))
                                    (sp <> Path.schemaSegment "additionalProperties")
                                    scope
                                    v
                        )
                        kvs
                )
                (Schema.schemaAdditionalProperties obj)
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              foldMap
                ( \nameValidator ->
                    foldMap
                      ( \k ->
                          let
                            name = Key.toText k
                           in
                            Node.runNodeValidator
                              nameValidator
                              (ip <> Path.instanceSegment name)
                              (sp <> Path.schemaSegment "propertyNames")
                              scope
                              (Aeson.String name)
                      )
                      . KeyMap.keys
                      $ kvs
                )
                (Schema.schemaPropertyNames obj)
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              foldMap
                ( foldMap
                    ( \(depKey, depValidator) ->
                        if KeyMap.member (Key.fromText depKey) kvs
                          then
                            Node.runNodeValidator
                              depValidator
                              ip
                              (sp <> Path.schemaSegment "dependentSchemas" <> Path.schemaSegment depKey)
                              scope
                              value
                          else Evaluation.emptyResult
                    )
                    . OMap.assocs
                )
                (Schema.schemaDependentSchemas obj)
            else Evaluation.emptyResult
        , foldMap
            ( foldMap
                ( \(depKey, dep) ->
                    if KeyMap.member (Key.fromText depKey) kvs
                      then case dep of
                        Schema.PropertyDependency reqKeys ->
                          foldMap
                            ( \k ->
                                if KeyMap.member (Key.fromText k) kvs
                                  then Evaluation.emptyResult
                                  else
                                    Evaluation.errorResult
                                      (Error.validationError ip sp (Error.DependentRequiredMissing "dependencies" depKey k))
                            )
                            reqKeys
                        Schema.SchemaDependency depValidator ->
                          Node.runNodeValidator
                            depValidator
                            ip
                            (sp <> Path.schemaSegment "dependencies" <> Path.schemaSegment depKey)
                            scope
                            value
                      else Evaluation.emptyResult
                )
                . OMap.assocs
            )
            (Schema.schemaDependencies obj)
        , if validationVocab
            then
              foldMap
                ( foldMap
                    ( \(depKey, reqKeys) ->
                        if KeyMap.member (Key.fromText depKey) kvs
                          then
                            foldMap
                              ( \k ->
                                  if KeyMap.member (Key.fromText k) kvs
                                    then Evaluation.emptyResult
                                    else
                                      Evaluation.errorResult
                                        (Error.validationError ip sp (Error.DependentRequiredMissing "dependentRequired" depKey k))
                              )
                              reqKeys
                          else Evaluation.emptyResult
                    )
                    . OMap.assocs
                )
                (Schema.schemaDependentRequired obj)
            else Evaluation.emptyResult
        ]
  _ -> Evaluation.emptyResult

isPropertyKnown :: KeyMap.KeyMap a -> [Text.Text -> Bool] -> Key.Key -> Bool
isPropertyKnown propValidators patternMatchers k =
  KeyMap.member k propValidators || any ($ Key.toText k) patternMatchers

validateUnevaluatedItems ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Aeson.Value ->
  Evaluation.Evaluation ->
  Evaluation.Evaluation
validateUnevaluatedItems ip sp scope obj value baseResult = case Schema.schemaUnevaluatedItems obj of
  Nothing -> baseResult
  Just unevalValidator -> case value of
    Aeson.Array arr ->
      let
        evaluated = Evaluation.evaluatedItems baseResult
        unevalResult =
          Vector.ifoldr
            ( \j v rest ->
                if IntSet.member j evaluated
                  then rest
                  else
                    let
                      inner =
                        Node.runNodeValidator
                          unevalValidator
                          (ip <> Path.instanceIndex j)
                          (sp <> Path.schemaSegment "unevaluatedItems")
                          scope
                          v
                     in
                      (if Evaluation.isValid inner then Evaluation.evaluateIndex j inner else Evaluation.dropEvaluated inner) <> rest
            )
            Evaluation.emptyResult
            arr
       in
        baseResult <> unevalResult
    _ -> baseResult

validateUnevaluatedProperties ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Aeson.Value ->
  Evaluation.Evaluation ->
  Evaluation.Evaluation
validateUnevaluatedProperties ip sp scope obj value baseResult = case Schema.schemaUnevaluatedProperties obj of
  Nothing -> baseResult
  Just unevalValidator -> case value of
    Aeson.Object kvs ->
      let
        evaluated = Evaluation.evaluatedProperties baseResult
        unevalResult =
          KeyMap.foldMapWithKey
            ( \k v ->
                if Set.member (Key.toText k) evaluated
                  then Evaluation.emptyResult
                  else
                    let
                      inner =
                        Node.runNodeValidator
                          unevalValidator
                          (ip <> Path.instanceSegment (Key.toText k))
                          (sp <> Path.schemaSegment "unevaluatedProperties")
                          scope
                          v
                     in
                      if Evaluation.isValid inner
                        then Evaluation.evaluateKey (Key.toText k) inner
                        else Evaluation.dropEvaluated inner
            )
            kvs
       in
        baseResult <> unevalResult
    _ -> baseResult
