module JsonSchema.Validate.Array
  ( validateArray
  , hasUniqueItems
  ) where

import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.HashSet qualified as HashSet
import Data.Hashable qualified as Hashable
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector

import JsonSchema qualified as Schema
import JsonSchema.Validate.Dialect qualified as Dialect
import JsonSchema.Validate.Error qualified as Error
import JsonSchema.Validate.Evaluation qualified as Evaluation
import JsonSchema.Validate.Node qualified as Node
import JsonSchema.Validate.Path qualified as Path
import JsonSchema.Validate.Registry qualified as Registry

validateArray ::
  Path.InstancePath ->
  Path.SchemaPath ->
  Registry.DynamicScope ->
  Schema.SchemaObjectF Node.NodeValidator ->
  Node.CompiledObj ->
  Aeson.Value ->
  Evaluation.Evaluation
validateArray ip sp scope obj compiled value = case value of
  Aeson.Array arr ->
    let
      validationVocab = Dialect.vocabularyFlagsValidation (Node.compiledVocabularyFlags compiled)
      applicatorVocab = Dialect.vocabularyFlagsApplicator (Node.compiledVocabularyFlags compiled)
      len = Vector.length arr
      prefixValidators = Node.compiledPrefixItems compiled
      prefixLen = Node.compiledPrefixItemsLength compiled
     in
      Foldable.fold
        [ if validationVocab
            then Evaluation.checkBound ip sp (fromIntegral len >=) (`Error.MinItemsFailed` len) (Schema.schemaMinItems obj)
            else Evaluation.emptyResult
        , if validationVocab
            then Evaluation.checkBound ip sp (fromIntegral len <=) (`Error.MaxItemsFailed` len) (Schema.schemaMaxItems obj)
            else Evaluation.emptyResult
        , if validationVocab
            then case Schema.schemaUniqueItems obj of
              Just True ->
                if hasUniqueItems arr
                  then Evaluation.emptyResult
                  else Evaluation.errorResult (Error.validationError ip sp Error.UniqueItemsFailed)
              _ -> Evaluation.emptyResult
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              foldMap
                ( \itemValidator ->
                    Vector.ifoldr
                      ( \j item ->
                          ( Evaluation.evaluateIndex
                              (prefixLen + j)
                              ( Node.runNodeValidator
                                  itemValidator
                                  (ip <> Path.instanceIndex (prefixLen + j))
                                  (sp <> Path.schemaSegment "items")
                                  scope
                                  item
                              )
                              <>
                          )
                      )
                      Evaluation.emptyResult
                      (Vector.drop prefixLen arr)
                )
                (Schema.schemaItems obj)
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              Foldable.fold
                ( zipWith
                    ( \i v ->
                        foldMap
                          ( Evaluation.evaluateIndex i
                              . Node.runNodeValidator
                                v
                                (ip <> Path.instanceIndex i)
                                (sp <> Path.schemaSegment "prefixItems" <> Path.schemaIndex i)
                                scope
                          )
                          (arr Vector.!? i)
                    )
                    [(0 :: Int) ..]
                    prefixValidators
                )
            else Evaluation.emptyResult
        , if applicatorVocab
            then
              foldMap
                ( \containsValidator ->
                    let
                      (matchedIndices, cycles) =
                        Vector.ifoldl'
                          ( \(matched, cycleAcc) i item ->
                              let
                                itemResult =
                                  Node.runNodeValidator
                                    containsValidator
                                    (ip <> Path.instanceIndex i)
                                    (sp <> Path.schemaSegment "contains")
                                    scope
                                    item
                               in
                                ( if Evaluation.isValid itemResult then IntSet.insert i matched else matched
                                , cycleAcc <> Evaluation.cycleErrorsOnly itemResult
                                )
                          )
                          (IntSet.empty, Evaluation.emptyResult)
                          arr
                      matchCount = IntSet.size matchedIndices
                      lowerBoundResult
                        | not validationVocab =
                            if matchCount >= 1
                              then Evaluation.emptyResult
                              else Evaluation.errorResult (Error.validationError ip sp Error.ContainsFailed)
                        | otherwise = case Schema.schemaMinContains obj of
                            Nothing ->
                              if matchCount >= 1
                                then Evaluation.emptyResult
                                else Evaluation.errorResult (Error.validationError ip sp Error.ContainsFailed)
                            Just 0 -> Evaluation.emptyResult
                            Just n ->
                              if matchCount >= fromIntegral n
                                then Evaluation.emptyResult
                                else Evaluation.errorResult (Error.validationError ip sp (Error.ContainsTooFew n matchCount))
                      upperBoundResult =
                        if validationVocab
                          then
                            Evaluation.checkBound
                              ip
                              sp
                              (\n -> matchCount <= fromIntegral n)
                              (`Error.ContainsTooMany` matchCount)
                              (Schema.schemaMaxContains obj)
                          else Evaluation.emptyResult
                      boundsResult = lowerBoundResult <> upperBoundResult
                     in
                      cycles
                        <> if Evaluation.isValid boundsResult
                          then Evaluation.withEvaluatedItems matchedIndices boundsResult
                          else boundsResult
                )
                (Schema.schemaContains obj)
            else Evaluation.emptyResult
        ]
  _ -> Evaluation.emptyResult

hasUniqueItems :: (Foldable t, Hashable.Hashable a) => t a -> Bool
hasUniqueItems xs = foldr (\x k seen -> not (HashSet.member x seen) && k (HashSet.insert x seen)) (const True) xs HashSet.empty
