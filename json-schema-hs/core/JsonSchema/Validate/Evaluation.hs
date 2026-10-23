module JsonSchema.Validate.Evaluation
  ( Evaluation (..)
  , emptyResult
  , errorResult
  , cycleResult
  , cycleErrorsOnly
  , checkBound
  , validationErrors
  , evaluatedProperties
  , evaluatedItems
  , withEvaluated
  , withEvaluatedItems
  , dropEvaluated
  , evaluateKey
  , evaluateIndex
  , annotationsOnly
  , isValid
  ) where

import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Data.Text qualified as Text

import JsonSchema.Validate.Error qualified as Error
import JsonSchema.Validate.Path qualified as Path

data Evaluation
  = EmptyResult
  | NonEmptyResult ![Error.ValidationError] (Set.Set Text.Text) IntSet.IntSet ![Error.ValidationError]

instance Semigroup Evaluation where
  EmptyResult <> r = r
  l <> EmptyResult = l
  NonEmptyResult le lp li lc <> NonEmptyResult re rp ri rc =
    NonEmptyResult (le <> re) (lp <> rp) (li <> ri) (lc <> rc)

instance Monoid Evaluation where
  mempty = EmptyResult

emptyResult :: Evaluation
emptyResult = EmptyResult

errorResult :: Error.ValidationError -> Evaluation
errorResult err = NonEmptyResult [err] Set.empty IntSet.empty []

cycleResult :: Error.ValidationError -> Evaluation
cycleResult err = NonEmptyResult [] Set.empty IntSet.empty [err]

cycleErrorsOnly :: Evaluation -> Evaluation
cycleErrorsOnly r = case r of
  NonEmptyResult _ _ _ cycles@(_ : _) -> NonEmptyResult [] Set.empty IntSet.empty cycles
  _ -> EmptyResult

checkBound ::
  Path.InstancePath ->
  Path.SchemaPath ->
  (bound -> Bool) ->
  (bound -> Error.ValidationErrorKind) ->
  Maybe bound ->
  Evaluation
checkBound ip sp accepts mkKind =
  foldMap
    ( \bound ->
        if accepts bound
          then emptyResult
          else errorResult (Error.validationError ip sp (mkKind bound))
    )

validationErrors :: Evaluation -> [Error.ValidationError]
validationErrors r = case r of
  EmptyResult -> []
  NonEmptyResult errs _ _ [] -> errs
  NonEmptyResult errs _ _ cycles -> errs <> cycles

evaluatedProperties :: Evaluation -> Set.Set Text.Text
evaluatedProperties r = case r of
  EmptyResult -> Set.empty
  NonEmptyResult _ props _ _ -> props

evaluatedItems :: Evaluation -> IntSet.IntSet
evaluatedItems r = case r of
  EmptyResult -> IntSet.empty
  NonEmptyResult _ _ idxs _ -> idxs

withEvaluated :: Set.Set Text.Text -> Evaluation -> Evaluation
withEvaluated keys r = case r of
  EmptyResult -> NonEmptyResult [] keys IntSet.empty []
  NonEmptyResult errs props idxs cycles -> NonEmptyResult errs (props <> keys) idxs cycles

withEvaluatedItems :: IntSet.IntSet -> Evaluation -> Evaluation
withEvaluatedItems idxs r = case r of
  EmptyResult -> NonEmptyResult [] Set.empty idxs []
  NonEmptyResult errs props i cycles -> NonEmptyResult errs props (i <> idxs) cycles

dropEvaluated :: Evaluation -> Evaluation
dropEvaluated r = case r of
  EmptyResult -> EmptyResult
  NonEmptyResult errs _ _ cycles -> NonEmptyResult errs Set.empty IntSet.empty cycles

evaluateKey :: Text.Text -> Evaluation -> Evaluation
evaluateKey k = withEvaluated (Set.singleton k) . dropEvaluated

evaluateIndex :: Int -> Evaluation -> Evaluation
evaluateIndex i = withEvaluatedItems (IntSet.singleton i) . dropEvaluated

annotationsOnly :: Evaluation -> Evaluation
annotationsOnly r = case r of
  EmptyResult -> EmptyResult
  NonEmptyResult _ props idxs cycles -> NonEmptyResult [] props idxs cycles

isValid :: Evaluation -> Bool
isValid r = case r of
  EmptyResult -> True
  NonEmptyResult errs _ _ cycles -> null errs && null cycles
