module TestUtil
  ( assertValidates
  , assertRejects
  , validatesProp
  , validatePure
  , withinTimeout
  , isRefCycle
  , isRefUnresolved
  , decodeSchema
  , decodeSchemaValue
  , findJsonFiles
  , testSuitePath
  ) where

import Control.DeepSeq qualified as DeepSeq
import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy qualified as LBS
import Hedgehog qualified as H
import JsonSchema qualified as Schema
import JsonSchema.Validate qualified as Validate
import System.Directory qualified as Directory
import System.FilePath qualified as FilePath
import System.Timeout qualified as Timeout
import Test.Tasty.HUnit qualified as HUnit

assertValidates :: String -> Schema.Schema -> Aeson.Value -> HUnit.Assertion
assertValidates label schema value =
  HUnit.assertBool (label <> "; unexpected validation errors: " <> show errs) (null errs)
  where
    errs = Validate.validateOn schema value

assertRejects :: String -> Schema.Schema -> Aeson.Value -> HUnit.Assertion
assertRejects label schema value =
  HUnit.assertBool (label <> "; expected at least one validation error") (not (null errs))
  where
    errs = Validate.validateOn schema value

validatesProp :: Schema.Schema -> Aeson.Value -> H.PropertyT IO Bool
validatesProp schema value = pure (Validate.isValidOn schema value)

validatePure :: Schema.Schema -> Aeson.Value -> [Validate.ValidationError]
validatePure = Validate.validateOn

withinTimeout :: (DeepSeq.NFData a) => a -> IO a
withinTimeout result = do
  forced <- Timeout.timeout 5000000 . Exception.evaluate . DeepSeq.force $ result
  maybe (HUnit.assertFailure "did not terminate within 5s") pure forced

isRefCycle :: Validate.ValidationErrorKind -> Bool
isRefCycle kind = case kind of
  Validate.RefCycle{} -> True
  _ -> False

isRefUnresolved :: Validate.ValidationErrorKind -> Bool
isRefUnresolved kind = case kind of
  Validate.RefUnresolved{} -> True
  _ -> False

decodeSchema :: LBS.ByteString -> Either String Schema.Schema
decodeSchema = Aeson.eitherDecode

decodeSchemaValue :: Aeson.Value -> Either String Schema.Schema
decodeSchemaValue = AesonTypes.parseEither Aeson.parseJSON

findJsonFiles :: FilePath -> IO [FilePath]
findJsonFiles dir = do
  exists <- Directory.doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- Directory.listDirectory dir
      foldMap
        ( \entry -> do
            let
              path = dir FilePath.</> entry
            isDir <- Directory.doesDirectoryExist path
            if isDir
              then findJsonFiles path
              else
                if FilePath.takeExtension path == ".json"
                  then pure [path]
                  else pure []
        )
        entries

testSuitePath :: FilePath -> FilePath
testSuitePath = (FilePath.</>) "../JSON-Schema-Test-Suite"
