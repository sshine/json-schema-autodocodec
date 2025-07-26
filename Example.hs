{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Autodocodec
import Autodocodec.Schema (jsonSchemaViaCodec, validateAccordingTo)
import Data.Aeson (ToJSON(..), FromJSON(..), Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as L8
import GHC.Generics (Generic)

data Person = Person
  { personName :: String
  , personAge :: Int
  , personIsActive :: Bool
  } deriving stock (Show, Eq, Generic)

-- Age bounds: 0 to 200 years
ageBounds :: Bounds Integer
ageBounds = Bounds (Just 0) (Just 200)

-- Non-empty string validation
nonEmptyStringCodec :: JSONCodec String
nonEmptyStringCodec = bimapCodec
  (\s -> if null s then Left "String cannot be empty" else Right s)
  id
  stringCodec

instance HasCodec Person where
  codec = object "Person" $
    Person
      <$> requiredFieldWith "name" nonEmptyStringCodec "The person's name (non-empty)" .= personName
      <*> requiredFieldWith "age" (dimapCodec fromIntegral fromIntegral (integerWithBoundsCodec ageBounds)) "The person's age (0-200 years)" .= personAge
      <*> requiredField "isActive" "Whether the person is currently active" .= personIsActive

instance ToJSON Person where
  toJSON = toJSONViaCodec

instance FromJSON Person where
  parseJSON = parseJSONViaCodec

main :: IO ()
main = do
  let schema = jsonSchemaViaCodec @Person
  let examplePerson = Person "Alice" 30 True
  let validJson = toJSON examplePerson
  let invalidJson = Aeson.object ["name" Aeson..= ("Bob" :: String), "age" Aeson..= ("not-a-number" :: String)]
  let boundaryValidJson = Aeson.object ["name" Aeson..= ("Charlie" :: String), "age" Aeson..= (200 :: Int), "isActive" Aeson..= True]
  let boundaryInvalidJson = Aeson.object ["name" Aeson..= ("Dave" :: String), "age" Aeson..= (201 :: Int), "isActive" Aeson..= False]
  let emptyNameJson = Aeson.object ["name" Aeson..= ("" :: String), "age" Aeson..= (25 :: Int), "isActive" Aeson..= True]

  putStrLn "Generated JSON Schema:"
  putStrLn ""
  L8.putStrLn $ encodePretty schema
  putStrLn ""

  putStrLn "Example JSON data:"
  putStrLn ""
  L8.putStrLn $ encodePretty examplePerson
  putStrLn ""

  putStrLn "Schema Validation Tests:"
  putStrLn ""

  putStrLn "✓ Valid JSON validation:"
  if validateAccordingTo validJson schema
    then putStrLn "  PASSED: Valid JSON accepted"
    else putStrLn "  ERROR: Valid JSON was rejected"
  putStrLn ""

  putStrLn "✗ Invalid JSON validation:"
  if validateAccordingTo invalidJson schema
    then putStrLn "  ERROR: Invalid JSON was incorrectly accepted"
    else putStrLn "  PASSED: Invalid JSON rejected"
  putStrLn ""

  putStrLn "🔢 Boundary validation tests:"
  putStrLn "✓ Valid boundary (age 200):"
  if validateAccordingTo boundaryValidJson schema
    then putStrLn "  PASSED: Age 200 accepted"
    else putStrLn "  ERROR: Age 200 was rejected"

  putStrLn "✗ Invalid boundary (age 201):"
  if validateAccordingTo boundaryInvalidJson schema
    then putStrLn "  ERROR: Age 201 was incorrectly accepted"
    else putStrLn "  PASSED: Age 201 rejected"

  putStrLn "✗ Empty name schema validation:"
  if validateAccordingTo emptyNameJson schema
    then putStrLn "  NOTE: Empty name passes schema validation (custom validation is in parsing)"
    else putStrLn "  PASSED: Empty name rejected"
  putStrLn ""

  putStrLn "📋 Alternative validation with error messages:"
  putStrLn "  Using parseJSONViaCodec for detailed errors:"
  case Aeson.fromJSON validJson of
    Aeson.Success (_ :: Person) -> putStrLn "  ✓ Valid JSON parsed successfully"
    Aeson.Error err -> putStrLn $ "  ✗ Valid JSON failed to parse: " ++ err
  case Aeson.fromJSON invalidJson of
    Aeson.Success (_ :: Person) -> putStrLn "  ✗ Invalid JSON parsed incorrectly"
    Aeson.Error err -> putStrLn $ "  ✓ Invalid JSON rejected: " ++ err
  case Aeson.fromJSON boundaryInvalidJson of
    Aeson.Success (_ :: Person) -> putStrLn "  ✗ Age 201 parsed incorrectly"
    Aeson.Error err -> putStrLn $ "  ✓ Age 201 rejected: " ++ err
  case Aeson.fromJSON emptyNameJson of
    Aeson.Success (_ :: Person) -> putStrLn "  ✗ Empty name parsed incorrectly"
    Aeson.Error err -> putStrLn $ "  ✓ Empty name rejected: " ++ err
