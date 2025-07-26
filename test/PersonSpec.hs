{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PersonSpec (spec) where

import Test.Hspec
import Data.Aeson (Value, toJSON, ToJSON(..), FromJSON(..))
import Data.Aeson qualified as Aeson
import Autodocodec
import Autodocodec.Schema (jsonSchemaViaCodec, validateAccordingTo)
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

spec :: Spec
spec = describe "Person JSON Schema Validation" $ do
  let schema = jsonSchemaViaCodec @Person

  describe "Valid JSON validation" $ do
    it "accepts valid Person JSON with all required fields" $ do
      let validJson = Aeson.object
            [ "name" Aeson..= ("Alice" :: String)
            , "age" Aeson..= (30 :: Int)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo validJson schema `shouldBe` True

    it "accepts Person JSON created via toJSON" $ do
      let person = Person "Bob" 25 False
      let personJson = toJSON person
      validateAccordingTo personJson schema `shouldBe` True

    it "accepts edge case values" $ do
      let validJson = Aeson.object
            [ "name" Aeson..= ("" :: String)  -- empty string
            , "age" Aeson..= (0 :: Int)       -- zero age
            , "isActive" Aeson..= False
            ]
      validateAccordingTo validJson schema `shouldBe` True

  describe "Invalid JSON validation" $ do
    it "rejects JSON with wrong type for age field" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= ("Charlie" :: String)
            , "age" Aeson..= ("not-a-number" :: String)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo invalidJson schema `shouldBe` False

    it "rejects JSON with missing required field" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= ("Dave" :: String)
            , "isActive" Aeson..= True
            -- missing "age" field
            ]
      validateAccordingTo invalidJson schema `shouldBe` False

    it "rejects JSON with wrong type for name field" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= (123 :: Int)  -- number instead of string
            , "age" Aeson..= (40 :: Int)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo invalidJson schema `shouldBe` False

    it "rejects JSON with wrong type for isActive field" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= ("Eve" :: String)
            , "age" Aeson..= (35 :: Int)
            , "isActive" Aeson..= ("yes" :: String)  -- string instead of boolean
            ]
      validateAccordingTo invalidJson schema `shouldBe` False

  describe "Age boundary validation" $ do
    it "accepts minimum age boundary (0)" $ do
      let validJson = Aeson.object
            [ "name" Aeson..= ("Baby" :: String)
            , "age" Aeson..= (0 :: Int)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo validJson schema `shouldBe` True

    it "accepts maximum age boundary (200)" $ do
      let validJson = Aeson.object
            [ "name" Aeson..= ("Elder" :: String)
            , "age" Aeson..= (200 :: Int)
            , "isActive" Aeson..= False
            ]
      validateAccordingTo validJson schema `shouldBe` True

    it "rejects age below minimum (-1)" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= ("Invalid" :: String)
            , "age" Aeson..= (-1 :: Int)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo invalidJson schema `shouldBe` False

    it "rejects age above maximum (201)" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= ("TooOld" :: String)
            , "age" Aeson..= (201 :: Int)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo invalidJson schema `shouldBe` False

  describe "Name length validation" $ do
    it "accepts single character name" $ do
      let validJson = Aeson.object
            [ "name" Aeson..= ("A" :: String)
            , "age" Aeson..= (25 :: Int)
            , "isActive" Aeson..= True
            ]
      validateAccordingTo validJson schema `shouldBe` True

    it "rejects empty name during parsing" $ do
      let invalidJson = Aeson.object
            [ "name" Aeson..= ("" :: String)
            , "age" Aeson..= (25 :: Int)
            , "isActive" Aeson..= True
            ]
      case Aeson.fromJSON invalidJson of
        Aeson.Success (_ :: Person) -> expectationFailure "Empty name should not parse successfully"
        Aeson.Error _ -> return ()  -- Expected failure

