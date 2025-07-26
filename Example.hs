{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Autodocodec
import Autodocodec.Schema (jsonSchemaViaCodec)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as L8
import GHC.Generics (Generic)

data Person = Person
  { personName :: String
  , personAge :: Int
  , personIsActive :: Bool
  } deriving (Show, Eq, Generic)

instance HasCodec Person where
  codec = object "Person" $
    Person
      <$> requiredField "name" "The person's name" .= personName
      <*> requiredField "age" "The person's age in years" .= personAge
      <*> requiredField "isActive" "Whether the person is currently active" .= personIsActive

instance ToJSON Person where
  toJSON = toJSONViaCodec

instance FromJSON Person where
  parseJSON = parseJSONViaCodec

main :: IO ()
main = do
  putStrLn "Generated JSON Schema:"
  putStrLn ""
  L8.putStrLn $ encodePretty $ jsonSchemaViaCodec @Person
  putStrLn ""
  putStrLn "Example JSON data:"
  putStrLn ""
  let examplePerson = Person "Alice" 30 True
  L8.putStrLn $ encodePretty examplePerson