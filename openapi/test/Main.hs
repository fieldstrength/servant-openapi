{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main where

import           Control.Monad       (void)
import           Data.Aeson          as Aeson
import           Data.Aeson.Deriving as AD
import           Data.Proxy
import qualified Data.Map            as Map (empty)
import           Data.Yaml           as Yaml
import           GHC.Generics
import           Hedgehog
import           Hedgehog.Main       (defaultMain)
import           OpenAPI
import           Text.RawString.QQ   (r)

main :: IO ()
main = do
    defaultMain [checkParallel $$(discover)]

once :: Property -> Property
once = withTests 1

prop_decode_petstore :: Property
prop_decode_petstore = once . property . evalIO $
  void $ decodeFileThrow @_ @OpenAPI "examples/petstore.yaml"

prop_decode_petstore_extended :: Property
prop_decode_petstore_extended = once . property . evalIO $
  void $ decodeFileThrow @_ @OpenAPI "examples/petstore-expanded.yaml"

prop_roundtrip_petstore :: Property
prop_roundtrip_petstore = once . property $ do
  yaml <- evalIO $ decodeFileThrow @_ @Value "examples/petstore.yaml"
  openapi <- either error pure . parseEither (parseJSON @OpenAPI) $ yaml

  toJSON openapi === yaml

prop_roundtrip_petstore_extended :: Property
prop_roundtrip_petstore_extended = once . property $ do
  yaml <- evalIO $ decodeFileThrow @_ @Value "examples/petstore-expanded.yaml"
  openapi <- either error pure . parseEither (parseJSON @OpenAPI) $ yaml

  toJSON openapi === yaml

prop_encode_paths_object_as_json_object :: Property
prop_encode_paths_object_as_json_object = once . property $ do
  toJSON (Map.empty :: PathsObject) === Aeson.object []


data Foo = Foo {fooNumber :: Int, fooBoolean :: Maybe Bool}
  deriving stock (Generic, Show, Eq)
  deriving (ToOpenAPISchema, ToJSON) via
    GenericEncoded
     '[ FieldLabelModifier := SnakeCase
      , AD.SumEncoding := TaggedObject "TAG" "CONTENTS"
      , OmitNothingFields := 'True
      , TagSingleConstructors := 'True
      ]
      Foo

prop_foo_schema :: Property
prop_foo_schema = once . property $
  (Right . toSchema) (Proxy @Foo) === Yaml.decodeEither [r|
title: Foo
type: object
properties:
  TAG:
    type: string
    enum:
      - Foo
  foo_number:
    type: integer
  foo_boolean:
    type: boolean
required:
  - TAG
  - foo_number
|]


type UntaggedOptions =
  '[FieldLabelModifier := SnakeCase
  , AD.SumEncoding := UntaggedValue
  , OmitNothingFields := 'True
  , TagSingleConstructors := 'True
  ]
type UntaggedEncoded = GenericEncoded UntaggedOptions

type TaggedOptions =
  '[FieldLabelModifier := SnakeCase
  , AD.SumEncoding := TaggedObject "TAG" "CONTENTS"
  , OmitNothingFields := 'True
  , TagSingleConstructors := 'True
  ]
type TaggedEncoded = GenericEncoded TaggedOptions



data FooBar
  = MkFoo Foo
  | MkBar Bar
  deriving stock (Generic, Show, Eq)
  deriving (ToOpenAPISchema, ToJSON) via UntaggedEncoded FooBar

data Bar = Bar {barNumber :: Int}
  deriving stock (Generic, Show, Eq)
  deriving (ToOpenAPISchema, ToJSON) via TaggedEncoded Bar

data U2 = U2 Int Bool
  deriving stock (Generic, Show, Eq)
  deriving (ToOpenAPISchema, ToJSON) via TaggedEncoded U2

data AB
  = A Foo
  | B Int
  deriving stock (Generic, Show, Eq)
  deriving (ToOpenAPISchema, ToJSON) via TaggedEncoded AB

data CD
  = C Foo
  | D Bar Int
  deriving stock (Generic, Show, Eq)
  deriving (ToOpenAPISchema, ToJSON) via UntaggedEncoded CD
