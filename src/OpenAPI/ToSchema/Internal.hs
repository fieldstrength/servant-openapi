{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module OpenAPI.ToSchema.Internal where

import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Kind (Type)
import Data.Proxy
import Servant.OpenAPI.Internal.Types
import GHC.Generics
import Prelude hiding (maximum, minimum, not,)


class ToOpenAPISchema a where
  toSchema :: Proxy a -> SchemaObject

  default toSchema :: GToOpenAPISchema (Rep a) => Proxy a -> SchemaObject
  toSchema Proxy = gToSchema defaultSchemaOptions $ Proxy @(Rep a)


defaultSchemaOptions :: GenericSchemaOptions
defaultSchemaOptions = GenericSchemaOptions
  { fieldNameModifier = id
  , sumEncoding = UntaggedValue
  }

instance ToOpenAPISchema Text where toSchema Proxy = (blankSchema String) {title=Just "Text"}
instance ToOpenAPISchema Int where toSchema Proxy = (blankSchema Integer) {title=Just "Int"}


------------------------------- Generic class for Maps of properties -------------------------------

data Requirement = Required | Optional
  deriving stock (Show, Eq, Generic)

class GPropertyMap (f :: Type -> Type) where
  gToPropertyMap :: GenericSchemaOptions -> Proxy f -> Map Text (SchemaObject, Requirement)

instance (Selector sel, ToOpenAPISchema a) => GPropertyMap (S1 sel (Rec0 (Maybe a))) where
  gToPropertyMap GenericSchemaOptions{fieldNameModifier} Proxy =
    Map.fromList
      [ ( T.pack . fieldNameModifier $ selName @sel undefined
        , (toSchema $ Proxy @a, Optional)
        )
      ]
instance (Selector sel, ToOpenAPISchema a) => GPropertyMap (S1 sel (Rec0 a)) where
  gToPropertyMap GenericSchemaOptions{fieldNameModifier} Proxy =
    Map.fromList
      [ ( T.pack . fieldNameModifier $ selName @sel undefined
        , (toSchema $ Proxy @a, Required)
        )
      ]

instance (GPropertyMap l, GPropertyMap r) => GPropertyMap (l :*: r) where
  gToPropertyMap opts Proxy =
       gToPropertyMap opts (Proxy @l)
    <> gToPropertyMap opts (Proxy @r)


------------------------------- Generic class for constructors/cases -------------------------------

class GSchemaUnion (f :: Type -> Type) where
  gSchemaCases :: GenericSchemaOptions -> Proxy f -> [SchemaObject]

instance (Constructor cons, GPropertyMap f) => GSchemaUnion (C1 cons f) where
  gSchemaCases opts Proxy = pure
    blankObjectSchema
      { properties = Just . Properties . fmap (ReferenceOr . fst) . gToPropertyMap opts $ Proxy @f
      , required =  Just . Map.keys . Map.filter ((==Required) . snd) . gToPropertyMap opts $ Proxy @f
      }

instance (GSchemaUnion l, GSchemaUnion r) => GSchemaUnion (l :+: r) where
  gSchemaCases opts Proxy =
    gSchemaCases opts (Proxy @l) <>
    gSchemaCases opts (Proxy @r)


---------------------------------------- Main generic class ----------------------------------------

class GToOpenAPISchema (f :: Type -> Type) where
  gToSchema :: GenericSchemaOptions -> Proxy f -> SchemaObject

instance (Datatype d, GSchemaUnion f) => GToOpenAPISchema (D1 d f) where
  gToSchema opts Proxy = case foo of
    Left xs ->
      blankObjectSchema
        { title = Just . T.pack $ datatypeName @d undefined
        , oneOf = Just $ ReferenceOr <$> xs
        }
    Right x -> x
      { title = Just . T.pack $ datatypeName @d undefined
      }
    where
      foo = case gSchemaCases opts $ Proxy @f of
              [] -> Left [] -- FIXME
              [x] -> Right x
              xs -> Left xs


data GenericSchemaOptions = GenericSchemaOptions
  { fieldNameModifier :: String -> String
  , sumEncoding :: SumEncoding
  }
  deriving stock Generic

data SumEncoding
  = UntaggedValue
  deriving stock (Show, Eq)


instance ToOpenAPISchema a => GToOpenAPISchema (Rec0 a) where
  gToSchema _opts Proxy = toSchema (Proxy @a)


data Dog = Dog
  { dogName :: Text
  , dogAge :: Int
  } deriving stock (Generic)

data User = Anonymous Text | LoggedInUser Int Text
  deriving stock (Generic)

instance ToOpenAPISchema Dog
instance ToOpenAPISchema User


blankSchema :: SchemaType -> SchemaObject
blankSchema ty = SchemaObject
  { title = Nothing
  , type_ = ty
  , multipleOf = Nothing -- Maybe Int
  , maximum = Nothing -- Maybe Int
  , exclusiveMaximum = Nothing -- Maybe Int
  , minimum = Nothing -- Maybe Int
  , exclusiveMinimum = Nothing -- Maybe Int
  , maxLength = Nothing -- Maybe Int
  , minLength = Nothing -- Maybe Int
  , pattern = Nothing -- Maybe Text
  , maxItems = Nothing -- Maybe Int
  , minItems = Nothing -- Maybe Int
  , uniqueItems = Nothing -- Maybe Bool
  , maxProperties = Nothing -- Maybe Int
  , minProperties = Nothing -- Maybe Int
  , required = Nothing -- Maybe [Text]
  , enum = Nothing -- Maybe [Text]
  , allOf = Nothing -- Maybe [ReferenceOr SchemaObject]
  , oneOf = Nothing -- Maybe [ReferenceOr SchemaObject]
  , anyOf = Nothing -- Maybe [ReferenceOr SchemaObject]
  , not = Nothing -- Maybe [ReferenceOr SchemaObject]
  , items = Nothing -- Maybe ReferenceOr SchemaObject
  , properties = Nothing -- Maybe Properties
  , additionalProperties = Nothing -- Maybe (Either Bool ReferenceOr SchemaObject)
  , description = Nothing -- Maybe Text
  , format = Nothing -- Maybe Text
  , default_ = Nothing -- Maybe Aeson.Value
  }

blankObjectSchema :: SchemaObject
blankObjectSchema = blankSchema Object
