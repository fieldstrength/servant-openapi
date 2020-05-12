{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLabels #-}

module OpenAPI.ToSchema.Internal where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Kind (Type)
import Data.Proxy
import Control.Lens hiding (enum)
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Data.Coerce (coerce)
import Servant.OpenAPI.Internal.Types
import GHC.Generics
import Data.Functor
import Prelude hiding (maximum, minimum, not,)
import GHC.TypeLits


-- | Types for which we can produce a 'SchemaObject' that accurately describes the
--   JSON serialization format.
class ToOpenAPISchema a where
  toSchema :: Proxy a -> SchemaObject

  default toSchema :: GToOpenAPI (Rep a) => Proxy a -> SchemaObject
  toSchema = genericToSchema defaultSchemaOptions

instance ToOpenAPISchema Text where toSchema Proxy = (blankSchema String) {title=Just "Text"}
instance ToOpenAPISchema Int where toSchema Proxy = (blankSchema Integer) {title=Just "Integer"}
instance ToOpenAPISchema Bool where toSchema Proxy = (blankSchema Boolean) {title=Just "Bool"}

instance ToOpenAPISchema a => ToOpenAPISchema [a] where
  toSchema Proxy =
    (blankSchema Array)
      { title = Just "Array"
      , items = Just . Concrete . toSchema $ Proxy @a
      }

instance ToOpenAPISchema a => ToOpenAPISchema (NonEmpty a) where
  toSchema Proxy =
    (toSchema $ Proxy @[a])
       {minItems = Just 1}

------------------------------- Configuration for deriving -------------------------------

data GenericSchemaOptions = GenericSchemaOptions
  { fieldNameModifier :: String -> String
  , constructorTagModifier :: String -> String
  , sumEncoding :: SumEncoding
  -- , unwrapUnaryRecords :: Bool  == false
  , allNullaryToStringTag :: Bool  -- ENUMS
  , tagSingleConstructors :: Bool
  , useDiscrimatorField :: Bool
  -- ^ Requires tracking schema refs, with required names
  }
  deriving stock Generic

data SumEncoding
  = Untagged
  -- | Don't need the other `content` string since we dont support non-record product types.
  | Tagged String
  deriving stock (Show, Eq)

defaultSchemaOptions :: GenericSchemaOptions
defaultSchemaOptions = GenericSchemaOptions
  { fieldNameModifier = id
  , constructorTagModifier = id
  , sumEncoding = Untagged
  , allNullaryToStringTag = True
  , tagSingleConstructors = False
  , useDiscrimatorField = False
  }


-------------------------- Generic class for sets of properties --------------------------

-- | Holds the true source field name, contained data schema, and if required.
--   Could make the selector field Maybe'd but that would support a tiny fraction of uses.
data FieldInfo = FieldInfo
  { selector :: String
  , schema :: SchemaObject
  , requirement :: Requirement
  } deriving stock (Generic, Show)

-- | Denotes when a property (field) is required or not.
--   In practice, this mostly is determined by if the type is a 'Maybe'.
data Requirement = Required | Optional
  deriving stock (Show, Eq, Generic)


-- | Class for information abount individual fields under a particular constructor.
--   Currently has instances only for record fields. Deriving for non-record product
--   types yields a custom compiler error.
class GFieldMap (f :: Type -> Type) where
  fieldMap :: Proxy f -> [FieldInfo]

instance GFieldMap U1 where fieldMap Proxy = []

instance (KnownSymbol sel, ToOpenAPISchema a)
  => GFieldMap (S1 ('MetaSel ('Just sel) x y z) (Rec0 a)) where
    fieldMap Proxy = pure @[]
      FieldInfo
        { selector = symbolVal $ Proxy @sel
        , schema = toSchema $ Proxy @a
        , requirement = Required
        }

-- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances
instance {-# OVERLAPPING #-} (KnownSymbol sel, ToOpenAPISchema a)
  => GFieldMap (S1 ('MetaSel ('Just sel) x y z) (Rec0 (Maybe a))) where
    fieldMap Proxy = pure @[]
      FieldInfo
        { selector = symbolVal $ Proxy @sel
        , schema = toSchema $ Proxy @a
        , requirement = Optional
        }

instance TypeError ('Text "Non-record product types are not supported for schema deriving")
  => GFieldMap (S1 ('MetaSel 'Nothing x y z) (Rec0 a)) where
    fieldMap = undefined

instance (GFieldMap l, GFieldMap r) => GFieldMap (l :*: r) where
  fieldMap Proxy =
    fieldMap (Proxy @l) <>
    fieldMap (Proxy @r)


-------------------------- Generic class for constructors/cases --------------------------

-- Information about a particular constructor: Name and contained fields.
data ConstructorInfo = ConstructorInfo
  { constructorName :: String
  , fields :: [FieldInfo]
  } deriving stock (Generic, Show)

-- | Gathers data on each datatype constructor
class GConstructorInfo (f :: Type -> Type) where
  constructorInfo :: Proxy f -> [ConstructorInfo]

instance (KnownSymbol cons, GFieldMap f)
  => GConstructorInfo (C1 ('MetaCons cons fx sl) f) where
    constructorInfo Proxy = pure @[]
      ConstructorInfo
        { constructorName = symbolVal $ Proxy @cons
        , fields = fieldMap $ Proxy @f
        }

instance (GConstructorInfo l, GConstructorInfo r) => GConstructorInfo (l :+: r) where
  constructorInfo Proxy =
    constructorInfo (Proxy @l) <>
    constructorInfo (Proxy @r)


---------------------------- Main Generic class for datatypes ----------------------------

-- | Overall shape of a datatype, including all constructor and selector names.
--   Carries a phantom type parameter indicating whether the names are those appearing
--   in the Haskell source, or transformed by 'transformNames' to the wire format
--   via functions supplied in 'GenericSchemaOptions'.
data DatatypeInfo (f :: FormatType) = DatatypeInfo
  { constructors :: [ConstructorInfo]
  , typeName :: String
  , modName :: String
  , pkgName :: String
  } deriving stock (Generic, Show)

-- | Used as a phantom data kind. See 'DatatypeInfo'.
data FormatType = Source | Wire

-- | Class that captures datatype structure, which is derived from 'Generic' and produces
--   a schema by way of the intermediate data type 'DatatypeInfo'.
class GToOpenAPI (f :: Type -> Type) where
  gToOpenAPI :: Proxy f -> DatatypeInfo 'Source

instance (GConstructorInfo f, KnownSymbol name, KnownSymbol mod, KnownSymbol pkg)
  => GToOpenAPI (D1 ('MetaData name mod pkg nt) f) where
    gToOpenAPI Proxy = DatatypeInfo
      { constructors = constructorInfo $ Proxy @f
      , typeName = symbolVal $ Proxy @name
      , modName = symbolVal $ Proxy @mod
      , pkgName = symbolVal $ Proxy @pkg
      }

genericToSchema :: forall a. (GToOpenAPI (Rep a)) => GenericSchemaOptions -> Proxy a -> SchemaObject
genericToSchema opts Proxy = mkSchema opts . gToOpenAPI $ Proxy @(Rep a)

---------------------------- Main Generic class for datatypes ----------------------------

mkSchema :: GenericSchemaOptions -> DatatypeInfo 'Source -> SchemaObject
mkSchema opts d
  | isEnum d && allNullaryToStringTag opts = enumSchema (transformNames opts d)
  | otherwise, [c] <- constructors d = singleConstructorSchema (typeName d) c
  | otherwise = case sumEncoding opts of
      Tagged tag -> taggedRecordSum opts tag $ transformNames opts d
      Untagged -> untaggedRecordSum $ transformNames opts d

isEnum :: DatatypeInfo p -> Bool
isEnum DatatypeInfo{constructors} = all nullary constructors

nullary :: ConstructorInfo -> Bool
nullary = (==0) . length . fields

-- | How to encode enums (types with all nullary constructors).
--   Used when:
--
--     * All 'nullary' constructors
--
--     * @allNullaryToStringTag = True@ in 'GenericSchemaOptions'
enumSchema :: DatatypeInfo 'Wire -> SchemaObject
enumSchema DatatypeInfo{typeName, constructors} =
  (blankSchema String)
    { title = Just $ T.pack typeName
    , enum = Just $ T.pack . constructorName <$> constructors
    }

-- | How to encode once we decide to use this "Tagged Record" approach.
--   This is used when all the following conditions are met:
--
--     * @sumEncoding = Tagged str@
--
--     * Conditions for 'enumSchema' not met.
--
--     * NOT exactly one constructor.
--
--     * (Trivially true, for now:) only record/selector constructor fields
taggedRecordSum :: GenericSchemaOptions -> String -> DatatypeInfo 'Wire -> SchemaObject
taggedRecordSum opts tag DatatypeInfo{typeName, constructors} =
  blankObjectSchema
    { title = Just $ T.pack typeName
    , discriminator =
        if useDiscrimatorField opts
          then Just Discriminator
            { propertyName = T.pack tag
            , mapping = Nothing  -- TODO
            }
          else Nothing
    , required = Just [T.pack tag]
    , properties = Just . Properties $ Map.singleton (T.pack tag) (Concrete $ blankSchema String)
    , oneOf = Just $ Concrete . constructorSchema <$> constructors
    }

-- | How to encode once we decide to use this "Untagged Record" approach.
--   This is used when:
--
--     * @sumEncoding = Untagged@
--
--     * Conditions for 'enumSchema' not met.
--
--     * (trivially true:) only record/selector constructor fields
untaggedRecordSum :: DatatypeInfo 'Wire -> SchemaObject
untaggedRecordSum DatatypeInfo{typeName, constructors} =
  blankObjectSchema
    { title = Just $ T.pack typeName
    , oneOf = Just $ Concrete . constructorSchema <$> constructors
    }

-- | Populate main fields of a constructor into an object schema.
constructorSchema :: ConstructorInfo -> SchemaObject
constructorSchema ConstructorInfo{fields} =
  blankObjectSchema
    { properties
        = Just
        . Properties
        . Map.fromList $ fields <&> \FieldInfo{selector,schema} ->
          (T.pack selector, Concrete schema)
    , required = Just $
        T.pack . selector <$>
          filter ((==Required) . requirement) fields
    }

-- | For when the schema of a data type is just that of the one constructor.
singleConstructorSchema :: String -> ConstructorInfo -> SchemaObject
singleConstructorSchema name = set #title (Just $ T.pack name) . constructorSchema

-- Can define this either with coerce, or a proper type-changing lens in place of #constructos.
-- | Transform constructor and field names according to the functions defined in
--   the 'GenericSchemaOptions'.
transformNames :: GenericSchemaOptions -> DatatypeInfo 'Source -> DatatypeInfo 'Wire
transformNames opts
  = over (#constructors . mapped . #constructorName)             (constructorTagModifier opts)
  . over (#constructors . mapped . #fields . mapped . #selector) (fieldNameModifier opts)
  . switchPhantom

  where
    switchPhantom :: DatatypeInfo 'Source -> DatatypeInfo 'Wire
    switchPhantom = coerce


blankSchema :: SchemaType -> SchemaObject
blankSchema ty = SchemaObject
  { title = Nothing
  , type_ = ty
  , discriminator = Nothing
  , multipleOf = Nothing
  , maximum = Nothing
  , exclusiveMaximum = Nothing
  , minimum = Nothing
  , exclusiveMinimum = Nothing
  , maxLength = Nothing
  , minLength = Nothing
  , pattern = Nothing
  , maxItems = Nothing
  , minItems = Nothing
  , uniqueItems = Nothing
  , maxProperties = Nothing
  , minProperties = Nothing
  , required = Nothing
  , enum = Nothing
  , allOf = Nothing
  , oneOf = Nothing
  , anyOf = Nothing
  , not = Nothing
  , items = Nothing
  , properties = Nothing
  , additionalProperties = Nothing
  , description = Nothing
  , format = Nothing
  , default_ = Nothing
  }

blankObjectSchema :: SchemaObject
blankObjectSchema = blankSchema Object
