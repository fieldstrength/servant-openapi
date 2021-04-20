{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenAPI.Internal.Class where

import           Control.Lens           hiding (enum)
import qualified Data.Aeson             as Aeson
import           Data.Aeson.Deriving    hiding (SumEncoding)
import           Data.Coerce            (coerce)
import           Data.Function
import           Data.Functor
import           Data.Traversable       (for)
import           Data.Int
import           Data.Kind              (Type)
import           Data.Maybe             (isJust)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map.Strict        as Map
import qualified Data.HashMap.Strict    as HashMap
import           Data.Proxy
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LazyText
import           Data.Time              (Day, UTCTime)
import           Data.UUID              (UUID)
import           GHC.Generics
import           GHC.TypeLits
import           OpenAPI.Internal.Types
import           Prelude                hiding (maximum, minimum, not)
import           Servant.API            (WithStatus)

-- | Types for which we can produce a 'SchemaObject' that accurately describes the
--   JSON serialization format.
class ToOpenAPISchema a where
  toSchema :: Proxy a -> SchemaObject

  default toSchema :: GToOpenAPI (Rep a) => Proxy a -> SchemaObject
  toSchema = genericToSchema defaultSchemaOptions


instance ToOpenAPISchema Text where toSchema Proxy = (blankSchema String)
instance ToOpenAPISchema LazyText.Text where toSchema Proxy = (blankSchema String)
instance {-# OVERLAPPING #-} ToOpenAPISchema String where toSchema Proxy = (blankSchema String)

instance ToOpenAPISchema Int where toSchema Proxy = (blankSchema Integer)
instance ToOpenAPISchema Integer where toSchema Proxy = (blankSchema Integer)
instance ToOpenAPISchema Float where toSchema Proxy = (blankSchema Number) {format=Just "float"}
instance ToOpenAPISchema Double where toSchema Proxy = (blankSchema Number) {format=Just "double"}
instance ToOpenAPISchema Int32 where toSchema Proxy = (blankSchema Integer) {format = Just "int32"}
instance ToOpenAPISchema Int64 where toSchema Proxy = (blankSchema Integer) {format = Just "int64"}

instance ToOpenAPISchema Bool where toSchema Proxy = (blankSchema Boolean)

instance ToOpenAPISchema a => ToOpenAPISchema [a] where
  toSchema Proxy =
    (blankSchema Array)
      {items = Just . Concrete . toSchema $ Proxy @a}

instance ToOpenAPISchema a => ToOpenAPISchema (NonEmpty a) where
  toSchema Proxy =
    (toSchema $ Proxy @[a])
       {minItems = Just 1}

instance ToOpenAPISchema a => ToOpenAPISchema (WithStatus s a) where
  toSchema Proxy = toSchema (Proxy @a)

------ Orphan instances ------

instance ToOpenAPISchema UTCTime where toSchema Proxy = (blankSchema String) {format = Just "date-time"}
instance ToOpenAPISchema Day where toSchema Proxy = (blankSchema String) {format = Just "date"}
instance ToOpenAPISchema UUID where toSchema Proxy =  (blankSchema String) {format = Just "uuid"}


------------------------------- Configuration for deriving -------------------------------

-- | Configuration for generic deriving of 'ToOpenAPISchema'.
--
--   It is recommended not to use this type directly. Instead consider deriving the
--   instance together with 'FromJSON'/'ToJSON' instances using @DerivingVia@ with types
--   from @aeson-deriving@. This ensures schemas are consistent with (de)serialization.
data GenericSchemaOptions = GenericSchemaOptions
  { fieldNameModifier :: String -> String
  , constructorTagModifier :: String -> String
  , sumEncoding :: SumEncoding
  -- , unwrapUnaryRecords :: Bool  == false
  , allNullaryToStringTag :: Bool
  , tagSingleConstructors :: Bool
  , useDiscrimatorField :: Bool
  -- ^ Requires tracking schema refs, with required names
  }
  deriving stock Generic

data SumEncoding
  = Untagged
  -- | Don't need the other `content` string since we dont support non-record product types.
  | Tagged String String
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
  { selector :: Maybe String
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
        { selector = Just . symbolVal $ Proxy @sel
        , schema = toSchema $ Proxy @a
        , requirement = Required
        }

-- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances
instance {-# OVERLAPPING #-} (KnownSymbol sel, ToOpenAPISchema a)
  => GFieldMap (S1 ('MetaSel ('Just sel) x y z) (Rec0 (Maybe a))) where
    fieldMap Proxy = pure @[]
      FieldInfo
        { selector = Just . symbolVal $ Proxy @sel
        , schema = toSchema $ Proxy @a
        , requirement = Optional
        }

instance ToOpenAPISchema a => GFieldMap (S1 ('MetaSel 'Nothing x y z) (Rec0 a)) where
    fieldMap Proxy = pure @[]
      FieldInfo
        { selector = Nothing
        , schema = toSchema $ Proxy @a
        , requirement = Required
        }

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

-- | Create the 'SchemaObject' for a type using the generic type classes.
--   NOTE: Consider avoiding calling this function directly. Its output is only correct
--   if the right 'GenericSchemaOptions' is supplied, aligning with aeson's 'Options'
---  or equivalent. Instead consider deriving via types from @aeson-deriving@, together
--   with aeson classes, to ensure schemas are consistent with (de)serialization.
genericToSchema :: forall a. (GToOpenAPI (Rep a)) => GenericSchemaOptions -> Proxy a -> SchemaObject
genericToSchema opts Proxy = mkSchema opts . gToOpenAPI $ Proxy @(Rep a)


--------------------------- Function to create the SchemaObject --------------------------

-- | The main function that creates the SchemaObject, using the 'DatatypeInfo' generated
--   by the generic type classes.
mkSchema :: GenericSchemaOptions -> DatatypeInfo 'Source -> SchemaObject
mkSchema opts (transformNames opts -> dtInfo)
  -- view pattern here ^ prevents acces to the wrong version of the DatatypeInfo
  | isVoid dtInfo = voidSchema
  | isEnum dtInfo && allNullaryToStringTag opts = enumSchema dtInfo
  | otherwise, [c] <- constructors dtInfo =
      singleConstructorSchema (typeName dtInfo) c
        & if tagSingleConstructors opts
            then setSingleConstructorTag (T.pack $ constructorName c)
            else id
  | otherwise = case sumEncoding opts of
      Tagged tag contents -> taggedSum opts tag contents dtInfo
      Untagged -> untaggedSum dtInfo

  where
    setSingleConstructorTag :: Text -> SchemaObject -> SchemaObject
    setSingleConstructorTag tagVal s = case sumEncoding opts of
      Untagged -> s
      Tagged (T.pack -> tag) _contents -> s
        & #properties %~ addToProperties tag (simpleEnumSchema [tagVal])
        & #required %~ maybe (Just [tag]) (Just . (tag :))

        -- QUESTION: should properties be populated at all when using `oneOf`?


addToProperties :: Text -> SchemaObject -> Maybe Properties -> Maybe Properties
addToProperties name schema = \case
  Nothing -> Just . Properties $ Map.singleton name (Concrete schema)
  Just (Properties ps) -> Just . Properties $ Map.insert name (Concrete schema) ps

isVoid :: DatatypeInfo p -> Bool
isVoid DatatypeInfo{constructors} = length constructors == 0

isEnum :: DatatypeInfo p -> Bool
isEnum DatatypeInfo{constructors} =
  all nullary constructors && length constructors > 0

isRecord :: ConstructorInfo -> Bool
isRecord ConstructorInfo{fields} = all (isJust . selector) fields

nullary :: ConstructorInfo -> Bool
nullary = (==0) . length . fields

voidSchema :: SchemaObject
voidSchema = blank {oneOf = Just []}

-- | A nameless schema that only declares type `String` with an enum options list
simpleEnumSchema :: [Text] -> SchemaObject
simpleEnumSchema vals =
  (blankSchema String)
    {enum = Just vals}

-- | How to encode enums (types with all nullary constructors).
--   Used when:
--
--     * All 'nullary' constructors
--
--     * @allNullaryToStringTag = True@ in 'GenericSchemaOptions'
enumSchema :: DatatypeInfo 'Wire -> SchemaObject
enumSchema dtInfo =
  (tagSchema dtInfo)
    { title = Just $ T.pack (view #typeName dtInfo)
    }

-- | Schema for tag values
tagSchema :: DatatypeInfo 'Wire -> SchemaObject
tagSchema DatatypeInfo{constructors} =
  (blankSchema String)
    { enum = Just $ T.pack . constructorName <$> constructors
    }

-- | How to encode once we decide to use this "Tagged Record" approach.
--   This is used when all the following conditions are met:
--
--     * @sumEncoding = Tagged str@
--
--     * Conditions for 'enumSchema' not met.
--
--     * Number of constructors > 1.
taggedSum :: GenericSchemaOptions -> String -> String -> DatatypeInfo 'Wire -> SchemaObject
taggedSum opts tag _contents dtInfo@DatatypeInfo{typeName, constructors} =
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
    , properties = Just . Properties $
      Map.singleton
        (T.pack tag)
        (Concrete $ tagSchema dtInfo)
    -- QUESTION: should properties be populated at all when using `oneOf`?
    , oneOf = Just $ Concrete . constructorSchema <$> constructors
    }

constructorSchema :: ConstructorInfo -> SchemaObject
constructorSchema con
  | isRecord con = recordConstructorSchema con
  | otherwise    = nonrecordConstructorSchema con

-- | How to encode once we decide to use this "Untagged Record" approach.
--   This is used when:
--
--     * @sumEncoding = Untagged@
--
--     * Conditions for 'enumSchema' not met.
--
--     * Number of constructors > 1.
untaggedSum :: DatatypeInfo 'Wire -> SchemaObject
untaggedSum DatatypeInfo{typeName, constructors} =
  blankObjectSchema
    { title = Just $ T.pack typeName
    , oneOf = Just $ Concrete . constructorSchema <$> constructors
    }

-- | Populate main fields of a record constructor into an object schema
recordConstructorSchema :: ConstructorInfo -> SchemaObject
recordConstructorSchema ConstructorInfo{fields} =
  blankObjectSchema
    { properties =
        fmap (Properties . Map.fromList) . for fields $ \FieldInfo{selector,schema} -> do
            sel <- selector
            Just $ (T.pack sel, Concrete schema)
    , required =
        fmap (map fst . filter ((== Required) . snd)) . for fields $ \FieldInfo{selector,requirement} -> do
          sel <- selector
          Just $ (T.pack sel, requirement)
    }

-- | Encoding of a non-record constructor. This is what appears under "contents" when tagging is used.
--   Only in the case of a single field can we give something meaningful.
nonrecordConstructorSchema :: ConstructorInfo -> SchemaObject
nonrecordConstructorSchema ConstructorInfo{fields} = case fields of
  [] -> blankObjectSchema
  [FieldInfo{schema}] -> schema
  _ -> arraySchema blank



-- | For when the schema of a data type is just that of the one constructor.
singleConstructorSchema :: String -> ConstructorInfo -> SchemaObject
singleConstructorSchema name cInfo
  | isRecord cInfo = set #title (Just $ T.pack name) $ recordConstructorSchema cInfo
  | otherwise = set #title (Just $ T.pack name) $ nonrecordConstructorSchema cInfo

-- | Transform constructor and field names from Haskell source to wire format values
--   according to the functions defined in 'GenericSchemaOptions'.
transformNames :: GenericSchemaOptions -> DatatypeInfo 'Source -> DatatypeInfo 'Wire
transformNames opts
  = over (#constructors . mapped . #constructorName)                     (constructorTagModifier opts)
  . over (#constructors . mapped . #fields . mapped . #selector . _Just) (fieldNameModifier opts)
  . switchPhantom
  -- We use a coercion here, but a proper type-changing lens in place of #constructors
  -- would also work

  where
    switchPhantom :: DatatypeInfo 'Source -> DatatypeInfo 'Wire
    switchPhantom = coerce


--------------------------------- aeson-deriving support ---------------------------------

instance (GToOpenAPI (Rep a), ToAesonOptions opts) => ToOpenAPISchema (GenericEncoded opts a) where
  toSchema Proxy = case fromAesonOptions . toAesonOptions $ Proxy @opts of
    SupportedOptions options -> genericToSchema options (Proxy @a)
    UnsupportedTwoElemArray -> blankSchema Array
    UnsupportedObjectWithSingleField -> blankSchema Object

fromAesonOptions :: Aeson.Options -> SupportedOptions GenericSchemaOptions
fromAesonOptions options =
  fromAesonSumEncoding (Aeson.sumEncoding options) <&> \sumOptions ->
    GenericSchemaOptions
      { fieldNameModifier = Aeson.fieldLabelModifier options
      , constructorTagModifier = Aeson.constructorTagModifier options
      , sumEncoding = sumOptions
      , allNullaryToStringTag = Aeson.allNullaryToStringTag options
      , tagSingleConstructors = Aeson.tagSingleConstructors options
      , useDiscrimatorField = False  -- no analog in aeson
      }

-- | For handling conversion from Aeson options that are not easy to describe in OpenAPI
data SupportedOptions a
  = SupportedOptions a
  | UnsupportedObjectWithSingleField
  | UnsupportedTwoElemArray
  deriving stock (Show, Functor)

fromAesonSumEncoding :: Aeson.SumEncoding -> SupportedOptions SumEncoding
fromAesonSumEncoding = \case
  Aeson.TaggedObject tag contents -> SupportedOptions $ Tagged tag contents
  Aeson.UntaggedValue -> SupportedOptions $ Untagged
  Aeson.ObjectWithSingleField -> UnsupportedObjectWithSingleField
  Aeson.TwoElemArray -> UnsupportedTwoElemArray

instance (ToOpenAPISchema a, KnownSymbol field)
  => ToOpenAPISchema (SingleFieldObject field a) where
  toSchema Proxy =
    blankObjectSchema
      { properties = Just . Properties $
          Map.singleton
            (textVal $ Proxy @field)
            (Concrete $ set #title Nothing baseSchema)
      , title = view #title baseSchema
      }

    where
      baseSchema = toSchema $ Proxy @a

instance (ToOpenAPISchema a, KnownJSONObject obj)
  => ToOpenAPISchema (WithConstantFieldsOut obj a) where
    toSchema Proxy
      = over (#properties) addConstantProperties
      . toSchema
      $ Proxy @a

      where
        addConstantProperties :: Maybe Properties -> Maybe Properties
        addConstantProperties
          = foldl (.) id $
            (HashMap.toList . objectVal $ Proxy @obj) <&> \(fieldName, val) ->
              addToProperties fieldName (valToSchema val)

instance (ToOpenAPISchema a, KnownJSONObject obj)
  => ToOpenAPISchema (WithConstantFieldsIn obj a) where
    toSchema Proxy
      = over (#required) setRequired
      . toSchema
      $ Proxy @(WithConstantFieldsOut obj a)

      where
        extraObjKeys = HashMap.keys . objectVal $ Proxy @obj

        setRequired :: Maybe [Text] -> Maybe [Text]
        setRequired = Just . maybe extraObjKeys (extraObjKeys <>)

-- Will replace this third newtype with a type synonym in a future release of aeson-deriving.
instance (ToOpenAPISchema a, KnownJSONObject obj)
  => ToOpenAPISchema (WithConstantFields obj a) where
    toSchema Proxy = toSchema $
      Proxy @(WithConstantFieldsIn obj (WithConstantFieldsOut obj a))


arraySchema :: SchemaObject -> SchemaObject
arraySchema elementSchema = (blankSchema Array) {items = Just $ Concrete elementSchema}

valToSchema :: Aeson.Value -> SchemaObject
valToSchema = \case
  Aeson.String txt -> (blankSchema String) {enum = Just [txt]}
  Aeson.Bool _ -> blankSchema Boolean
  Aeson.Null -> blankSchema Null
  Aeson.Array _ -> arraySchema blank
  Aeson.Object hm -> blankObjectSchema
    { properties
      = Just
      . Properties
      . Map.fromList
      . over (mapped . _2) (Concrete . valToSchema)
      $ HashMap.toList hm
    }
  Aeson.Number _ -> blankSchema Number

textVal :: KnownSymbol s => Proxy s -> Text
textVal = T.pack . symbolVal
