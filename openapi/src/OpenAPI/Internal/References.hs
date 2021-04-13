{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE StrictData       #-}

module OpenAPI.Internal.References where

import           Prelude hiding (not, maximum, minimum)

import           Control.Lens hiding (enum, allOf, anyOf)
import           Control.Monad.Trans.Accum
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Maybe                 ()
import           Data.Text                  (Text)
import           OpenAPI.Internal.Types
import           GHC.Generics
import           Control.Monad (unless)

-- | Add all named schemas to the '#components' environment, and replace
--   their occurrences with references.
pruneAndReference :: OpenAPI -> OpenAPI
pruneAndReference api = addEnv env prunedApi
  where
    env :: Map Text (ReferenceOr SchemaObject)
    prunedApi :: OpenAPI
    (prunedApi, env)
      = fmap (fmap Concrete . definitions)
      . flip runAccum mempty
      $ defineAndPrune api

-- | Traverse over all schemas in the 'OpenAPI' applying 'defineAndPruneSchema'.
defineAndPrune :: OpenAPI -> Accum DefinitionEnv OpenAPI
defineAndPrune = allPathItems . allOperations . operationSchemas $ defineAndPruneSchema

allPathItems :: Traversal' OpenAPI PathItemObject
allPathItems = #paths . traverse

-- | Add the given environment of schema definitions to an 'OpenAPI'.
addEnv :: Map Text (ReferenceOr SchemaObject) -> OpenAPI -> OpenAPI
addEnv env = over #components . (Just .) $
  maybe
    (set #schemas (Just env) emptyComponents)
    (over #schemas $ maybe (Just env) (Just . (env <>)))

-- | The data we keep track of while traversing the schemas in an OpenAPI.
--   It accumulates a set of schema definitions indexed by name.
--   The monoidal property is important for its use in the 'Accum' monad.
data DefinitionEnv = DefinitionEnv
  { definitions :: Map Text SchemaObject
  -- ^ The environment of definitions we are accumulating.
  , seen :: Set Text
  -- ^ A running list of names that have been seen. A guard against unbounded recursion
  --   in the presence of schemas with recursive self-references (direct or indirect).
  } deriving stock (Show, Eq, Generic)

instance Semigroup DefinitionEnv where
    d1 <> d2 = DefinitionEnv
      { definitions = definitions d1 <> definitions d2
      , seen = seen d1 <> seen d2
      }
instance Monoid DefinitionEnv where
    mempty = DefinitionEnv mempty mempty


-- | This prefix in a 'ReferenceObject' indicates to look in the indicated subobject of
--   the 'OpenAPI' for the referenced item.
localRefPrefix :: Text
localRefPrefix = "#/components/schemas/"

newDefinition :: Text -> SchemaObject -> DefinitionEnv
newDefinition name schema = DefinitionEnv (Map.singleton name schema) mempty

newSeenName :: Text -> DefinitionEnv
newSeenName name = DefinitionEnv mempty $ Set.singleton name

lookupDefinition :: Text -> DefinitionEnv -> Maybe SchemaObject
lookupDefinition name DefinitionEnv{definitions} =
  definitions Map.!? (localRefPrefix <> name)


-- | Add the present schema, along with all named schemas that occur
--   nested within it, in referenced form.
--   Defined as a Kleisli arrow so that it can be lifted with 'Traversal''.
defineAndPruneSchema :: ReferenceOr SchemaObject -> Accum DefinitionEnv (ReferenceOr SchemaObject)
defineAndPruneSchema = \case
  Ref r -> pure (Ref r)
  Concrete s -> case view #title s of
    Nothing -> do
        referencedSchema <- nestedSchemas defineAndPruneSchema s
        pure $ Concrete referencedSchema
    Just name -> do
      env <- look
      -- Always return the reference, regardless of other actions from this point
      toReference name <$ case lookupDefinition name env of
        Just _envSchema -> pure ()
        Nothing -> unless (name `Set.member` seen env) $ do
          -- Though we are not done with the present schema, we guard against recursing
          -- into the same definition again by adding it to the 'seen' list first
          add $ newSeenName name
          referencedSchema <- nestedSchemas defineAndPruneSchema s
          add $ newDefinition name referencedSchema

pruneSchema :: SchemaObject -> SchemaObject
pruneSchema = over nestedSchemas $ \case
  Ref n -> Ref n
  Concrete s -> case view #title s of
    Nothing -> Concrete s
    Just name -> toReference name

toReference :: Text -> ReferenceOr a
toReference name = Ref . ReferenceObject $ localRefPrefix <> name

-- | Given any 'Traversable' container of 'SchemaObject's, compile the fully referenced definition
--   environment. In the context of OpenAPI specs, you usually prefer to use 'pruneAndReference' instead.
--   This function on the other hand is useful if you only want definitions for serialization formats,
--   in a potentially different context than an HTTP API.
gatherSchemaEnvironment :: Traversable f => f SchemaObject -> Map Text SchemaObject
gatherSchemaEnvironment
  = definitions
  . flip execAccum mempty
  . traverse defineAndPruneSchema
  . fmap Concrete

nestedSchemas :: Traversal' SchemaObject (ReferenceOr SchemaObject)
nestedSchemas f SchemaObject{..} =
  SchemaObject
    <$> pure title
    <*> pure type_
    <*> pure discriminator
    <*> pure multipleOf
    <*> pure maximum
    <*> pure exclusiveMaximum
    <*> pure minimum
    <*> pure exclusiveMinimum
    <*> pure maxLength
    <*> pure minLength
    <*> pure pattern
    <*> pure maxItems
    <*> pure minItems
    <*> pure uniqueItems
    <*> pure maxProperties
    <*> pure minProperties
    <*> pure required
    <*> pure enum
    <*> (_Just . traverse @[]) f allOf
    <*> (_Just . traverse @[]) f oneOf
    <*> (_Just . traverse @[]) f anyOf
    <*> (_Just . traverse @[]) f not
    <*> _Just f items
    <*> (_Just . #unProperties . traverse @(Map Text)) f properties
    <*> (_Just . _Right) f additionalProperties
    <*> pure description
    <*> pure format
    <*> pure default_


-- | Traversal over nested schemas from 'OperationObject':
--     * parameters
--     * requestBody
--     * responses
operationSchemas :: Traversal' OperationObject (ReferenceOr SchemaObject)
operationSchemas f OperationObject{..} =
  OperationObject
    <$> pure tags
    <*> pure summary
    <*> pure description
    <*> pure externalDocs
    <*> pure operationId
    <*> parameterSchemas f parameters
    <*> requestBodySchemas f requestBody
    <*> responsesSchemas f responses
    <*> pure callbacks
    <*> pure deprecated
    <*> pure security
    <*> pure servers

  where
    parameterSchemas
      = _Just
      . traverse
      . #_Concrete
      . #schema
      . _Just

    responsesSchemas
      = #unResponsesObject
      . traverse
      . #_Concrete
      . responseSchemas

    requestBodySchemas
      = _Just
      . #_Concrete
      . #content
      . traverse
      . #schema
      . _Just

-- | Traversal over nested schemas from the 'ResponseObject':
--     * 'headers'
--     * 'content'
responseSchemas :: Traversal' ResponseObject (ReferenceOr SchemaObject)
responseSchemas f ResponseObject{..} =
  ResponseObject
    <$> pure description
    <*> headerSchemas f headers
    <*> contentSchemas f content
    <*> pure links

  where
    headerSchemas
      = _Just
      . traverse
      . #_Concrete
      . #schema
      . _Just

    contentSchemas
      = _Just
      . traverse
      . #schema
      . _Just
