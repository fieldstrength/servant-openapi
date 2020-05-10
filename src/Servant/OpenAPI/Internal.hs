{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}

module Servant.OpenAPI.Internal where

import           Control.Lens (Lens', mapped, over, set, view, _1)
import           Data.Functor
import           Data.Generics.Labels           ()
import           Data.Map (Map)
import qualified Data.Map.Strict                as Map
import           Data.Proxy
import qualified Data.Text                      as Text
import           GHC.TypeLits
import           GHC.Generics
import           OpenAPI.ToSchema
import           Servant.API                    as Servant
import           Servant.API.Modifiers
import           Servant.OpenAPI.Internal.Types as OpenAPI


class HasAPISchema api where
  toAPISchema :: Proxy api -> OpenAPI


type PathsObject = Map PathPattern PathItemObject

-- | Only to be used with order-preserving function
unsafeMapPathPatterns :: (PathPattern -> PathPattern) -> PathsObject -> PathsObject
unsafeMapPathPatterns f
  = Map.fromList
  . over (mapped . _1) f
  . Map.toList

class HasEndpoints api where
  toEndpoints :: Proxy api -> Map PathPattern PathItemObject


-- | The change to the 'PathsObject' may affect one or many endpoints.
instance (KnownSymbol str, HasEndpoints api)
  => HasEndpoints
  (str :> api) where
    toEndpoints Proxy =
      unsafeMapPathPatterns
        (over #unPathPattern $ ([PathPart . Text.pack . symbolVal $ Proxy @str]<>))
        (toEndpoints $ Proxy @api)

instance (KnownSymbol name, HasEndpoints api) => HasEndpoints (QueryFlag name :> api) where
  toEndpoints Proxy = (toEndpoints $ Proxy @api) <&> mapOperations (over #parameters updateParams)

    where
      updateParams = maybe (Just [param]) (\ps -> Just (param : ps))
      param = Concrete $ ParameterObject
        { in_ = Query
        , name = Text.pack . symbolVal $ Proxy @name
        , description = Nothing
        , required = Just False
        , deprecated = Nothing
        , allowEmptyValue = Just True
        , style = Nothing
        , explode = Nothing
        , allowReserved = Nothing
        , schema = Nothing
        , example = Nothing
        , examples = Nothing
        , content = Nothing
        }

instance
  ( HasEndpoints api
  , ToOpenAPISchema a
  , SBoolI (FoldLenient mods))
  => HasEndpoints
    (ReqBody' mods contentTypes a :> api) where
      toEndpoints Proxy =
        mapOperations (set #requestBody . Just $ Concrete body)
          <$> toEndpoints (Proxy @api)
        where
          body = RequestBodyObject
            { description = Nothing
            , content = Map.singleton "application/json"  -- FIXME
              MediaTypeObject
                { schema = Just . Concrete . toSchema $ Proxy @a
                , example = Nothing
                , examples = Nothing
                , encoding = Nothing
                }
            , required = case sbool @(FoldLenient mods) of
              STrue -> False
              SFalse -> True
            }

instance (v ~ Verb verb status contentTypes returned, HasOperation v, IsVerb verb)
  => HasEndpoints
    (Verb verb status contentTypes returned) where
      toEndpoints Proxy =
        Map.singleton (PathPattern []) $
          set
            (verbLens . toVerb $ Proxy @verb)
            (Just . view #operation . toOperation $ Proxy @v)
            blankPathItem

blankPathItem :: PathItemObject
blankPathItem = PathItemObject
  { summary = Nothing
  , description = Nothing
  , get = Nothing
  , put = Nothing
  , post = Nothing
  , delete = Nothing
  , options = Nothing
  , head = Nothing
  , patch = Nothing
  , trace = Nothing
  , servers = Nothing
  , parameters = Nothing
  }

class HasOperation api where
  toOperation :: Proxy api -> VerbOperation

data VerbOperation = VerbOperation
  { status :: Int
  , operation :: OperationObject
  } deriving stock (Generic)

instance (KnownNat status, HasResponse response)
  => HasOperation (Verb verb status contentTypes response) where
    toOperation Proxy = VerbOperation
      { status = fromInteger . natVal $ Proxy @status
      , operation = OperationObject
        { tags = Nothing
        , summary = Nothing
        , description = Nothing
        , externalDocs = Nothing
        , operationId = Nothing
        , parameters = Nothing
        , requestBody = Nothing
        -- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#responsesObject
        , responses = ResponsesObject .
            Map.singleton (Text.pack . show . natVal $ Proxy @status) . Concrete . toResponseObject $ Proxy @response
        , callbacks = Nothing
        , deprecated = Nothing
        , security = Nothing
        , servers = Nothing
        }
      }

class HasResponse api where
  toResponseObject :: Proxy api -> ResponseObject

applicationJson :: MediaType
applicationJson = "application/json"

instance {-# OVERLAPPABLE #-} (ToOpenAPISchema a, KnownHeaders hs) => HasResponse (Headers hs a) where
  toResponseObject Proxy =
    ResponseObject
      { description = "Successful result response"  -- FIXME
      , headers = Just . Map.fromList $
        (headerVals $ Proxy @hs) <&> \h ->
          (Text.pack h,) . Concrete $ HeaderObject
            { description = Nothing
            , required = Nothing
            , deprecated = Nothing
            , explode = Nothing
            , schema = Nothing  -- TODO: header schema
            , example = Nothing
            , examples = Nothing
            }
      , content = Just $ Map.singleton applicationJson MediaTypeObject
        { schema = Just . Concrete . toSchema $ Proxy @a
        , example = Nothing
        , examples = Nothing
        , encoding = Nothing
        }
      , links = Nothing
      }

--instance {-# OVERLAPPABLE #-} ToOpenAPISchema a => HasResponse a where
--  toResponseObject Proxy = ResponseObject
--    { description = "Successful result response"  -- FIXME
--    , headers = Nothing
--    , content = Just $ Map.singleton applicationJson MediaTypeObject
--      { schema = Just . Concrete . toSchema $ Proxy @a
--      , example = Nothing
--      , examples = Nothing
--      , encoding = Nothing
--      }
--    , links = Nothing
--    }
--
--instance {-# OVERLAPPABLE #-} (HasResponse a, KnownHeaders hs) => HasResponse (Headers hs a) where
--  toResponseObject Proxy =
--    (toResponseObject $ Proxy @a)
--      { headers = Just $
--        Map.fromList $ (headerVals $ Proxy @hs) <&> \h ->
--          (Text.pack h,) . Concrete $ HeaderObject
--            { description = Nothing
--            , required = Nothing
--            , deprecated = Nothing
--            , explode = Nothing
--            , schema = Nothing  -- TODO: header schema
--            , example = Nothing
--            , examples = Nothing
--            }
--      }



class IsVerb verb where toVerb :: Proxy verb -> VERB

instance IsVerb 'GET where toVerb Proxy = VerbGet
instance IsVerb 'PUT where toVerb Proxy = VerbPut
instance IsVerb 'POST where toVerb Proxy = VerbPost
instance IsVerb 'DELETE where toVerb Proxy = VerbDelete
instance IsVerb 'OPTIONS where toVerb Proxy = VerbOptions
instance IsVerb 'HEAD where toVerb Proxy = VerbHead
instance IsVerb 'PATCH where toVerb Proxy = VerbPatch
instance IsVerb 'TRACE where toVerb Proxy = VerbTrace



data VERB
  = VerbGet
  | VerbPut
  | VerbPost
  | VerbDelete
  | VerbOptions
  | VerbHead
  | VerbPatch
  | VerbTrace
  deriving stock Show

verbLens :: VERB -> Lens' PathItemObject (Maybe OperationObject)
verbLens = \case
  VerbGet -> #get
  VerbPut -> #put
  VerbPost -> #post
  VerbDelete -> #delete
  VerbOptions -> #options
  VerbHead -> #head
  VerbPatch -> #patch
  VerbTrace -> #trace

-- Basically collect endpoint stuff as described in Operations, PathItem objects
-- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#operation-object
-- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#pathsObject

-- OperationObject  - contains parameter list, req body, responses. also server info
-- RequestBodyObject  - contains the reqbody schema within a Map indexed by content types
-- PathItemObject   - Describes multiple endpoints corresponding to different verbs associated to a path.
--                  - Contains an OperationObject for every verb.

class KnownHeaders hs where
  headerVals :: Proxy hs -> [String] -- TODO: pair with schemas

instance KnownHeaders ('[] :: [*]) where headerVals Proxy = []
instance (KnownSymbol str, KnownHeaders rest) => KnownHeaders ((Header str a ': rest) :: [*]) where
  headerVals Proxy = symbolVal (Proxy @str) : headerVals (Proxy @rest)
