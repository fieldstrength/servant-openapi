{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.OpenAPI.Internal where

import           Control.Applicative    ((<|>))
import           Control.Lens           (Lens', mapped, over, set, view, (&), _1)
import           Data.Functor
import           Data.Generics.Labels   ()
import           Data.Map               (Map)
import qualified Data.Map.Strict        as Map
import           Data.Proxy
import qualified Data.Text              as Text
import           GHC.Generics
import           GHC.TypeLits
import           OpenAPI
import           Servant.API            as Servant
import           Servant.API.Modifiers

-- | Create an 'OpenAPI' object from the servant-generated endpoint data and provided
--   'InfoObject'.
toOpenAPI :: forall api. HasOpenAPI api => Proxy api -> InfoObject -> OpenAPI
toOpenAPI Proxy info =
  blankOpenAPI
    & set #paths (toEndpointInfo $ Proxy @api)
    & set #info info
    & pruneAndReference

-- | Create an 'OpenAPI' object from the servant-generated endpoint data by providing
--   the bare minimum hardcoded stub values for metadata fields. See 'blankOpenAPI'.
--   Prefer to use 'toOpenAPI' instead for most uses.
toBareOpenAPI :: forall api. HasOpenAPI api => Proxy api -> OpenAPI
toBareOpenAPI Proxy =
  blankOpenAPI
    & set #paths (toEndpointInfo $ Proxy @api)
    & set #info blankInfo
    & pruneAndReference

-- | Provide meaningless values for the required fields of 'InfoObject'. Consider
--   filling in meaningful values for the required fields. Otherwise this gives:
--
--     * title: "Untitled API"
--
--     * version: "1.0"
blankInfo :: InfoObject
blankInfo = InfoObject
  { title = "Untitled API"
  , description = Nothing
  , termsOfService = Nothing
  , contact = Nothing
  , license = Nothing
  , version = "1.0"
  }

-- | Provide meaningless stub values for the required data of 'OpenAPI'. It's
--   recommended to fill in required fields of 'InfoObject'; see 'blankInfo'.
blankOpenAPI :: OpenAPI
blankOpenAPI = OpenAPI
  { openapi = "3.0.3"
  , info = blankInfo
  , servers = Nothing
  , paths = mempty
  , components = Nothing
  , security = Nothing
  , tags = Nothing
  , externalDocs = Nothing
  }

-- | Only to be used with order-preserving function.
unsafeMapPathPatterns :: (PathPattern -> PathPattern) -> PathsObject -> PathsObject
unsafeMapPathPatterns f
  = Map.fromList
  . over (mapped . _1) f
  . Map.toList

class HasOpenAPI api where
  -- | WARNING: The resulting value may be infinite if the types contain self-references.
  --   This function is internal. Library users should instead use 'toOpenAPI', which
  --   contains a call to 'pruneAndReference' that makes the resulting 'OpenAPI' finite.
  toEndpointInfo :: Proxy api -> Map PathPattern PathItemObject


-- | The change to the 'PathsObject' may affect one or many endpoints.
instance (KnownSymbol str, HasOpenAPI api)
  => HasOpenAPI
  (str :> api) where
    toEndpointInfo Proxy =
      unsafeMapPathPatterns
        (over #unPathPattern $ (<>) [PathPart . Text.pack . symbolVal $ Proxy @str])
        (toEndpointInfo $ Proxy @api)

instance (ToOpenAPISchema a, KnownSymbol name, HasOpenAPI api)
  => HasOpenAPI
  (Capture name a :> api) where
    toEndpointInfo Proxy =
      unsafeMapPathPatterns
        (over #unPathPattern $ (<>) [PathVariable . Text.pack . symbolVal $ Proxy @name])
        (mapOperations (addParam param) <$> toEndpointInfo (Proxy @api))
      where
        param = ParameterObject
          { in_ = Path
          , name = Text.pack . symbolVal $ Proxy @name
          , description = Nothing
          , required = Just True
          , deprecated = Nothing
          , allowEmptyValue = Just False
          , style = Nothing
          , explode = Nothing
          , allowReserved = Nothing
          , schema = Just . Concrete . toSchema $ Proxy @a
          , example = Nothing
          , examples = Nothing
          , content = Nothing
          }

instance (ToOpenAPISchema a, KnownSymbol name, SBoolI (FoldRequired mods), HasOpenAPI api)
  => HasOpenAPI
  (QueryParam' mods name a :> api) where
    toEndpointInfo Proxy =
      mapOperations (addParam param) <$> toEndpointInfo (Proxy @api)
      where
        param = ParameterObject
          { in_ = Query
          , name = Text.pack . symbolVal $ Proxy @name
          , description = Nothing
          , required = Just $ case sbool @(FoldRequired mods) of
              STrue  -> True
              SFalse -> False
          , deprecated = Nothing
          , allowEmptyValue = Just False
          , style = Nothing
          , explode = Nothing
          , allowReserved = Nothing
          , schema = Just . Concrete . toSchema $ Proxy @a
          , example = Nothing
          , examples = Nothing
          , content = Nothing
          }

instance (ToOpenAPISchema a, KnownSymbol name, SBoolI (FoldRequired mods), HasOpenAPI api)
  => HasOpenAPI
  (Header' mods name a :> api) where
    toEndpointInfo Proxy =
      mapOperations (addParam param) <$> toEndpointInfo (Proxy @api)
      where
        param = ParameterObject
          { in_ = OpenAPI.Header
          , name = Text.pack . symbolVal $ Proxy @name
          , description = Nothing
          , required = Just $ case sbool @(FoldRequired mods) of
              STrue  -> True
              SFalse -> False
          , deprecated = Nothing
          , allowEmptyValue = Just False
          , style = Nothing
          , explode = Nothing
          , allowReserved = Nothing
          , schema = Just . Concrete . toSchema $ Proxy @a
          , example = Nothing
          , examples = Nothing
          , content = Nothing
          }

instance (HasOpenAPI api)
  => HasOpenAPI (BasicAuth realm a :> api) where
    toEndpointInfo Proxy =
      toEndpointInfo (Proxy @api)

instance (HasOpenAPI l, HasOpenAPI r)
  => HasOpenAPI
    (l :<|> r) where
      toEndpointInfo Proxy =
        Map.unionWith fish
          (toEndpointInfo $ Proxy @l)
          (toEndpointInfo $ Proxy @r)

fish :: PathItemObject -> PathItemObject -> PathItemObject
fish x y = PathItemObject
  { summary     = view #summary x      <|> view #summary y
  , description = view #description x  <|> view #description y
  , get         = view #get x          <|> view #get y
  , put         = view #put x          <|> view #put y
  , post        = view #post x         <|> view #post y
  , delete      = view #delete x       <|> view #delete y
  , options     = view #options x      <|> view #options y
  , head        = view #head x         <|> view #head y
  , patch       = view #patch x        <|> view #patch y
  , trace       = view #trace x        <|> view #trace y
  , servers     = view #servers x      <|> view #servers y
  , parameters  = view #parameters x   <|> view #parameters y
  }

instance (KnownSymbol name, HasOpenAPI api)
  => HasOpenAPI (QueryFlag name :> api) where
    toEndpointInfo Proxy =
      mapOperations (addParam param) <$> toEndpointInfo (Proxy @api)
      where
        param = ParameterObject
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

addParam :: ParameterObject -> OperationObject -> OperationObject
addParam param = over #parameters $
  maybe
    (Just [Concrete param])
    (\ps -> Just (Concrete param : ps))

instance
  ( HasOpenAPI api
  , KnownSymbol s
  ) => HasOpenAPI (Description s :> api) where
    toEndpointInfo Proxy = set #description (Just description) <$> toEndpointInfo @api Proxy
      where
        description = Text.pack . symbolVal $ Proxy @s

instance
  ( HasOpenAPI api
  , KnownSymbol s
  ) => HasOpenAPI (Summary s :> api) where
    toEndpointInfo Proxy = set #summary (Just summary) <$> toEndpointInfo @api Proxy
      where
        summary = Text.pack . symbolVal $ Proxy @s

instance
  ( HasOpenAPI api
  , ToOpenAPISchema a
  , SBoolI (FoldLenient mods))
  => HasOpenAPI
    (ReqBody' mods contentTypes a :> api) where
      toEndpointInfo Proxy =
        mapOperations (set #requestBody . Just $ Concrete body)
          <$> toEndpointInfo (Proxy @api)
        where
          body = RequestBodyObject
            { description = Nothing
            , content = Map.singleton applicationJson
              MediaTypeObject
                { schema = Just . Concrete . toSchema $ Proxy @a
                , example = Nothing
                , examples = Nothing
                , encoding = Nothing
                }
            , required = case sbool @(FoldLenient mods) of
              STrue  -> False
              SFalse -> True
            }

instance (v ~ Verb verb status contentTypes returned, HasOperation v, IsVerb verb)
  => HasOpenAPI
    (Verb verb status contentTypes returned) where
      toEndpointInfo Proxy =
        Map.singleton (PathPattern []) $
          set
            (verbLens . toVerb $ Proxy @verb)
            (Just . view #operation . toOperation $ Proxy @v)
            blankPathItem

instance (v ~ NoContentVerb verb, HasOperation v, IsVerb verb)
  => HasOpenAPI
    (NoContentVerb verb) where
      toEndpointInfo Proxy =
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
        , responses
            = ResponsesObject
            . Map.singleton (Text.pack . show . natVal $ Proxy @status)
            . Concrete
            . toResponseObject
            $ Proxy @response
        , callbacks = Nothing
        , deprecated = Nothing
        , security = Nothing
        , servers = Nothing
        }
      }

instance HasOperation (NoContentVerb verb) where
    toOperation Proxy = VerbOperation
      { status = 204
      , operation = OperationObject
        { tags = Nothing
        , summary = Nothing
        , description = Nothing
        , externalDocs = Nothing
        , operationId = Nothing
        , parameters = Nothing
        , requestBody = Nothing
        , responses
            = ResponsesObject
            . Map.singleton "204"
            . Concrete
            $ ResponseObject
                { description = "Successful no-content response"
                , headers = Nothing
                , content = Just $ Map.singleton applicationJson
                  MediaTypeObject
                    { schema = Nothing
                    , example = Nothing
                    , examples = Nothing
                    , encoding = Nothing
                    }
                , links = Nothing
                }
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


instance (KnownHeaders hs, HasResponse r) => HasResponse (Headers hs r) where
  toResponseObject Proxy =
    (toResponseObject $ Proxy @r)
      { headers = Just . Map.fromList $
        (headerVals $ Proxy @hs) <&> \(hStr, hSchema) ->
          (Text.pack hStr,) . Concrete $ HeaderObject
            { description = Nothing
            , required = Nothing
            , deprecated = Nothing
            , explode = Nothing
            , schema = Just $ Concrete hSchema
            , example = Nothing
            , examples = Nothing
            }
      }

instance {-# OVERLAPPABLE #-} (ToOpenAPISchema a) => HasResponse a where
  toResponseObject Proxy =
    ResponseObject
      { description = "Successful result response"  -- FIXME
      , headers = Nothing
      , content = Just $ Map.singleton applicationJson
        MediaTypeObject
          { schema = Just . Concrete . toSchema $ Proxy @a
          , example = Nothing
          , examples = Nothing
          , encoding = Nothing
          }
      , links = Nothing
      }

instance HasResponse NoContent where
  toResponseObject Proxy =
    ResponseObject
      { description = "Empty response"
      , headers = Nothing
      , content = Nothing
      , links = Nothing
      }


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

class IsVerb verb where toVerb :: Proxy verb -> VERB

instance IsVerb 'GET where toVerb Proxy = VerbGet
instance IsVerb 'PUT where toVerb Proxy = VerbPut
instance IsVerb 'POST where toVerb Proxy = VerbPost
instance IsVerb 'DELETE where toVerb Proxy = VerbDelete
instance IsVerb 'OPTIONS where toVerb Proxy = VerbOptions
instance IsVerb 'HEAD where toVerb Proxy = VerbHead
instance IsVerb 'PATCH where toVerb Proxy = VerbPatch
instance IsVerb 'TRACE where toVerb Proxy = VerbTrace


class KnownHeaders hs where
  headerVals :: Proxy hs -> [(String, SchemaObject)]

instance KnownHeaders ('[] :: [*]) where headerVals Proxy = []
instance (KnownSymbol str, ToOpenAPISchema a, KnownHeaders rest)
  => KnownHeaders ((Header str a ': rest) :: [*]) where
    headerVals Proxy = (symbolVal $ Proxy @str, toSchema $ Proxy @a) : headerVals (Proxy @rest)
