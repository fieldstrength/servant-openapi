{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Servant.OpenAPI.Internal where

import           Data.Functor
import           Data.Generics.Labels           ()
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (mapMaybe)
import           Data.Proxy
import qualified Data.Text                      as Text
import           GHC.Generics
import           GHC.TypeLits
import           OpenAPI.ToSchema
import           Servant.API                    as Servant
import           Servant.API.Modifiers
import           Servant.OpenAPI.Internal.Types as OpenAPI


class HasAPISchema api where
  toAPISchema :: Proxy api -> OpenAPI

data EndpointInfo = EndpointInfo PathPattern PathItemObject


class HasEndpointSchema endpoint where
  toEndpointSchema :: Proxy endpoint -> AttrSet

class HasEndpointAttribute endpoint where
  toAttribute :: Proxy endpoint -> AttrSet

instance (HasEndpointAttribute x, HasEndpointSchema xs) => HasEndpointSchema (x :> xs) where
  toEndpointSchema Proxy = toAttribute (Proxy @x) <> toEndpointSchema (Proxy @ xs)

instance KnownSymbol str => HasEndpointAttribute (str :: Symbol) where
  toAttribute Proxy
    = AttrSet
    . pure
    . PathPiece
    . PathVariable
    . Text.pack
    . symbolVal
    $ Proxy @str

instance (SBoolI (FoldRequired mods), KnownSymbol name, ToOpenAPISchema a)
  => HasEndpointAttribute (QueryParam' mods name a) where
    toAttribute Proxy = AttrSet . pure . Param $
      ParameterObject
        { in_ = Query
        , name = Text.pack . symbolVal $ Proxy @name
        , description = Nothing
        , required = case sbool @(FoldRequired mods) of
            STrue -> Just True
            SFalse -> Just False
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

instance KnownSymbol name => HasEndpointAttribute (QueryFlag name) where
  toAttribute Proxy = AttrSet . pure . Param $
    ParameterObject
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

instance (SBoolI (FoldRequired mods), KnownSymbol name, ToOpenAPISchema a)
  => HasEndpointAttribute (Header' mods name a) where
    toAttribute Proxy = AttrSet . pure . Param $
      ParameterObject
        { in_ = OpenAPI.Header
        , name = Text.pack . symbolVal $ Proxy @name
        , description = Nothing
        , required = case sbool @(FoldRequired mods) of
            STrue -> Just True
            SFalse -> Just False
        , deprecated = Nothing
        , allowEmptyValue = Nothing
        , style = Nothing
        , explode = Nothing
        , allowReserved = Nothing
        , schema = Just . Concrete . toSchema $ Proxy @a
        , example = Nothing
        , examples = Nothing
        , content = Nothing
        }

-- NOTE: 'Capture' uses FromHttpApiData rather than FromJSON for deserialization
instance (KnownSymbol name, ToOpenAPISchema a) => HasEndpointAttribute (Capture name a) where
  toAttribute Proxy = AttrSet . pure . Param $
    ParameterObject
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

instance (ToOpenAPISchema a, SBoolI (FoldLenient mods)) => HasEndpointAttribute (ReqBody' mods contentTypes a) where
  toAttribute Proxy = AttrSet . pure . Body $
    RequestBodyObject
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

-- OVERLAPPABLE / OVERLAPPING structure of following two instances cribbed from HasServer instances
-- Either there is a naked return type, or one wrapped in `Headers`. See also:
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-overlap

instance {-# OVERLAPPABLE #-} (IsVerb verb, KnownNat status)
  => HasEndpointAttribute (Verb verb status contentTypes returned) where
    toAttribute Proxy = AttrSet
      [ Op . toVerb $ Proxy @verb
      , Response OperationObject
        { tags = Nothing
        , summary = Nothing
        , description = Nothing
        , externalDocs = Nothing
        , operationId = Nothing
        , parameters = Nothing
        , requestBody = Nothing
        -- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#responsesObject
        , responses = ResponsesObject .
            Map.singleton (Text.pack . show . natVal $ Proxy @status) . Concrete $ ResponseObject
              { description = ":-)"  -- FIXME
              , headers = Nothing
              , content = Nothing
              , links = Nothing
              }
        , callbacks = Nothing
        , deprecated = Nothing
        , security = Nothing
        , servers = Nothing
        }
      ]

instance {-# OVERLAPPING #-} (IsVerb verb, KnownNat status, KnownHeaders hs)
  => HasEndpointAttribute (Verb verb status contentTypes (Headers hs returned)) where
    toAttribute Proxy = AttrSet
      [ Op . toVerb $ Proxy @verb
      , Response OperationObject
        { tags = Nothing
        , summary = Nothing
        , description = Nothing
        , externalDocs = Nothing
        , operationId = Nothing
        , parameters = Nothing
        , requestBody = Nothing
        -- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#responsesObject
        , responses = ResponsesObject .
            Map.singleton (Text.pack . show . natVal $ Proxy @status) . Concrete $ ResponseObject
              { description = ":-)"
              , headers = Just $ Map.fromList $ (headerVals $ Proxy @hs) <&> \h ->
                (Text.pack h,) . Concrete $ HeaderObject
                  { description = Nothing
                  , required = Nothing
                  , deprecated = Nothing
                  , explode = Nothing
                  , schema = Nothing
                  , example = Nothing
                  , examples = Nothing
                  }
              , content = Nothing
              , links = Nothing
              }
        , callbacks = Nothing
        , deprecated = Nothing
        , security = Nothing
        , servers = Nothing
        }
      ]


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

-- Basically collect endpoint stuff as described in Operations, PathItem objects
-- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#operation-object
-- https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md#pathsObject
data Attribute
  = Param ParameterObject
  | PathPiece PathPatternPiece
  | Op VERB
  | Body RequestBodyObject
  | Response OperationObject -- More info than available from response
  deriving stock (Show, Generic)

newtype AttrSet = AttrSet [Attribute] deriving newtype (Show, Semigroup, Monoid)

-- OperationObject  - contains parameter list, req body, responses. also server info
-- RequestBodyObject  - contains the reqbody schema within a Map indexed by content types
-- PathItemObject   - Describes multiple endpoints corresponding to different verbs associated to a path.
--                  - Contains an OperationObject for every verb.

class KnownHeaders hs where
  headerVals :: Proxy hs -> [String]

instance KnownHeaders ('[] :: [*]) where headerVals Proxy = []
instance (KnownSymbol str, KnownHeaders rest) => KnownHeaders ((Header str a ': rest) :: [*]) where
  headerVals Proxy = symbolVal (Proxy @str) : headerVals (Proxy @rest)


interpPathSegment :: Attribute -> Maybe PathPatternPiece
interpPathSegment (PathPiece x) = Just x
interpPathSegment _ = Nothing

interpAttrSet :: AttrSet -> EndpointInfo
interpAttrSet (AttrSet attrs) =
  EndpointInfo
    (PathPattern $ mapMaybe interpPathSegment attrs)
    PathItemObject
      { ref = Nothing
      , summary = Nothing
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
