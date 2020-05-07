-- |
--  https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.3.md
--
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.OpenAPI.Internal.Types where

import           Control.Lens.Type (Lens')
import           Control.Monad ((>=>))
import qualified Data.Aeson as Aeson (Value)
import           Data.Aeson
import           Data.Char (toLower)
import           Data.Functor ((<&>))
import           Data.Generics.Labels ()
import           Data.List.Extra (stripSuffix)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import qualified Data.Text as Text
import           Data.Text (Text)
import           GHC.Generics (Generic(..))


data OpenAPI = OpenAPI
  { openapi :: Text
    -- ^ This string MUST be the semantic version number of the OpenAPI
    --   Specification version that the OpenAPI document uses. The openapi field
    --   SHOULD be used by tooling specifications and clients to interpret the
    --   OpenAPI document. This is not related to the API info.version string.
  , info :: InfoObject
    -- ^ Provides metadata about the API. The metadata MAY be used by tooling
    --   as required
  , servers :: Maybe [ServerObject]
    -- ^ An array of Server Objects, which provide connectivity information to
    --   a target server. If the servers property is not provided, or is an empty
    --   array, the default value would be a Server Object with a url value of
    --   @/@.
  , paths :: Map PathPattern PathItemObject
    -- ^ The available paths and operations for the API
  , compontents :: Maybe ComponentsObject
    -- ^ An element to hold various schemas for the specification
  , security :: Maybe [SecurityRequirementObject]
    -- ^ A declaration of which security mechanisms can be used across the API.
    --   The list of values includes alternative security requirement objects
    --   that can be used. Only one of the security requirement objects need to
    --   be satisfied to authorize a request. Individual operations can override
    --   this definition. To make security optional, an empty security
    --   requirement ({}) can be included in the array.
  , tags :: Maybe [TagObject]
    -- ^ A list of tags used by the specification with additional metadata. The
    --   order of the tags can be used to reflect on their order by the parsing
    --   tools. Not all tags that are used by the Operation Object must be
    --   declared. The tags that are not declared MAY be organized randomly or
    --   based on the tools' logic. Each tag name in the list MUST be unique.
  , externalDocs :: Maybe ExternalDocumentationObject
    -- ^ Additional external documentation
  }
  deriving stock (Generic, Show)

apiInfo :: Lens' OpenAPI InfoObject
apiInfo = #info

apiServers :: Lens' OpenAPI (Maybe [ServerObject])
apiServers = #servers

apiPaths :: Lens' OpenAPI (Map PathPattern PathItemObject)
apiPaths = #paths

apiComponents :: Lens' OpenAPI (Maybe ComponentsObject)
apiComponents = #compontents

apiSecurity :: Lens' OpenAPI (Maybe [SecurityRequirementObject])
apiSecurity = #security

apiTags :: Lens' OpenAPI (Maybe [TagObject])
apiTags = #tags

apiExternalDocs :: Lens' OpenAPI (Maybe ExternalDocumentationObject)
apiExternalDocs = #externalDocs

data InfoObject = InfoObject
  { title :: Text
    -- ^ The title of the API
  , description :: Maybe Text
    -- ^ A short description of the API. CommonMark syntax MAY be used for rich
    --   text representation.
  , termsOfService :: Maybe Text
    -- ^ A URL to the Terms of Service for the API. MUST be in the format of a
    --   URL
  , contact :: Maybe ContactObject
    -- ^ The contact information for the exposed API
  , license :: Maybe LicenseObject
    -- ^ The license information for the exposed API
  , version :: Text
    -- ^ The version of the OpenAPI document (which is distinct from the
    --   OpenAPI Specification version or the API implementation version)
  }
  deriving stock (Generic, Show)

infoTitle :: Lens' InfoObject Text
infoTitle = #title

infoDescription :: Lens' InfoObject (Maybe Text)
infoDescription = #description

infoTermsOfService :: Lens' InfoObject (Maybe Text)
infoTermsOfService = #termsOfService

infoContact :: Lens' InfoObject (Maybe ContactObject)
infoContact = #contact

infoLicense :: Lens' InfoObject (Maybe LicenseObject)
infoLicense = #license

infoVersion :: Lens' InfoObject Text
infoVersion = #version

data ServerObject = ServerObject
  { url :: Text
    -- ^ A URL to the target host. This URL supports Server Variables and MAY
    --   be relative, to indicate that the host location is relative to the
    --   location where the OpenAPI document is being served. Variable
    --   substitutions will be made when a variable is named in @{@ brackets @}@.
  , description :: Maybe Text
    -- ^ An optional string describing the host designated by the URL.
    --   CommonMark syntax MAY be used for rich text representation.
  , variables :: Maybe (Map Text ServerVariableObject)
    -- ^ A map between a variable name and its value. The value is used for
    --   substitution in the server's URL template.
  }
  deriving stock (Generic, Show)

serverUrl :: Lens' ServerObject Text
serverUrl = #url

serverDescription :: Lens' ServerObject (Maybe Text)
serverDescription = #description

data ServerVariableObject = ServerVariableObject
  { enum :: Maybe [Text]
    -- ^ An enumeration of string values to be used if the substitution options
    --   are from a limited set. The array SHOULD NOT be empty.
  , default_ :: Text
    -- ^ The default value to use for substitution, which SHALL be sent if an
    --   alternate value is not supplied. Note this behavior is different than
    --   the Schema Object's treatment of default values, because in those cases
    --   parameter values are optional. If the enum is defined, the value SHOULD
    --   exist in the enum's values.
  , description :: Maybe Text
    -- ^ An optional description for the server variable. CommonMark syntax MAY
    --   be used for rich text representation.
  }
  deriving stock (Generic, Show)

data TagObject = TagObject
  { name :: Text
    -- ^ The name of the tag
  , description :: Maybe Text
    -- ^ A short description for the tag. CommonMark syntax MAY be used for
    --   rich text representation.
  , externalDocs :: Maybe ExternalDocumentationObject
    -- ^ Additional external documentation for this tag
  }
  deriving stock (Generic, Show)

tagName :: Lens' TagObject Text
tagName = #name

tagDescription :: Lens' TagObject (Maybe Text)
tagDescription = #description

tagExternalDocs :: Lens' TagObject (Maybe ExternalDocumentationObject)
tagExternalDocs = #externalDocs

data ExternalDocumentationObject = ExternalDocumentationObject
  { url :: Text
    -- ^ A short description of the target documentation. CommonMark syntax MAY
    --   be used for rich text representation.
  , description :: Maybe Text
    -- ^ The URL for the target documentation. Value MUST be in the format of a
    --   URL
  }
  deriving stock (Generic, Show)

externalDocsUrl :: Lens' ExternalDocumentationObject Text
externalDocsUrl = #url

externalDocsDescription :: Lens' ExternalDocumentationObject (Maybe Text)
externalDocsDescription = #description

data LicenseObject = LicenseObject
  { name :: Text
    -- ^ The license name used for the API
  , url :: Maybe Text
    -- ^ A URL to the license used for the API. MUST be in the format of a URL
  }
  deriving stock (Generic, Show)

licenseName :: Lens' LicenseObject Text
licenseName = #name

licenseUrl :: Lens' LicenseObject (Maybe Text)
licenseUrl = #url

data ContactObject = ContactObject
  { name :: Maybe Text
    -- ^ The identifying name of the contact person/organization
  , url :: Maybe Text
    -- ^ The URL pointing to the contact information. MUST be in the format of
    --   a URL
  , email :: Maybe Text
    -- ^ The email address of the contact person/organization. MUST be in the
    --   format of an email address
  }
  deriving stock (Generic, Show)

contactName :: Lens' ContactObject (Maybe Text)
contactName = #name

contactUrl :: Lens' ContactObject (Maybe Text)
contactUrl = #url

contactEmail :: Lens' ContactObject (Maybe Text)
contactEmail = #email

data SecurityRequirementObject = SecurityRequirementObject -- FIXME
  deriving stock (Generic, Show)

newtype PathPattern' = PathPattern' Text
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (IsString, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype PathPattern = PathPattern [PathPatternPiece]
  deriving stock (Generic, Show, Eq, Ord)

instance FromJSON PathPattern where
  parseJSON = withText "String" $ pure . pathPatternFromText

pathPatternFromText :: Text -> PathPattern
pathPatternFromText path = PathPattern $ (Text.splitOn "/" path) <&> \pathPiece ->
  maybe (PathPart pathPiece) PathVariable $
    Text.stripPrefix "{" >=> Text.stripSuffix "}" $ pathPiece


instance FromJSONKey PathPattern where
  fromJSONKey = FromJSONKeyText pathPatternFromText


data PathPatternPiece
  = PathVariable Text
    -- ^ A variable in the path
  | PathPart Text
    -- ^ A subpart of the path
  deriving stock (Generic, Show, Eq, Ord)

data PathItemObject = PathItemObject
  { ref :: Maybe Text
    -- ^ Allows for an external definition of this path item
    --
    --   The referenced structure MUST be in the format of a Path Item Object.
    --   In case a Path Item Object field appears both in the defined object
    --   and the referenced object, the behavior is undefined.
  , summary ::  Maybe Text
    -- ^ An optional, string summary, intended to apply to all operations in
    --   this path.
  , description :: Maybe Text
    -- ^ An optional, string description, intended to apply to all operations in
    --   this path. CommonMark syntax MAY be used for rich text representation.
  , get :: Maybe OperationObject
    -- ^ A definition of a GET operation on this path
  , put :: Maybe OperationObject
    -- ^ A definition of a PUT operation on this path
  , post :: Maybe OperationObject
    -- ^ A definition of a POST operation on this path
  , delete :: Maybe OperationObject
    -- ^ A definition of a DELETE operation on this path
  , options :: Maybe OperationObject
    -- ^ A definition of an OPTIONS operation on this path
  , head :: Maybe OperationObject
    -- ^ A definition of a HEAD operation on this path
  , patch :: Maybe OperationObject
    -- ^ A definition of a PATCH operation on this path
  , trace :: Maybe OperationObject
    -- ^ A definition of a TRACE operation on this path
  , servers :: Maybe [ServerObject]
    -- ^ An alternative server array to service all operations in this path.
  , parameters :: Maybe [ReferenceOr ParameterObject]
    -- ^ A list of parameters that are applicable for all the operations
    --   described under this path. These parameters can be overridden at the
    --   operation level, but cannot be removed there. The list MUST NOT
    --   include duplicated parameters. A unique parameter is defined by a
    --   combination of a name and location. The list can use the Reference
    --   Object to link to parameters that are defined at the OpenAPI Object's
    --   components/parameters.
  }
  deriving stock (Generic, Show)

data OperationObject = OperationObject
  { tags :: Maybe [Text]
    -- ^ A list of tags for API documentation control. Tags can be used for
    --   logical grouping of operations by resources or any other qualifier.
  , summary :: Maybe Text
    -- ^ A short summary of what the operation does
  , description :: Maybe Text
    -- ^ A verbose explanation of the operation behavior. CommonMark syntax MAY
    --   be used for rich text representation.
  , externalDocs :: Maybe ExternalDocumentationObject
    -- ^ Additional external documentation for this operation
  , operationId :: Maybe Text
    -- ^ Unique string used to identify the operation. The id MUST be unique
    --   among all operations described in the API. The operationId value is
    --   case-sensitive. Tools and libraries MAY use the operationId to uniquely
    --   identify an operation, therefore, it is RECOMMENDED to follow common
    --   programming naming conventions.
  , parameters :: Maybe [ReferenceOr ParameterObject]
    -- ^ A list of parameters that are applicable for this operation. If a
    --   parameter is already defined at the Path Item, the new definition will
    --   override it but can never remove it. The list MUST NOT include duplicated
    --   parameters. A unique parameter is defined by a combination of a name and
    --   location. The list can use the Reference Object to link to parameters that
    --   are defined at the OpenAPI Object's components/parameters.
  , requestBody :: Maybe (ReferenceOr RequestBodyObject)
    -- ^ The request body applicable for this operation. The requestBody is only
    --   supported in HTTP methods where the HTTP 1.1 specification RFC7231 has
    --   explicitly defined semantics for request bodies. In other cases where the
    --   HTTP spec is vague, requestBody SHALL be ignored by consumers.
  , responses :: ResponsesObject
    -- ^ The list of possible responses as they are returned from executing
    --   this operation
  , callbacks :: Maybe (ReferenceOr CallbackObject)
    -- ^ A map of possible out-of band callbacks related to the parent operation.
    --   The key is a unique identifier for the Callback Object. Each value in the
    --   map is a Callback Object that describes a request that may be initiated by
    --   the API provider and the expected responses.
  , deprecated :: Maybe Bool
    -- ^ Declares this operation to be deprecated. Consumers SHOULD refrain from
    --   usage of the declared operation. Default value is false.
  , security :: Maybe [SecurityRequirementObject]
    -- ^ A declaration of which security mechanisms can be used for this
    --   operation. The list of values includes alternative security requirement
    --   objects that can be used. Only one of the security requirement objects
    --   need to be satisfied to authorize a request. To make security optional,
    --   an empty security requirement ({}) can be included in the array. This
    --   definition overrides any declared top-level security. To remove a
    --   top-level security declaration, an empty array can be used.
  , servers :: Maybe [ServerObject]
    -- ^ An alternative server array to service this operation. If an alternative
    --   server object is specified at the Path Item Object or Root level, it will
    --   be overridden by this value.
  }
  deriving stock (Generic, Show)

data ComponentsObject = ComponentsObject
  { schemas :: [ReferenceOr SchemaObject]
    -- ^ An object to hold reusable Schema Objects
  , responses :: [ReferenceOr ResponseObject]
    -- ^ An object to hold reusable Response Objects
  , parameters :: [ReferenceOr ParameterObject]
    -- ^ An object to hold reusable Parameter Objects
  , examples :: [ReferenceOr ExampleObject]
    -- ^ An object to hold reusable Example Objects
  , requestBodies :: [ReferenceOr RequestBodyObject]
    -- ^ An object to hold reusable Request Body Objects
  , headers :: [ReferenceOr HeaderObject]
    -- ^ An object to hold reusable Header Objects
  , securitySchemes :: [ReferenceOr SecuritySchemeObject]
    -- ^ An object to hold reusable Security Scheme Objects
  , links :: [ReferenceOr LinkObject]
    -- ^ An object to hold reusable Link Objects
  , callbacks :: [ReferenceOr CallbackObject]
    -- ^ An object to hold reusable Callback Objects
  }
  deriving stock (Generic, Show)

-- | An object containing a @$ref@ field
data ReferenceObject = ReferenceObject { ref :: Text }
  deriving stock (Generic, Show)

data ReferenceOr a = ReferenceOr a  -- FIXME
-- data ReferenceOr a
--  = Reference Text ReferenceObject
--  | Or Text a
  deriving stock (Generic, Show)

data ResponseObject = ResponseObject
  { description :: Text
    -- ^ A short description of the response. CommonMark syntax MAY be used for
    --   rich text representation.
  , headers :: Maybe [ReferenceOr HeaderObject]
    -- ^ Maps a header name to its definition. RFC7230 states header names are
    --   case insensitive. If a response header is defined with the name
    --   "Content-Type", it SHALL be ignored.
  , content :: Maybe (Map Text MediaTypeObject)
    -- ^ A map containing descriptions of potential response payloads. The key is
    --   a media type or media type range and the value describes it. For responses
    --   that match multiple keys, only the most specific key is applicable. e.g.
    --   text/plain overrides text/*
  , links :: Maybe [ReferenceOr LinkObject]
    -- ^ A map of operations links that can be followed from the response. The
    --   key of the map is a short name for the link, following the naming
    --   constraints of the names for Component Objects.
  }
  deriving stock (Generic, Show)

data ParameterObject = ParameterObject
  { name :: Text
    -- ^ The name of the parameter. Parameter names are case sensitive.
    --
    --   If in is "path", the name field MUST correspond to a template
    --   expression occurring within the path field in the Paths Object. See
    --   Path Templating for further information.
    --
    --   If in is "header" and the name field is "Accept", "Content-Type" or
    --   "Authorization", the parameter definition SHALL be ignored.
    --
    --   For all other cases, the name corresponds to the parameter name used
    --   by the in property.
  , in_ :: ParameterIn
    -- ^ REQUIRED. The location of the parameter
  , description :: Maybe Text
    -- ^ A brief description of the parameter. This could contain examples of
    --   use. CommonMark syntax MAY be used for rich text representation.
  , required :: Maybe Bool
    -- ^ Determines whether this parameter is mandatory. If the parameter
    --   location is "path", this property is REQUIRED and its value MUST be
    --   true. Otherwise, the property MAY be included and its default value is
    --   false.
  , deprecated :: Maybe Bool
    -- ^ Specifies that a parameter is deprecated and SHOULD be transitioned
    --   out of usage. Default value is false.
  , allowEmptyValue :: Maybe Bool
    -- ^ Sets the ability to pass empty-valued parameters. This is valid only for
    --   query parameters and allows sending a parameter with an empty value.
    --   Default value is false. If style is used, and if behavior is n/a (cannot
    --   be serialized), the value of allowEmptyValue SHALL be ignored. Use of this
    --   property is NOT RECOMMENDED, as it is likely to be removed in a later
    --   revision.
  , style :: Maybe StyleValue
    -- ^ Describes how the parameter value will be serialized depending on the
    --   type of the parameter value. Default values (based on value of in):
    --   for @query - form@; for @path - simple@; for @header - simple@; for
    --   @cookie - form@.
  , explode :: Maybe Bool
    -- ^ When this is true, parameter values of type array or object generate
    --   separate parameters for each value of the array or key-value pair of the
    --   map. For other types of parameters this property has no effect. When
    --   style is form, the default value is true. For all other styles, the
    --   default value is false.
  , allowReserved :: Maybe Bool
    -- ^ Determines whether the parameter value SHOULD allow reserved characters,
    --   as defined by RFC3986 @:/?#[]@!$&'()*+,;=@ to be included without
    --   percent-encoding. This property only applies to parameters with an in
    --   value of query. The default value is false.
  , schema :: Maybe (ReferenceOr SchemaObject)
    -- ^ The schema defining the type used for the parameter.
  , example :: Maybe Aeson.Value
    -- ^ Example of the parameter's potential value. The example SHOULD match
    --   the specified schema and encoding properties if present. The example
    --   field is mutually exclusive of the examples field. Furthermore, if
    --   referencing a schema that contains an example, the example value SHALL
    --   override the example provided by the schema. To represent examples of
    --   media types that cannot naturally be represented in JSON or YAML, a
    --   string value can contain the example with escaping where necessary.
  , examples :: Maybe [ReferenceOr ExampleObject]
  -- ^ Examples of the parameter's potential value. Each example SHOULD contain
  --   a value in the correct format as specified in the parameter encoding.
  --   The examples field is mutually exclusive of the example field.
  --   Furthermore, if referencing a schema that contains an example, the
  --   examples value SHALL override the example provided by the schema.
  , content :: Maybe (Map Text MediaTypeObject)
  -- ^ A map containing the representations for the parameter. The key is the
  --   media type and the value describes it. The map MUST only contain one
  --   entry.
  }
  deriving stock (Generic, Show)

-- | The @in@ field as an enum
data ParameterIn
  = Query
  | Header
  | Path
  | Cookie
  deriving stock (Generic, Show)

data StyleValue
  = Matrix
    -- ^ Path-style parameters defined by RFC6570
  | Label
    -- ^ Label style parameters defined by RFC6570
  | Form
    -- ^ Form style parameters defined by RFC6570. This option replaces
    --   collectionFormat with a csv (when explode is false) or multi (when
    --   explode is true) value from OpenAPI 2.0.
  | Simple
    -- ^ Simple style parameters defined by RFC6570. This option replaces
    --   collectionFormat with a csv value from OpenAPI 2.0.
  | SpaceDelimited
    -- ^ Space separated array values. This option replaces collectionFormat
    --   equal to ssv from OpenAPI 2.0.
  | PipeDelimited
    -- ^ Pipe separated array values. This option replaces collectionFormat
    --   equal to pipes from OpenAPI 2.0.
  | DeepObject
    -- ^ Provides a simple way of rendering nested objects using form
    --   parameters.
  deriving stock (Generic, Show)

data ExampleObject = ExampleObject
  { summary :: Maybe Text
    -- ^ Short description for the example
  , description :: Maybe Text
    -- ^ Long description for the example
    --
    --   CommonMark syntax MAY be used for rich text representation.
  , value :: Maybe Aeson.Value
    -- ^ Embedded literal example
    --
    --   The 'value' field and 'externalValue' field are mutually exclusive. To
    --   represent examples of media types that cannot naturally represented in
    --   JSON or YAML, use a string value to contain the example, escaping
    --   where necessary.
  , externalValue :: Maybe Text
    -- ^ A URL that points to the literal example
    --
    --   This provides the capability to reference examples that cannot easily
    --   be included in JSON or YAML documents. The 'value' field and
    --   'externalValue' field are mutually exclusive.
  }
  deriving stock (Generic, Show)

data RequestBodyObject = RequestBodyObject
  { description :: Maybe Text
    -- ^ A brief description of the request body. This could contain examples
    --   of use. CommonMark syntax MAY be used for rich text representation.
  , content :: Map Text MediaTypeObject
    -- ^ The content of the request body. The key is a media type or media type
    --   range and the value describes it. For requests that match multiple keys,
    --   only the most specific key is applicable. e.g. text/plain overrides
    --   @text/*@
  , required :: Bool
    -- ^ Determines if the request body is required in the request. Defaults to
    --   false
  }
  deriving stock (Generic, Show)

data HeaderObject = HeaderObject
  { description :: Maybe Text
    -- ^ A brief description of the parameter. This could contain examples of
    --   use. CommonMark syntax MAY be used for rich text representation.
  , required :: Maybe Bool
    -- ^ Determines whether this parameter is mandatory. If the parameter
    --   location is "path", this property is REQUIRED and its value MUST be
    --   true. Otherwise, the property MAY be included and its default value is
    --   false.
  , deprecated :: Maybe Bool
    -- ^ Specifies that a parameter is deprecated and SHOULD be transitioned
    --   out of usage. Default value is false.
  , explode :: Maybe Bool
    -- ^ When this is true, parameter values of type array or object generate
    --   separate parameters for each value of the array or key-value pair of the
    --   map. For other types of parameters this property has no effect. The
    --   default value is false.
  , schema :: Maybe SchemaOrReference
    -- ^ The schema defining the type used for the parameter.
  , example :: Maybe Aeson.Value
    -- ^ Example of the parameter's potential value. The example SHOULD match
    --   the specified schema and encoding properties if present. The example
    --   field is mutually exclusive of the examples field. Furthermore, if
    --   referencing a schema that contains an example, the example value SHALL
    --   override the example provided by the schema. To represent examples of
    --   media types that cannot naturally be represented in JSON or YAML, a
    --   string value can contain the example with escaping where necessary.
  , examples :: Maybe [ReferenceOr ExampleObject]
  -- ^ Examples of the parameter's potential value. Each example SHOULD contain
  --   a value in the correct format as specified in the parameter encoding.
  --   The examples field is mutually exclusive of the example field.
  --   Furthermore, if referencing a schema that contains an example, the
  --   examples value SHALL override the example provided by the schema.
  }
  deriving stock (Generic, Show)

data SecuritySchemeObject = SecuritySchemeObject
  { type_ :: SecuritySchemaType
    -- ^ The type of the security scheme. Valid values are "apiKey", "http",
    --   "oauth2", "openIdConnect".
  , description :: Maybe Text
    -- ^ A short description for security scheme
    --
    --   CommonMark syntax MAY be used for rich text representation.
  , name :: Maybe Text
    -- ^ The name of the header, query or cookie parameter to be used.
    --
    --   /Note:/ Required when @type_ == "apiKey"@
  , in_ :: Maybe Text
    -- ^ The location of the API key. Valid values are "query", "header" or
    --   "cookie".
    --
    --   /Note:/ Required when @type_ == "apiKey"@
  , scheme :: Maybe Text
    -- ^ The name of the HTTP Authorization scheme to be used in the
    --   Authorization header as defined in RFC7235. The values used SHOULD be
    --   registered in the IANA Authentication Scheme registry.
    --
    --   /Note:/ Required when @type_ == "http"@
  , bearerFormat :: Maybe Text
    -- ^ A hint to the client to identify how the bearer token is formatted.
    --   Bearer tokens are usually generated by an authorization server, so this
    --   information is primarily for documentation purposes.
    --
    --   /Note:/ Required when @type_ == "http" && scheme == "bearer"@
  , flows :: Maybe OathFlowsObject
    -- ^ An object containing configuration information for the flow types
    --   supported.
    --
    --   /Note:/ Required when @type_ == "oauth2"@
  , openIdConnectUrl :: Maybe Text
    -- ^ OpenId Connect URL to discover OAuth2 configuration values. This MUST
    --   be in the form of a URL.
    --
    --   /Note:/ Required when @type_ == "openIdConnect"@
  }
  deriving stock (Generic, Show)

newtype SecuritySchemaType = SecuritySchemaType Text
  deriving newtype (IsString)
  deriving stock (Generic, Show)

data OathFlowsObject = OauthFlowsObject
  { implicit :: Maybe ImplicitOauthFlowObject
    -- ^ Configuration for the OAuth Implicit flow
  , password :: Maybe PasswordOauthFlowObject
    -- ^ Configuration for the OAuth Resource Owner Password flow
  , clientCredentials :: Maybe ClientCredentialsOauthFlowObject
    -- ^ Configuration for the OAuth Client Credentials flow. Previously called
    --   @application@ in OpenAPI 2.0.
  , authorizationCode :: Maybe AuthorizationCodeOauthFlowObject
    -- ^ Configuration for the OAuth Authorization Code flow. Previously called
    --   @accessCode@ in OpenAPI 2.0.
  }
  deriving stock (Generic, Show)

data SchemaObject = SchemaObject
  { title :: Maybe Text
    -- ^ The name of the schema
  , type_ :: SchemaType
    -- ^ Value MUST be a string. Multiple types via an array are not supported.
  , multipleOf :: Maybe Int
    -- ^ When @type@ is integer, this defines valid values as a multiple
  , maximum :: Maybe Int
    -- ^ When @type@ is integer, this defines the maximum value allowed
  , exclusiveMaximum :: Maybe Int
    -- ^ When @type@ is integer, this defines the non-inclusive maximum value
    --   allowed. E.g. @exclusiveMaximum: 3@ largest value allowed is @2@
  , minimum :: Maybe Int
    -- ^ When @type@ is integer, this defines the minimum value allowed
  , exclusiveMinimum :: Maybe Int
    -- ^ When @type@ is integer, this defines the non-inclusive minimum value
    --   allowed. E.g. @exclusiveMinimum: 3@ smallest value allowed is @4@
  , maxLength :: Maybe Int
    -- ^ When @type@ is string, this defines the maximum length of the string
  , minLength :: Maybe Int
    -- ^ When @type@ is string, this defines the minimum length of the string
  , pattern :: Maybe Text
    -- ^ This string SHOULD be a valid regular expression, according to the
    --   Ecma-262 Edition 5.1 regular expression dialect)
  , maxItems :: Maybe Int
    -- ^ When @type@ is array, this defines the maximum length of the array
  , minItems :: Maybe Int
    -- ^ When @type@ is array, this defines the minimum length of the array
  , uniqueItems :: Maybe Bool
    -- ^ When @type@ is array, this defines if duplicates are disallowed,
    --   defaults to @False@
  , maxProperties :: Maybe Int
    -- ^ When @type@ is object, this defines the max of how many properties are
    --   allowed
  , minProperties :: Maybe Int
    -- ^ When @type@ is object, this defines the min of how many properties are
    --   allowed
  , required :: Maybe [Text]
    -- ^ When @type@ is object, this defines which properties are required
  , enum :: Maybe [Text]
    -- ^ When @type@ is string, this defines the possible values the string may
    --   take
  , allOf :: Maybe [SchemaOrReference]
    -- ^ Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
  , oneOf :: Maybe [SchemaOrReference]
    -- ^ Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
  , anyOf :: Maybe [SchemaOrReference]
    -- ^ Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
  , not :: Maybe [SchemaOrReference]
    -- ^ Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema.
  , items :: Maybe SchemaOrReference
    -- ^ Value MUST be an object and not an array. Inline or referenced schema
    --   MUST be of a Schema Object and not a standard JSON Schema. items MUST be
    --   present if the type is array.
  , properties :: Maybe Properties
    -- ^ Property definitions MUST be a Schema Object and not a standard JSON Schema (inline or referenced).
  , additionalProperties :: Maybe (Either Bool SchemaOrReference)
    -- ^ When @type_ == "object" 'additionalProperties' can be set to:
    --
    --    - Bool
    --    - Object
    --    - Schema
    --
    --   If @additionalProperties == True || additionalProperties == {}@, keys of object may have any type.
    --
    --   If it's a schema, or schema reference, all values in the map are of that type.
    --
    --   /Note:/ additionalProperties defaults to true.
  , description :: Maybe Text
    -- ^ CommonMark syntax MAY be used for rich text representation.
  , format :: Maybe Text
    -- ^ See Data Type Formats for further details. While relying on JSON
    --   Schema's defined formats, the OAS offers a few additional predefined
    --   formats.
  , default_ :: Maybe Aeson.Value
    -- ^ The default value represents what would be assumed by the consumer of
    --   the input as the value of the schema if one is not provided. Unlike JSON
    --   Schema, the value MUST conform to the defined type for the Schema Object
    --   defined at the same level. For example, if type is string, then default
    --   can be "foo" but cannot be 1.
  }
  deriving stock (Generic, Show)

data SchemaType
  = Number
  | Integer
  | String
  | Object
  | Array
  | Boolean
  | Null
  deriving stock (Generic, Show)

newtype Properties = Properties { unProperties :: Map Text SchemaOrReference }
  deriving stock (Generic, Show)

data SchemaOrReference
  = PropertyReferenceObject ReferenceObject
  | PropertySchemaObject SchemaObject
  deriving stock (Generic, Show)

data MediaTypeObject = MediaTypeObject
  { schema :: Maybe SchemaOrReference
    -- ^ The schema defining the content of the request, response, or
    --   parameter.
  , example :: Maybe Aeson.Value
    -- ^ Example of the media type. The example object SHOULD be in the correct
    --   format as specified by the media type. The example field is mutually
    --   exclusive of the examples field. Furthermore, if referencing a schema
    --   which contains an example, the example value SHALL override the example
    --   provided by the schema.
  , examples :: [ReferenceOr ExampleObject]
    -- ^ Examples of the media type. Each example object SHOULD match the media
    --   type and specified schema if present. The examples field is mutually
    --   exclusive of the example field. Furthermore, if referencing a schema
    --   which contains an example, the examples value SHALL override the example
    --   provided by the schema.
  , encoding :: Maybe (Map Text EncodingObject)
    -- ^ A map between a property name and its encoding information. The key,
    --   being the property name, MUST exist in the schema as a property. The
    --   encoding object SHALL only apply to requestBody objects when the media
    --   type is multipart or application/x-www-form-urlencoded.
  }
  deriving stock (Generic, Show)

-- | A single encoding definition applied to a single schema property
data EncodingObject = EncodingObject
  { contentType :: Maybe Text
    -- ^ The Content-Type for encoding a specific property. Default value
    --   depends on the property type: for string with format being binary –
    --   application/octet-stream; for other primitive types – text/plain; for
    --   object - application/json; for array – the default is defined based on
    --   the inner type. The value can be a specific media type (e.g.
    --   application/json), a wildcard media type (e.g. image/*), or a
    --   comma-separated list of the two types.
  , headers :: Map Text (ReferenceOr HeaderObject)
    -- ^ A map allowing additional information to be provided as headers, for
    --   example Content-Disposition. Content-Type is described separately and
    --   SHALL be ignored in this section. This property SHALL be ignored if
    --   the request body media type is not a multipart.
  , style :: Maybe Text
    -- ^ Describes how a specific property value will be serialized depending
    --   on its type. See Parameter Object for details on the style property. The
    --   behavior follows the same values as query parameters, including default
    --   values. This property SHALL be ignored if the request body media type is
    --   not application/x-www-form-urlencoded.
  , explode :: Maybe Bool
    -- ^ When this is true, property values of type array or object generate
    --   separate parameters for each value of the array, or key-value-pair of
    --   the map. For other types of properties this property has no effect. When
    --   style is form, the default value is true. For all other styles, the
    --   default value is false. This property SHALL be ignored if the request
    --   body media type is not application/x-www-form-urlencoded.
  , allowReserved :: Maybe Bool
    -- ^ Determines whether the parameter value SHOULD allow reserved
    --   characters, as defined by RFC3986 :/?#[]@!$&'()*+,;= to be included
    --   without percent-encoding. The default value is false. This property
    --   SHALL be ignored if the request body media type is not
    --   application/x-www-form-urlencoded.
  }
  deriving stock (Generic, Show)

-- | A container for the expected responses of an operation. The container maps
--   a HTTP response code to the expected response.
--
--   The documentation is not necessarily expected to cover all possible HTTP
--   response codes because they may not be known in advance. However,
--   documentation is expected to cover a successful operation response and any
--   known errors.
--
--   The default MAY be used as a default response object for all HTTP codes
--   that are not covered individually by the specification.
--
--  The Responses Object MUST contain at least one response code, and it SHOULD
--  be the response for a successful operation call.
data ResponsesObject = ResponsesObject
  { httpDefault :: Maybe (Either ResponseObject ReferenceObject)
    -- ^ The documentation of responses other than the ones declared for specific
    --   HTTP response codes. Use this field to cover undeclared responses. A
    --   Reference Object can link to a response that the OpenAPI Object's
    --   components/responses section defines.
  , httpStatuses :: Maybe (Map Text (Either ResponseObject ReferenceObject))
    -- ^ A map of HTTP status codes to their expected response
    --
    --   Any HTTP status code can be used as the property name, but only one
    --   property per code, to describe the expected response for that HTTP
    --   status code. A Reference Object can link to a response that is defined
    --   in the OpenAPI Object's components/responses section. This field MUST be
    --   enclosed in quotation marks (for example, "200") for compatibility
    --   between JSON and YAML. To define a range of response codes, this field
    --   MAY contain the uppercase wildcard character X. For example, 2XX
    --   represents all response codes between [200-299]. Only the following
    --   range definitions are allowed: 1XX, 2XX, 3XX, 4XX, and 5XX. If a
    --   response is defined using an explicit code, the explicit code definition
    --   takes precedence over the range definition for that code.
  }
  deriving stock (Generic, Show)

-- | Configuration details for a supported OAuth Flow
data ImplicitOauthFlowObject = ImplicitOauthFlowObject
  { authorizationUrl :: Text
  , refreshUrl :: Maybe Text
  , scopes :: Map Text Text
  }
  deriving stock (Generic, Show)

-- | Configuration details for a supported OAuth Flow
data PasswordOauthFlowObject = PasswordOauthFlowObject
  { tokenUrl :: Text
  , refreshUrl :: Maybe Text
  , scopes :: Map Text Text
  }
  deriving stock (Generic, Show)

-- | Configuration details for a supported OAuth Flow
data ClientCredentialsOauthFlowObject = ClientCredentialsOauthFlowObject
  { tokenUrl :: Text
  , refreshUrl :: Maybe Text
  , scopes :: Map Text Text
  }
  deriving stock (Generic, Show)

-- | Configuration details for a supported OAuth Flow
data AuthorizationCodeOauthFlowObject = AuthorizationCodeOauthFlowObject
  { authorizationUrl :: Text
  , tokenUrl :: Text
  , refreshUrl :: Maybe Text
  , scopes :: Map Text Text
  }
  deriving stock (Generic, Show)

-- | A map of possible out-of band callbacks related to the parent operation.
--   Each value in the map is a Path Item Object that describes a set of requests
--   that may be initiated by the API provider and the expected responses. The
--   key value used to identify the path item object is an expression, evaluated
--   at runtime, that identifies a URL to use for the callback operation.
newtype CallbackObject = CallbackObject (Map Text PathItemObject)
  deriving stock (Generic, Show)

-- | The Link object represents a possible design-time link for a response. The
--   presence of a link does not guarantee the caller's ability to successfully
--   invoke it, rather it provides a known relationship and traversal mechanism
--   between responses and other operations.
--
--   Unlike dynamic links (i.e. links provided in the response payload), the OAS
--   linking mechanism does not require link information in the runtime response.
--
--   For computing links, and providing instructions to execute them, a runtime
--   expression is used for accessing values in an operation and using them as
--   parameters while invoking the linked operation.
data LinkObject = LinkObject
  { operationRef :: Maybe Text
    -- ^ A relative or absolute URI reference to an OAS operation. This field
    --   is mutually exclusive of the operationId field, and MUST point to an
    --   Operation Object. Relative operationRef values MAY be used to locate an
    --   existing Operation Object in the OpenAPI definition.
  , operationId :: Maybe Text
    -- ^ The name of an existing, resolvable OAS operation, as defined with a
    --   unique operationId. This field is mutually exclusive of the operationRef
    --   field.
  , parameters :: Maybe (Map String (Either Aeson.Value Expression))
    -- ^ A map representing parameters to pass to an operation as specified
    --   with operationId or identified via operationRef. The key is the
    --   parameter name to be used, whereas the value can be a constant or an
    --   expression to be evaluated and passed to the linked operation. The
    --   parameter name can be qualified using the parameter location
    --   [{in}.]{name} for operations that use the same parameter name in
    --   different locations (e.g. path.id).
  , requestBody :: Maybe (Either Aeson.Value Expression)
    -- ^ A literal value or {expression} to use as a request body when calling
    --   the target operation.
  , description :: Maybe Text
    -- ^ A description of the link. CommonMark syntax MAY be used for rich text
    --   representation.
  , server :: Maybe ServerObject
    -- ^ A server object to be used by the target operation.
  }
  deriving stock (Generic, Show)

-- | Runtime expressions allow defining values based on information that will
--   only be available within the HTTP message in an actual API call. This
--   mechanism is used by Link Objects and Callback Objects.
--
-- @
--   expression = ( "$url" / "$method" / "$statusCode" / "$request." source / "$response." source )
--   source = ( header-reference / query-reference / path-reference / body-reference )
--   header-reference = "header." token
--   query-reference = "query." name
--   path-reference = "path." name
--   body-reference = "body" ["#" json-pointer ]
--   json-pointer    = *( "/" reference-token )
--   reference-token = *( unescaped / escaped )
--   unescaped       = %x00-2E / %x30-7D / %x7F-10FFFF
--      ; %x2F ('/') and %x7E ('~') are excluded from 'unescaped'
--   escaped         = "~" ( "0" / "1" )
--     ; representing '~' and '/', respectively
--   name = *( CHAR )
--   token = 1*tchar
--   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
--     "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
-- @
newtype Expression = Expression Text
  deriving stock (Generic, Show)


snakeCase :: String -> String
snakeCase = camelTo2 '_' . stripUnderscore
  where
    stripUnderscore str = fromMaybe str $ stripSuffix "_" str

jsonOptions :: Options
jsonOptions = defaultOptions {fieldLabelModifier = snakeCase}

-- TODO use aeson-deriving
instance FromJSON OpenAPI where parseJSON = genericParseJSON jsonOptions
instance FromJSON ExternalDocumentationObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON InfoObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON TagObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ServerObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON PathItemObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ComponentsObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON SecurityRequirementObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ContactObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON LicenseObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON OperationObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ServerVariableObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ParameterObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ReferenceObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON SchemaObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ResponseObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ExampleObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON RequestBodyObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON HeaderObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON SecuritySchemeObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON LinkObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ResponsesObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON CallbackObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ParameterIn where parseJSON = genericParseJSON jsonOptions {constructorTagModifier = fmap toLower} -- FIXME be case insensitive
instance FromJSON StyleValue where parseJSON = genericParseJSON jsonOptions
instance FromJSON MediaTypeObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON SchemaOrReference where parseJSON = genericParseJSON jsonOptions
instance FromJSON Properties where parseJSON = genericParseJSON jsonOptions
instance FromJSON SecuritySchemaType where parseJSON = genericParseJSON jsonOptions
instance FromJSON OathFlowsObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON Expression where parseJSON = genericParseJSON jsonOptions
instance FromJSON EncodingObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON SchemaType where parseJSON = genericParseJSON jsonOptions {constructorTagModifier = fmap toLower} -- FIXME be case insensitive
instance FromJSON ImplicitOauthFlowObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON PasswordOauthFlowObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON ClientCredentialsOauthFlowObject where parseJSON = genericParseJSON jsonOptions
instance FromJSON AuthorizationCodeOauthFlowObject where parseJSON = genericParseJSON jsonOptions

instance FromJSON a => FromJSON (ReferenceOr a) where parseJSON = fmap ReferenceOr . parseJSON
