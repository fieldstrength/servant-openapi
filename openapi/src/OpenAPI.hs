module OpenAPI
  ( -- * Type class for schemas
    ToOpenAPISchema(..)
  -- ** Generic deriving configuration
  , GenericSchemaOptions(..)
  , SumEncoding(..)
  , defaultSchemaOptions
  -- ** Entrypoint function for schema deriving
  , genericToSchema

  -- ** Type for adding a description with DerivingVia
  , Description(..)

  -- * Handling References
  , pruneAndReference
  , pruneSchema
  , gatherSchemaEnvironment

  -- * OpenAPI types

  -- ** Types for datatype schemas
  , SchemaObject(..)
  , SchemaType(..)
  , blankSchema
  , blankObjectSchema

  -- ** OpenAPI object type
  , OpenAPI(..)
  -- *** OpenAPI object field lenses
  , apiInfo
  , apiServers
  , apiPaths
  , apiComponents
  , apiSecurity
  , apiTags
  , apiExternalDocs

  , allOperationsMay
  , allOperations
  , mapOperations

  -- ** Components Object
  , ComponentsObject(..)
  , emptyComponents

  -- ** Types for references and abstraction over referencing
  , ReferenceOr(..)
  , ReferenceObject(..)

  -- ** Types for endpoint data
  , PathsObject
  , PathPattern(..)
  , PathItemObject(..)
  , PathPatternPiece(..)

  , pathPatternFromText
  , pathPatternToText

  -- ** Types for parameters
  , ParameterObject(..)
  , ParameterIn(..)
  , StyleValue(..)

  -- ** Types for HTTP verbs
  , OperationObject(..)

  , RequestBodyObject(..)

  , HeaderObject(..)

  , ResponseObject(..)
  , ResponsesObject(..)

  , MediaType(..)
  , MediaTypeObject(..)

  -- ** Info Object
  , InfoObject(..)
  -- *** Info object lenses
  , infoTitle
  , infoDescription
  , infoTermsOfService
  , infoContact
  , infoLicense
  , infoVersion

  -- ** Server Object
  , ServerObject(..)

  -- *** Server object lenses
  , serverUrl
  , serverDescription

  , ServerVariableObject(..)

  -- ** Tag object
  , TagObject(..)
  -- *** Tag object lenses
  , tagName
  , tagDescription
  , tagExternalDocs

  -- ** External documentation object
  , ExternalDocumentationObject(..)
  -- *** External documentation object lenses
  , externalDocsUrl
  , externalDocsDescription

  -- ** License object
  , LicenseObject(..)
  -- *** License object lenses
  , licenseName
  , licenseUrl

  -- ** Contact object
  , ContactObject(..)
  -- *** Contact object lenses
  , contactName
  , contactUrl
  , contactEmail

  , SecurityRequirementObject(..)

  -- * Rendering conveniences
  , renderSchemaYaml
  , renderSchemaJson
  , writeSchemaYaml
  , writeSchemaJson
  ) where

import OpenAPI.Internal.Class
import OpenAPI.Internal.Types
import OpenAPI.Internal.References

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson

renderSchemaYaml :: OpenAPI -> BS.ByteString
renderSchemaYaml = Yaml.encode

renderSchemaJson :: OpenAPI -> BSL.ByteString
renderSchemaJson = Aeson.encode

writeSchemaYaml :: FilePath -> OpenAPI -> IO ()
writeSchemaYaml path = BS.writeFile path . renderSchemaYaml

writeSchemaJson :: FilePath -> OpenAPI -> IO ()
writeSchemaJson path = BSL.writeFile path . renderSchemaJson
