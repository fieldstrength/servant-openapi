module OpenAPI.ToSchema
  ( -- * Main schema class
  ToOpenAPISchema(..)
  -- * Generic schema class
  , GToOpenAPI(..)
  -- ** Generic class configuration
  , GenericSchemaOptions(..)
  , SumEncoding(..)
  , defaultSchemaOptions
  -- ** Entrypoint function for schema deriving
  , genericToSchema
  ) where

import OpenAPI.ToSchema.Internal
