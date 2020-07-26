-- | Provides the functionality for generating the 'OpenAPI' for a servant API. For examples see the Github README.
module Servant.OpenAPI
  ( -- * Interface for generating OpenAPI definition
    HasOpenAPI
  , toOpenAPI
  , blankInfo
  ) where

import Servant.OpenAPI.Internal
