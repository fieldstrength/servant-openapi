module Main where

import Servant.OpenAPI.Internal.Types
import Servant.OpenAPI.Internal
import OpenAPI.ToSchema.Internal
import Data.Yaml as Yaml
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

import Servant.API

main :: IO ()
main = do
  _petstore1 <- decodeFileThrow @_ @OpenAPI "examples/petstore.yaml"
  _petstore2 <- decodeFileThrow @_ @OpenAPI "examples/petstore-expanded.yaml"
  --print petstoreExpanded

  let endpoints = toEndpoints $ Proxy @API
  print endpoints

  putStrLn ""

  BS.putStrLn (Yaml.encode endpoints)

-- More examples at:
-- https://github.com/OAI/OpenAPI-Specification/tree/master/examples/v3.0


type API
  = "dogs"
  :> QueryFlag "good_boy"
  :> ReqBody '[JSON] Dog
  :> PutCreated '[JSON] (Headers '[Header "Location" Text] Dog)

data Dog = Dog
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)

data User = Anonymous Text | LoggedInUser Int Text
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)
