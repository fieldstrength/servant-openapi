module Main where

import Servant.OpenAPI.Internal.Types
import Data.Yaml

main :: IO ()
main = do
  _ <- decodeFileThrow @_ @OpenAPI "examples/petstore.yaml"
  petstoreExpanded <- decodeFileThrow @_ @OpenAPI "examples/petstore-expanded.yaml"
  print petstoreExpanded

-- More examples at:
-- https://github.com/OAI/OpenAPI-Specification/tree/master/examples/v3.0
