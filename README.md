[![Build Status](https://travis-ci.com/felixmulder/servant-openapi.svg?branch=master)](https://travis-ci.com/felixmulder/servant-openapi)

servant-openapi
===============
[![Hackage](https://img.shields.io/hackage/v/servant-openapi.svg)](http://hackage.haskell.org/package/servant-openapi)

Automatically derive OpenAPI definitions for your [Servant][servant-link] API.

[servant-link]: https://www.servant.dev/


openapi
===============
[![Hackage](https://img.shields.io/hackage/v/openapi.svg)](http://hackage.haskell.org/package/openapi)

Implements data types for OpenAPI 3.0.3 to specify your API and type serialization formats. Includes a type class to tie your haskell types to their schemas, and supports automatically deriving schemas together with the serialization directly using `DerivingVia`. This is accomplished by instances for the expressive type-level DSL of [aeson-deriving](https://github.com/fieldstrength/aeson-deriving).


### Example Usage

```haskell
type MyOpts =
  '[ FieldLabelModifier := SnakeCase
  ,  SumEncoding := TaggedObject "type" "contents"
  ,  TagSingleConstructors := 'True
  ]

newtype UserId = UserId Int64
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToOpenAPISchema)

data CreateUser = CreateUser
  { firstName :: Text
  , lastName :: Text
  , dateOfBirth :: Day
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON, ToOpenAPISchema) via GenericEncoded MyEncodingOpts CreateUser

data User = User
  { userId :: UserId
  , createdAt :: UTCTime
  , firstName :: Text
  , lastName :: Text
  , dateOfBirth :: Day
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON, ToOpenAPISchema) via GenericEncoded MyEncodingOpts User

type UserAPI =
  "users" :>
    (    ReqBody '[JSON] CreateUser :> Put '[JSON] User
    :<|> Capture "user_id" UserId :> Delete '[JSON] NoContent
    )

writeUserOpenAPI :: FilePath -> IO ()
writeUserOpenAPI path =
  writeSchemaYaml path $
    toOpenAPI (Proxy @UserAPI) InfoObject
      { title = "Acme Users API"
      , description = Nothing
      , termsOfService = Nothing
      , contact = Nothing
      , license = Nothing
      , version = "1.0"
      }
```

The resulting schema, obtained by running `writeUserOpenAPI` is the following:

```yaml
components:
  schemas:
    User:
      required:
      - type
      - user_id
      - created_at
      - first_name
      - last_name
      - date_of_birth
      title: User
      type: object
      properties:
        first_name:
          type: string
        date_of_birth:
          format: date
          type: string
        last_name:
          type: string
        created_at:
          format: date-time
          type: string
        type:
          type: string
          enum:
          - User
        user_id:
          format: int64
          type: integer
    CreateUser:
      required:
      - type
      - first_name
      - last_name
      - date_of_birth
      title: CreateUser
      type: object
      properties:
        first_name:
          type: string
        date_of_birth:
          format: date
          type: string
        last_name:
          type: string
        type:
          type: string
          enum:
          - CreateUser
openapi: 3.0.3
info:
  version: '1.0'
  title: Acme Users API
paths:
- - /users
  - put:
      responses:
        '400':
          description: Failure to parse request body or required parameter
        '200':
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/User'
          description: Successful result response
      request_body:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/CreateUser'
- - /users/{user_id}
  - delete:
      responses:
        '400':
          description: Failure to parse request body or required parameter
        '200':
          description: Empty response
      parameters:
      - allow_empty_value: false
        required: true
        schema:
          format: int64
          type: integer
        in: path
        name: user_id

```
