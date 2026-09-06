# Typed endpoints and OpenAPI

`Endpoint` describes a typed path, optional JSON body, and JSON response. Version
0.15 adds `MethodDelete` (no body) and `MethodPatch` (JSON body), alongside GET,
POST, and PUT. Existing `defEndpoint` handlers retain their argument order.

`Web.Spock.Api.Document` adds metadata that the server and OpenAPI generator
share. For example:

```haskell
{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

import Data.Text (Text)
import Web.Spock.Api
import Web.Spock.Api.Document

item :: DocumentedEndpoint '[Int] '[Maybe Int, Text] 'Nothing Text
item = DocumentedEndpoint (MethodGet $ "items" <//> var)
  ((operationInfo "getItem") { oi_summary = "Read an item", oi_tags = ["items"] })
  (PathParameter (parameterInfo "id" intSchema) NoPathParameters)
  (OptionalQueryParam (parameterInfo "offset" intSchema)
    :> HeaderParam (parameterInfo "X-Client" textSchema) :> NoParameters)
  NoBody textSchema
```

Path metadata must have one name/schema per captured type. `JsonBody schema`
must match the endpoint's input type, and the response schema matches its output
type. Primitive, array, and nullable schema helpers are provided. Use
`schemaObject` for custom JSON Schema objects that match your Aeson encodings;
the library does not infer arbitrary record schemas or validate custom codecs.

Register with `defDocumentedEndpoint` from `Spock-api-server`. Handler arguments
are path captures, query/header parameters in declaration order, then the JSON
body if present. Parameters use `FromHttpApiData` parsing. Available forms:

| Declaration | Handler value | Missing value |
| --- | --- | --- |
| `QueryParam info` | `a` | HTTP 400 |
| `OptionalQueryParam info` | `Maybe a` | `Nothing` |
| `QueryList info` | `[a]` | `[]` |
| `HeaderParam info` | `a` | HTTP 400 |
| `OptionalHeaderParam info` | `Maybe a` | `Nothing` |

Query lists use repeated keys (`?id=1&id=2`) and preserve order. Scalar duplicates
and malformed values return 400; header names are case-insensitive. An explicit
empty query value is parsed as empty text, not treated as an absent parameter.
Parameter errors use `{"error":"..."}` without echoing client values. Invalid
JSON bodies retain Spock-core's existing 400 behavior. Path parse failures retain
Spock's route matching behavior (usually 404). Header lists and cookie parameters
are not part of this initial API.

```haskell
openApiDocument "My API" "1.0.0" [SomeEndpoint item]
```

This returns `Either OpenApiError Value` containing an OpenAPI **3.1.1** document.
It describes methods, encoded paths, parameter names/locations/requiredness,
JSON schemas, request bodies, operation descriptions/tags, and JSON 200 responses.
Query lists use `style: form` and `explode: true`. Nullable schemas use `anyOf`
with JSON `null`. Generation rejects duplicate operation IDs, duplicate
path/method pairs, ambiguous path templates, and duplicate parameter names.
Individual metadata is also validated when a documented endpoint is registered.

The generator documents the endpoint's ordinary JSON 200 response. If handlers
use custom statuses, media types, authentication, or additional error responses,
extend the returned document to describe those contracts. OpenAPI reserves the
Accept, Content-Type, and Authorization header parameter names; use content or
security descriptions for them. See the
[OpenAPI 3.1.1 specification](https://spec.openapis.org/oas/v3.1.1.html).

`Spock-api-ghcjs` 0.15 implements these declarations with GHC's JavaScript backend.
See the [shared browser/server example](../examples/browser/README.md).
In API 0.17, query/header parameter constructors require both `FromHttpApiData`
and `ToHttpApiData`, carrying the parser and encoder in the shared definition.
Add an encoder instance when migrating a custom parameter type that previously
provided only a parser. Standard parameter types already supply both.

`openApiDocumentWith` and `renderRouteWith` accept the server's `SlashPolicy`
when using strict or canonical trailing-slash routing. Existing entry points
retain `IgnoreSlashes`. See the [routing guide](https://www.spock.li/tutorials/routing).

Typed file extensions use `<.>`: `var <.> "txt"` or `var <.> var`.
Use `renderRouteEncoded` for percent-encoded links. The
[routing guide](https://www.spock.li/tutorials/routing#file-extensions-in-typed-routes)
covers matching precedence, multiple dots, and typed extension values.
