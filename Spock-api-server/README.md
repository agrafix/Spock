# Typed API server

Use `defEndpoint` for the original JSON endpoint DSL, or
`defDocumentedEndpoint` for typed query/header parameters and OpenAPI metadata.
See [the API documentation](../Spock-api/README.md) for the declarations and
validation rules.

Both registration functions use **Spock-core routing**. For cookie-authenticated
browser endpoints in full Spock, put `csrfCheck` in a prehook for unsafe routes
or add equivalent CSRF middleware; `spc_csrfProtection` does not wrap routes
registered through Spock-core. The included example has no authentication or
persistent state.

A runnable example uses the same typed declarations to register routes and
generate the OpenAPI document:

```sh
cabal run spock-api-example
curl -H 'X-Client: example' 'http://localhost:8082/items/3?offset=4'
curl -X PATCH -H 'Content-Type: application/json' -d '9' http://localhost:8082/items/3
curl -X DELETE http://localhost:8082/items/3
curl http://localhost:8082/openapi.json
```

The example demonstrates typed responses; PATCH/DELETE do not change stored
data. Export just the specification with:

```sh
cabal run -v0 spock-api-example -- --openapi > openapi.json
```

You can load that document into OpenAPI tooling. The custom `Item` schema in
`example/ApiDefinitions.hs` demonstrates a record schema with a nullable field.
