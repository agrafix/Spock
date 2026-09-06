Spock-core
==========

[![Build Status](https://travis-ci.org/agrafix/Spock.svg)](https://travis-ci.org/agrafix/Spock)
[![Hackage](https://img.shields.io/hackage/v/Spock-core.svg)](http://hackage.haskell.org/package/Spock-core)

## Intro

Hackage: [Spock-core](http://hackage.haskell.org/package/Spock-core)
Stackage: [Spock-core](https://www.stackage.org/package/Spock-core)

Documentation: [Spock-core 0.12.0.0](https://spockdocs.s3.eu-central-1.amazonaws.com/Spock-core-0.12.0.0/Web-Spock-Core.html)

`Spock-core` is the "low level" platform powering [Spock](https://www.stackage.org/package/Spock). You can use it standalone if you
do not need the framework to provide you features like session management, database pooling, csrf protection and more.

For more information please visit our homepage at [www.spock.li](https://www.spock.li).
## Request IDs and structured logs

Enable `sc_logging = Just (defaultLoggingConfig sink)` in `defaultSpockConfig`.
Full Spock exposes the same option as `spc_logging` in `defaultSpockCfg`.
The sink receives `LogEvent` values for handler messages, errors, and responses;
`LogEvent` has a JSON instance. The existing text error hook still runs.

```haskell
let sink event = pushLogStrLn logger (toLogStr $ encode event)
    cfg = defaultSpockConfig { sc_logging = Just $ defaultLoggingConfig sink }
spockConfigT cfg id $ get root $ do
  logMessage LogInfo "serving home" [("example", toJSON True)]
  getRequestId >>= text . maybe "missing" id
```

This example uses `fast-logger` and `aeson`; a complete executable is included:

```sh
cabal run spock-logging-example
curl -i http://localhost:8081/
```

The generated 128-bit request ID appears in `X-Request-Id`, `getRequestId`, and
all events for that request. `runInContext`, route fallthrough, custom error
handlers, and responses from registered WAI middleware preserve correlation.
When logging is disabled, `getRequestId` returns `Nothing` and `logMessage`
does nothing. `newRequestLogger`, `requestLoggingMiddleware`, and
`lookupRequestLogger` also support standalone WAI integration.

Incoming IDs are ignored by default. Behind a trusted proxy, explicitly set
`lc_trustIncomingRequestId = True`. Only a single value of 1-128 ASCII letters,
digits, dots, underscores, or hyphens is accepted. Invalid values are replaced.
`lc_requestIdHeader` and `lc_generateRequestId` can be customized.

Access events include the response status and monotonic elapsed microseconds
until the response is available, excluding streaming-body transmission. Errors
that escape a WAI application are logged and rethrown; an access event requires
an actual response. Request context includes method and raw path, but no query
string, request body, cookies, or headers. User fields are nested under `fields`
in JSON, so they cannot overwrite request metadata.

Synchronous sink failures invoke `lc_logFailure` (a generic stderr message by
default) without changing the response. Asynchronous cancellation propagates.
Keep sinks and failure callbacks quick; queue events in your logger if needed.
Code constructing `SpockConfig` or `SpockCfg` directly must add the new logging
field; using the default configuration helpers avoids this migration.
