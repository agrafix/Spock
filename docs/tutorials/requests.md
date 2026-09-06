---
layout: page
title: "Request cookbook"
permalink: /tutorials/requests
---

These recipes cover common tasks after [Getting Started](getting-started).
Every Haskell snippet below comes from the compiled
[cookbook application](https://github.com/agrafix/Spock/tree/master/examples/cookbook).
Run it from the repository root:

```sh
cabal run spock-cookbook-example -- 8080
cabal test spock-cookbook-example --test-show-details=direct
```

This loopback example is a public stateless service with sessions disabled.
For authenticated forms, JSON requests, and logout, use the
[browser security guide](security). The [REST tutorial](rest-api) covers
Persistent and SQLite, and the [rendering guide](rendering) covers HTML.

## Request and response headers

`header` reads a request header; `setHeader` prepares a response header.
Set response metadata before sending the body, because `json`, `text`, and
other response helpers finish the action.

{% highlight haskell %}
{% include examples/cookbook-headers.hs %}
{% endhighlight %}

```sh
curl -i -H 'X-Client: example' http://localhost:8080/headers
```

The JSON contains the incoming value and a request ID. `X-Reply` and
`X-Request-Id` are response headers. Treat client headers as input; a header
value is not proof of identity.

## JSON and form bodies

`jsonBody` returns `Nothing` when decoding fails, which lets this route choose
its error representation:

{% highlight haskell %}
{% include examples/cookbook-json.hs %}
{% endhighlight %}

```sh
curl -i -H 'Content-Type: application/json' -d '"hello"' http://localhost:8080/json
curl -i -H 'Content-Type: application/json' -d '{}' http://localhost:8080/json
```

The first request returns 200; the second returns a JSON 400. `jsonBody'`
instead sends its own 400 response on failure. A custom generic error handler
does not replace responses explicitly sent by a route or helper.

For form-only fields, use `paramsPost` and decide how to handle duplicates:

{% highlight haskell %}
{% include examples/cookbook-form.hs %}
{% endhighlight %}

```sh
curl -i --data-urlencode 'name=Alex+Spock' http://localhost:8080/form
curl -i -d 'name=Alex&name=Spock' http://localhost:8080/form
```

`paramsGet` reads the query string. `params` combines query parameters before
form parameters, so `param` finds a query value first when names collide.
Route captures are separate arguments passed to the handler. Choose the
appropriate source explicitly for credentials or other sensitive fields.

## Status codes and errors

An error object under status 200 still looks successful to an HTTP client.
Set the status first:

{% highlight haskell %}
{% include examples/cookbook-errors.hs %}
{% endhighlight %}

The generic signature lets this helper run in both a full Spock action and
`spc_errorHandler`'s `ActionCtxT () IO`. That handler receives a `Status`, not
an exception, a session, or application state. Return a generic JSON response
there and send diagnostic context to the logging sink.

The example returns JSON 404 for an unknown route and JSON 500 for its
intentional `/failure` route. The latter's diagnostic message appears in the
structured error log, not the response body. Its request-size limit produces
JSON 413 when a handler consumes more than 1 MiB; unused request bodies are
not eagerly read just to enforce the limit.

## Middleware and CORS

Spock accepts ordinary WAI middleware. Middleware can change request or response
metadata and can answer a request before routing, as CORS preflight handling does.

{% highlight haskell %}
{% include examples/cookbook-middleware.hs %}
{% endhighlight %}

The first middleware adds `X-Cookbook` even when no route matches. The second
uses [`wai-cors`](https://github.com/larskuhtz/wai-cors) to allow one development
origin without credentials. For example:

```sh
curl -i -X OPTIONS -H 'Origin: http://localhost:3000' \
  -H 'Access-Control-Request-Method: POST' \
  -H 'Access-Control-Request-Headers: content-type' http://localhost:8080/json
```

CORS controls browser access to responses; it is not authentication, and
non-browser clients can call these public routes. Configure the origins,
methods, and headers your client actually needs. Middleware may send its own
error responses outside Spock's JSON error handler. See the
[security guide](security) before enabling credentialed cross-origin access.

## Repeated file uploads

`filesMulti` returns every file under a field name. `files` retains only one
file per field. The temporary paths belong to Spock and are cleaned up after
the request, so consume or copy the data inside the action:

{% highlight haskell %}
{% include examples/cookbook-upload.hs %}
{% endhighlight %}

```sh
curl -i -F 'upload=@first.txt' -F 'upload=@second.txt' http://localhost:8080/upload
```

The example returns both files' names and byte counts. It never uses the
client-supplied filename as a server path. For stored uploads, choose your own
destination name, enforce size and content policies, and keep uploaded content
away from executable paths. Copy to owned storage before starting background
work; a temporary path is not durable storage.

## Structured logging and request IDs

The example supplies a `LogEvent -> IO ()` sink and enables it in configuration:

{% highlight haskell %}
{% include examples/cookbook-configuration.hs %}
{% endhighlight %}

Its executable encodes each event as one JSON line. Access, error, and explicit
`logMessage` events share a request ID, also returned in `X-Request-Id`.
The default request context contains the method and raw path, excluding query
strings, headers, and bodies. Do not put secrets in paths or custom log fields.
Incoming request IDs are ignored by default; enable trust only at a controlled
proxy boundary.

`spc_errorHandler` chooses the response; logging records diagnostics. This
example uses the structured sink for those diagnostics and disables the
separate legacy `spc_logError` callback to avoid duplicate messages.
See [Web.Spock.Logging](/reference/Spock-core-0.15.0.2/Web-Spock-Logging.html)
for sink behavior and configuration options.
