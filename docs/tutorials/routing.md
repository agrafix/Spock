---
layout: page
title: "Routing"
date: 2021-11-06 08:36:06
author: Alexander Thiemann
permalink: /tutorials/routing
---

This page contains a collection of routing cookbooks.

For custom capture parsers, regular expressions, and wildcards, see the
[routing FAQ](/faq/#can-i-use-regular-expressions-or-custom-types-in-routes)
and its compiled example.

# `var` matching on multiple types

You can use the `AltVar` `Either` alternative to allow a `var` that can match on different types:

```haskell

do get ("hello" <//> var) $ \(v :: AltVar Int T.Text) ->
      case v of
        AvLeft number -> text (T.pack (show (1 + number)))
        AvRight str -> text str

```

In this example, `/hello/1` would show `2`, and `/hello/alex` would return `alex`.


# Slash policies and canonical routes

Spock 0.17 and Spock-core 0.16 add an optional slash policy. The default
`IgnoreSlashes` preserves the earlier behavior: `/foo`, `/foo/`, and `/foo//`
match the same route, and repeated internal slashes are ignored too.

Set `spc_slashPolicy` in a full Spock application, or `sc_slashPolicy` in a
Spock-core application built with `spockConfigT`:

{% highlight haskell %}
cfg <- defaultSpockCfg () PCNoDatabase ()
spock (cfg { spc_slashPolicy = StrictSlashes }) $ do
  get "foo" $ text "file"
  get "foo/" $ text "directory"
{% endhighlight %}

With `StrictSlashes`, these handlers are distinct. Unknown spellings return
404. A leading slash in a route literal is optional; root is still `root` or
`"/"`. Repeated internal slashes are significant: `"a//b"` matches `/a//b`,
and does not match `/a/b`.

Use `trailingSlash` to require a final slash after a typed capture:

{% highlight haskell %}
get (trailingSlash $ "items" <//> (var :: Var Int)) $ \itemId ->
  text $ T.pack $ show itemId
{% endhighlight %}

Here `T` is `Data.Text`. `trailingSlash` leaves root and an existing final
slash unchanged. A wildcard keeps the remainder exactly as decoded, including
internal and trailing slashes. Joining `"items/" <//> var` adds a second
separator before the capture; use `trailingSlash` on the completed path instead.

## Canonical redirects

Set `RedirectTrailingSlashes` and register the canonical spelling:

{% highlight haskell %}
spock (cfg { spc_slashPolicy = RedirectTrailingSlashes }) $
  get "foo/bar/" $ text "directory"
{% endhighlight %}

`GET /foo/bar` now returns **308** with `Location: /foo/bar/`. Relative browser
links such as `baz` resolve below `/foo/bar/`. Register `"foo/bar"` instead to
redirect in the other direction. The router first tries an exact match; it
redirects only if adding or removing one final slash finds a route for that
same HTTP method. Registering both spellings keeps both handlers available.
Matching wildcard and fallback handlers retain control of the request.

A 308 retains the HTTP method and request body when followed, including for
POST, PUT, PATCH, and DELETE. The redirect does not run the target action or
its prehooks; authentication and CSRF checks run when the client requests the
target. Registered middleware still wraps the redirect. The raw percent-encoded
path and query string are preserved. Root stays `/`, internal slashes are not
normalized, and scheme-relative or backslash-containing paths are never
redirected automatically.

## Rendering and OpenAPI

Use the same policy when generating links:

{% highlight haskell %}
renderRouteWith StrictSlashes "foo/" == "/foo/"
renderRouteWith StrictSlashes (trailingSlash $ "items" <//> (var :: Var Int)) 42
-- "/items/42/"
{% endhighlight %}

The existing `renderRoute` uses `IgnoreSlashes` for compatibility. As before,
these renderers join URL pieces without percent-encoding their contents.
`Spock-api` also exports `renderRouteWith` (taking an `HVect` of captures).
Generate its schema with `openApiDocumentWith policy title version endpoints`
so documented path spellings match the server; `openApiDocument` retains the
compatibility policy.

When upgrading, use `defaultSpockCfg` / `defaultSpockConfig` and record updates.
Code that constructs configuration records directly must initialize the new
policy field. The underlying `reroute` registry has `runRegistryWith`; its
existing `runRegistry` also preserves the compatibility policy.

# File extensions in typed routes

Spock 0.17.1, Spock-core 0.16.1, and reroute 0.9 support `<.>` for joining
parts of one path segment. Fixed extensions keep the underlying capture type:

{% highlight haskell %}
get ("values" <//> var <//> "pages" <//> var <.> "txt") $
  \(key :: T.Text) (page :: Int) -> json (key, page)
{% endhighlight %}

With `OverloadedStrings` and `ScopedTypeVariables`, this matches
`/values/example/pages/12.txt` and passes `"example"` and `12` to the handler.
A missing `.txt`, a different extension, or a non-integer page produces no match.
A static route such as `"report" <.> "txt"` works too.

Both sides can capture a value. Give the extension its own type to constrain
which formats the endpoint accepts:

{% highlight haskell %}
-- Also import Web.HttpApiData.
data Format = Html | Txt deriving Show

instance FromHttpApiData Format where
  parseUrlPiece "html" = Right Html
  parseUrlPiece "txt" = Right Txt
  parseUrlPiece _ = Left "Expected html or txt"

instance ToHttpApiData Format where
  toUrlPiece Html = "html"
  toUrlPiece Txt = "txt"

-- Inside the route-registration block:
get ("pages" <//> var <.> var) $ \(page :: Int) (format :: Format) ->
  json (page, show format)
{% endhighlight %}

Here `/pages/12.html` supplies `12` and `Html`, and `/pages/12.xml` does not
match. A `Text` extension instead accepts any text, including an empty string
after the dot, following its `FromHttpApiData` instance.

The matcher tries dots from right to left and chooses the first split whose
literals and typed parsers succeed. Thus a text basename in `var <.> "txt"`
keeps all of `report.v2.final` from `report.v2.final.txt`. Fixed multi-dot
extensions such as `var <.> "tar.gz"` also work. For captured multi-dot
extensions, use a custom parser that recognizes the complete extension;
with two unrestricted text captures, the rightmost dot is the separator.
Percent-encoded dots are decoded before matching and follow the same rule.

You can chain extensions (`var <.> var <.> "gz"`) and append further route
segments or a wildcard. `<.>` joins the last segment on its left to the first
on its right; `<//>` introduces a slash. Handler arguments follow the captures
from left to right, including captures before and after the extension.

Static routes take priority, followed by extension patterns, ordinary captures,
and wildcards. Among competing extension patterns, more literal characters
win, so `.txt` beats a general extension capture. Equal-priority patterns keep
Spock's existing last-registration-first order and support `jumpNext`.

## Render complete URLs

Use `renderRouteEncoded` for links containing arbitrary captured values:

{% highlight haskell %}
renderRouteEncoded ("pages" <//> (var :: Var Int) <.> "txt") 12
-- "/pages/12.txt"
renderRouteEncoded ("files" <//> (var :: Var T.Text) <.> "txt") "a/b?c"
-- "/files/a%2Fb%3Fc.txt"
renderRouteEncoded ("pages" <//> (var :: Var Int) <.> (var :: Var Format)) 12 Html
-- "/pages/12.html"
{% endhighlight %}

Encoding happens after the extension parts form a complete segment. Slashes,
spaces, percent signs, Unicode, and query delimiters in captures stay within
that segment. `renderRouteEncodedWith` additionally takes the slash policy;
combine it with `trailingSlash` when the canonical URL ends in `/`.
The older unencoded renderers remain available for compatibility.

`Spock-api` 0.16 describes these routes using the same endpoint definitions.
Its OpenAPI paths retain literals, for example `/pages/{page}.txt` or
`/pages/{page}.{format}`; provide one `PathParameter` for each capture in order.
Query, header, and body arguments still follow all path captures.

Code that inspects the public `reroute` `Path` constructors directly should
handle `WithExtension` and `AppendPath` when upgrading. The supplied matchers,
renderers, and OpenAPI generator already support both.
