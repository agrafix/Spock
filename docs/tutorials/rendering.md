---
layout: page
title: "Rendering HTML with Blaze and Lucid"
date: 2018-10-19 15:00:00
author: Steven Williams
permalink: /tutorials/rendering
---

Spock accepts HTML from any template library. With Blaze, render to lazy `Text`
and convert it for `html`, or render directly to UTF-8 bytes and send them with
`lazyBytes`. Lucid provides the same choice.

## Run the example

The [complete example and response tests](https://github.com/agrafix/Spock/tree/master/examples/rendering)
are built by the repository's Cabal and Stack CI with GHC 9.14.1. From a checkout:

```sh
cabal run spock-rendering-example
```

Visit `http://localhost:8080/`, `/utf8`, and `/lucid`. Each route accepts a
`message` query parameter, for example `/utf8?message=Hello`.
To run the tests, use `cabal test spock-rendering-example`.

For a separate application, follow the [getting started setup](getting-started)
and add `blaze-html`, `lucid`, and `text` to its dependencies. The example uses
`Spock-core` because rendering does not need sessions or a database. These same
actions are available through `Web.Spock` in a full Spock application.

## Blaze with Text

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import Web.Spock.Core

hello :: H.Html
hello = H.p "Hello, λ!"

app :: SpockT IO ()
app = get root $
  html $ TL.toStrict $ R.renderHtml hello
```

`R.renderHtml` produces lazy `Text`. `html` accepts strict `Text`, encodes it as
UTF-8, and sets `Content-Type: text/html; charset=utf-8`. Calling it finishes the
action; set any status or other headers first.

## Blaze with UTF-8 bytes

Replace the renderer import and action with:

```haskell
import qualified Text.Blaze.Html.Renderer.Utf8 as R

app :: SpockT IO ()
app = get root $ do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes $ R.renderHtml hello
```

The qualified `R.renderHtml` now returns a lazy `ByteString`, so it can go
directly to `lazyBytes`. Set the content type yourself: `lazyBytes` accepts
arbitrary bytes and does not choose a media type. This path avoids converting
through `Text`; it does not require any internal Spock functions.

## Lucid

Lucid's `renderBS` also produces a lazy UTF-8 `ByteString`:

```haskell
import qualified Lucid as L

app :: SpockT IO ()
app = get root $ do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes $ L.renderBS $ L.p_ "Hello, λ!"
```

Alternatively, use `html . TL.toStrict . L.renderText` for the `Text` path.

## Rendering request data

Use Blaze's `H.toHtml` or Lucid's `L.toHtml` to insert text supplied by a user.
They escape HTML characters such as `<` and `&`. The executable's three routes
and tests cover Unicode and escaped request data. Reserve raw/pre-escaped HTML
operations for markup you control; do not concatenate user input into markup.

For JSON, use `json`; for plain text, use `text`. Both set the corresponding
content type. See the [API reference]({{ '/reference/' | relative_url }}) and its **Web.Spock.Action** module
for headers, files, streaming, and other response helpers.
