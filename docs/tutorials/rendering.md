---
layout: page
title: "Rendering HTML Content with Examples for Lucid and Blaze"
date: 2018-10-19 15:00:00
author: Steven Williams
permalink: /tutorials/rendering
---

* TOC
{:toc}

# Overview

This tutorial will cover how to incorporate templating systems into your Spock applications.
Examples will make use of Lucid and Blaze. By the end of this article you should be able to
make use of any HTML templating system of your choice.

To view the full code for this tutorial you may go [here.](https://github.com/flounders/spock-examples/tree/master/rendering)

# Project Setup

It is recommended to use [Haskell Stack](https://haskellstack.org) to build this tutorial.

To get started:

1. Create a new project with this command: `stack new Spock-rendering simple`
2. Next we move into the project: `cd Spock-rendering`

Side note: As of this writing stack has a bug with not copying the project name into
the cabal file's name. This can be remedied by renaming the cabal file from `{{name}}.cabal`
to `Spock-rendering.cabal`. Hopefully this issue is fixed soon.

## Dependencies

We will be using Spock of course, text and the blaze-html package. You will want to these to the build
depends list in `Spock-rendering.cabal` like so:

```
build-depends:       base >= 4.7 && < 5
                   , Spock >= 0.13
                   , blaze-html
                   , text
```

We should be ready to start making changes to our source file now.

# Example Code

Our first order of business is opening `src/Main.hs` in your text editor of choice.

## Imports and Language Pragmas

You will want the following at the top of `src/Main.hs`

{% highlight haskell %}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (toStrict)
import Web.Spock
import Web.Spock.Config

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R

{% endhighlight %}

`OverloadedStrings` is probably familar to you by now. If not, it makes writing string literals for
the other string type alternatives easier. This language pragma does not automatically convert strings
in our function calls however, and this is why we need `Data.Text.Lazy`.

As I said for this tutorial we are going to be using `blaze-html`. The one thing I wish to point out
is that there are four different rendering options the library provides, and `Text.Blaze.Html.Renderer.Text`
is not the only one. Otherwise, the other imports should be self explanatory.

## Type Definitions

Our small example doesn't really need these type definitions, but it is a good practice to get into, especially
if you have several actions in your application.

{% highlight haskell %}

type AppM = SpockM AppDb AppSession AppState ()
type AppAction ctx a = SpockActionCtx ctx AppDb AppSession AppState a

type AppDb = ()
type AppState = ()
type AppSession = ()

{% endhighlight %}

If you ever want to change the database you are using, or the type for application state, this is your one stop.

## Sample HTML Template

{% highlight haskell %}

hello :: H.Html
hello = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello from Blaze and Spock"
  H.body $ do
    H.p "Hello world! Powered by Blaze and Spock."

{% endhighlight %}

Since we are just looking at how to render the HTML that Blaze will generate for us, we are going to keep the
example simple.

## Rendering the Template

{% highlight haskell %}

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock spockCfg app)

app :: AppM
app = get root $ do
  html . toStrict $ R.renderHtml hello

{% endhighlight %}

Let us take a look at `app` first. Just to refresh `get root` specifies that for an HTTP GET request to `/` we
will run the action that is specified after `do`. The type of this action is going to be `AppAction ctx a`.
The call to `renderHtml` turns our `hello` HTML into lazy `Text`.  The `html` function will return this
HTML appropriately to the requester. However, it wants strict `Text`; which is why we have a call to `toStrict`.
Any time someone makes a `GET` request for `/`, `html` will provide the rendered form of our Blaze page structure.

`main` will setup the configuration for `Spock` to run the `app`. Since we are not using a database, state or session,
we will go with a default configuration using `defaultSpockCfg` and the arguments `() PCNoDatabase ()`. The next line
will run our application on port 3000 for us. Visit [http://localhost:3000](http://localhost:3000) to see our Blaze
version of "Hello, world!"

# Going a Little Deeper

When we import `Web.Spock`, it makes the `html` function available to us. This isn't the only method for generating
responses. Some others include `text`, `json`, and `file`. The main difference between each of these is they set
different values for the content type in the HTTP header that we are responding with. This is accomplished by the
`setHeaderUnsafe` function in the library and `setHeader` in Spock-lucid. The response is then sent by either `bytes`
for `text` and `html`, or `response` in the case of `file`.

We can make our responses more efficient. Both `text` and `html` end up setting the content type and use `bytes`
to return a response. These functions come from
[Web.Spock.Internal.CoreAction](https://hackage.haskell.org/package/Spock-core-0.13.0.0/docs/src/Web.Spock.Internal.CoreAction.html#html).
Before we take a look at these functions, do keep in mind this is an internal source file which is subject to
change at any time, so please consult the documentation and source code for the version you are using at all times.

{% highlight haskell %}

text :: MonadIO m => T.Text -> ActionCtxT ctx m a
text val =
    do setHeaderUnsafe "Content-Type" "text/plain; charset=utf-8"
       bytes $ T.encodeUtf8 val
...

html :: MonadIO m => T.Text -> ActionCtxT ctx m a
html val =
    do setHeaderUnsafe "Content-Type" "text/html; charset=utf-8"
       bytes $ T.encodeUtf8 val

{% endhighlight %}

As you can see the only difference between these is that they set a different content type in the header. Both
functions convert the `Text` that they receive to a strict `ByteString`. Since we have a renderer available for
`ByteString`, we can eliminate two conversions that happen in our example code above. The first is the conversion
to strict `Text` from lazy `Text`. `html` then converts this strict `Text` to a strict `ByteString`. Is that the
end of the conversion process? No, because `bytes` makes use of another function called `lazyBytes`. Let's have a
look at those now.

{% highlight haskell %}

bytes :: MonadIO m => BS.ByteString -> ActionCtxT ctx m a
bytes val =
    lazyBytes $ BSL.fromStrict val
...

lazyBytes :: MonadIO m => BSL.ByteString -> ActionCtxT ctx m a
lazyBytes val =
    response $ \status headers -> Wai.responseLBS status headers val

{% endhighlight %}

As you can see `bytes` makes a call to convert our strict `ByteString` from `html` to a lazy `ByteString` and then
passes it to `lazyBytes`. We can skip these conversions entirely by changing the revelant sections of our example.

{% highlight haskell %}

import qualified Text.Blaze.Html.Renderer.Utf8 as R

app = get root $ do
  setHeader "Content-Type" "text/html; charset=utf-8"
  lazyBytes $ renderHtml hello

{% endhighlight %}

As I warned earlier, this could all be possibly changed. It does help reduce some unnecessary function calls.
`ghci` was the greatest help to me for tracking down where these functions that I couldn't find in documentation.
Not only did I find out the types, but I was able to find the definitions too.

# Wrapping Up

To render content from `blaze-html`, `aeson`, or any other library with a similar purpose, `setHeader` and `lazyBytes`
or `response` should be what you need. If efficiency isn't an immediate concern, you can use `text`, `html`, `json` or
`file`.
