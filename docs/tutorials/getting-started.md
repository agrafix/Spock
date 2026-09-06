---
layout: page
title: "Getting Started"
date: 2016-08-26 08:36:06
author: Alexander Thiemann
permalink: /tutorials/getting-started
redirect_from:
  - /tutorial
  - /tutorial/
---

## Setup

Install GHC 9.14.1 and Stack 3.11.1 with [GHCup](https://www.haskell.org/ghcup/).
This guide uses Stack's **Cabal-only `simple` template**:

```sh
stack new spock-example simple --no-init
cd spock-example
```

The explicit template matters: Stack's default template uses Hpack and
`package.yaml`. In an existing Hpack project, edit `dependencies` in
`package.yaml`; the `.cabal` file is generated. In this tutorial there is no
`package.yaml`, so edit the `.cabal` file directly. See Stack's
[project template documentation](https://docs.haskellstack.org/en/stable/commands/new_command/).

## Dependencies

In `spock-example.cabal`, replace `build-depends` under
`executable spock-example` with:

<!-- setup:dependencies -->
```cabal
  build-depends: base >= 4.12 && < 5, Spock >= 0.16 && < 0.17, text, transformers
```

Add `-threaded` to the executable's `ghc-options`; Warp requires the threaded
runtime to serve requests.

Create `stack.yaml` with the following tested package set and Spock source
revision. This uses the GHC 9.14-compatible packages from the repository:

{% highlight yaml %}
{% include tutorial-stack.yaml %}
{% endhighlight %}

Run `stack build --fast --pedantic`. Stack creates `stack.yaml.lock`; commit it
with your project so later builds use the same resolved dependencies.

## Hello world

Now it is time to write some Haskell code. Open `src/Main.hs` in your favorite editor and replace the content with:

{% highlight haskell %}
{% raw %}{-# LANGUAGE OverloadedStrings #-}{% endraw %}
module Main (main) where

import Web.Spock
import Web.Spock.Config

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Text as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
{% endhighlight %}

Next, run `stack build --fast --pedantic` again to build the project. `stack exec spock-example` should start the executable - you may now point your browser to `http://localhost:8080` and `http://localhost:8080/hello/[YOUR_NAME]`.

## Code explained

Let us take a quick look at the interesting parts of the example. First, we import the core Spock modules:
{% highlight haskell %}
import Web.Spock
import Web.Spock.Config
{% endhighlight %}

Then, in the `main` function, we start out by configuring Spock. To do that, we need to describe what an
empty session for an individual user will look like (in our case, the `EmptySession` from our `MySession` type),
if and how we would like to use a database (no database, for now, so `PCNoDatabase`) and how our initial global
application state will look like. This is very useful to pass around configuration or other globally shared
information. We'll use it to implement a small hit counter by putting an `IORef Int` in our state. Let's not worry
about the last line in `main` just now, and move on to `app`.

The definition of a Spock application lives in the `SpockM conn sess st a` monad. The `conn` type parameter describes
what our database connection looks like (`()` for no database), the `sess` is the type of our session and `st` the
type of our global application state. Thus, for us: `SpockM () MySession MyAppState ()`. Inside the `SpockM` monad,
we'll wire URLs to actions. You can think of it as a [`Writer` monad](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Writer-Lazy.html).
To connect an URL to an action, we use *routes*. A route is either

- a static route piece, such as `"hello"` or `"blog"` or `root` (the `/` route)
- a route parameter: `var`
- or a combination of the above, using the `<//>` operator, such as `"hello" <//> var`.

A static route piece will match the exact text counterpart, eg. a route `"hello" <//> "world"` would match the URL
`/hello/world` and `/hello/world/`. A route parameter matches anything that does not contain a `/` and parses as the
inferred type of the parameter. The type of the parameter is inferred by the action bound to the route. In the example above, our action is:
{% highlight haskell %}
\name -> text ("Hello " <> name <> "!")
{% endhighlight %}
This lambda function takes a `Text`, integrates it between `"Hello "` and `"!"`, and returns it to the framework. Thus,
the `var` in the route `"hello" <//> var` will require the parameter to be a text. If our function would take more than one argument, or our route would not contain a parameter this would result in a type error.

Putting it all together, we first need an HTTP-Verb to match against. In the example, we match `GET` requests, so we'll use the `get` function to
wire our action. Next we specify the route (e.g. `root`), and then the handler (e.g. `text "Hello world!"`). Handlers run in the `SpockAction conn sess st a` monad. You can use various functions to read headers and the HTTP body, and return content to the browser. Note that after a function returning content to the browser is called (such as `text`), the action is terminated.

After having defined the application, you can create a [`Wai.Middleware`](https://hackage.haskell.org/package/wai) from it using `spock spockCfg app` and then run it using `runSpock 8080` (choosing any port you like). Internally, the application is run by the [warp](https://hackage.haskell.org/package/warp) web server.

## Next Steps

- Learn about [Spock's monads and type parameters](monads)
- Read the [FAQ on routing, sessions, and CSRF](/faq/)
- Follow the [browser security guide](security) for login, forms, JSON, and logout
- Read the [API reference](/reference/)
- Check out and/or contribute to the [example project](https://github.com/agrafix/funblog)
- Contribute on [Github](https://github.com/agrafix/Spock) by reporting or pull-requesting missing features and bugs
- Check out the *Addons* and *Works great with* section in the pages footer
- Watch our [blog]({{ "/blog/" | prepend: site.baseurl }})
- And of course: build your Spock powered application
