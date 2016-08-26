---
layout: page
title: "Tutorial"
date: 2016-08-26 08:36:06
author: Alexander Thiemann
permalink: /tutorial/
---

## Setup

Before using Spock, you'll need to install the Haskell toolchain on your machine. We recommend using the
[stack](http://haskellstack.org/) tool to quickly get started! Our guide will be `stack` based, but you can
easily translate this to `cabal`.
Next, you can prepare a directory for your first Spock powered application:

1. Create a new project using `stack new Spock-example`
1. Jump into the directory `cd Spock-example`

## Dependencies

To make sure your dependencies will match those of this tutorial, replace the content of `stack.yaml` with:

{% highlight yaml %}
resolver: lts-6.13
packages:
- '.'
- location:
      git: https://github.com/agrafix/Spock.git
      commit: 77333a2de5dea0dc8eba9432ab16864e93e5d70e
  subdirs:
    - Spock
    - Spock-core
    - reroute
extra-deps: []
{% endhighlight %}

Now we will add `Spock` to our dependencies by opening `Spock-example.cabal` and adding `Spock >=0.11`, `mtl` and `text` to `build-depends` in the
`executable Spock-example-exe` section.
Next we build everything once: `stack build --fast --pedantic`.

## Hello world

Now it is time to write some Haskell code. Open `app/Main.hs` in your favorite editor and replace the content with:

{% highlight haskell %}
{% raw %}{-# LANGUAGE OverloadedStrings #-}{% endraw %}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
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

Next, run `stack build --fast --pedantic` again to build the project. `stack exec Spock-example-exe` should start the executable - you may now point your browser to `http://localhost:8080` and `http://localhost:8080/hello/[YOUR_NAME]`.

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
what are database connection looks like (`()` for no database), the `sess` is the type of our session and `st` the
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

- Read the documentation on [Hackage](https://hackage.haskell.org/package/Spock)
- Check out and/or contribute to the [example project](https://github.com/agrafix/funblog)
- Contribute on [Github](https://github.com/agrafix/Spock) by reporting or pull-requesting missing features and bugs
- Check out the *Addons* and *Works great with* section in the pages footer
- Watch our [blog]({{ "/blog/" | prepend: site.baseurl }})
- And of course: build your Spock powered application
