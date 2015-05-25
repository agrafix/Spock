---
layout: page
title: "Tutorial"
date: 2015-03-27 12:36:06
author: Alexander Thiemann
permalink: /tutorial/
---

## Setup

Before using Spock, you'll need to install the Haskell tool chain on your machine. For more
information on installing it, check the [guide on Stackage.org](http://www.stackage.org/install).
Next, you can prepare a directory for your first Spock powered application:

1. Create a new directory for your project and switch into it
1. Create a new directory `src/`
1. Run `cabal sandbox init && cabal update && cabal init` to create a new cabal project
1. Answer the questions prompted
	- when asked *What does the package build* select *Executable (2)*
	- when asked about the source directory, choose *src (2)*

## Hello world

Now it is time to write some Haskell code. We start by creating a new file `src/Main.hs` and adding
the following contents:

{% highlight haskell %}
{% raw %}{-# LANGUAGE OverloadedStrings #-}{% endraw %}
module Main where

import Data.Monoid
import Web.Spock.Safe

main :: IO ()
main =
    runSpock 8080 $ spockT id $
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           text ("Hello " <> name <> "!")
{% endhighlight %}

To run the example, we will have to add some dependencies to our [Cabal file](https://www.haskell.org/cabal/users-guide/developing-packages.html). It is located in our project
root, called `YOUR_PROJECT.cabal`. Open the file and add the following dependencies to `build-depends`:
{% highlight text %}
Spock >=0.7.9
{% endhighlight %}

Next, run `cabal install --only-dependencies && cabal configure` to install the dependencies and
`cabal build` to build the project. `cabal run` should start the executable. You may now point your browser to `http://localhost:8080` and `http://localhost:8080/hello/[YOUR_NAME]`.

## Code explained

Let us take a quick look at the interesting parts of the example. First, we import the core Spock module
{% highlight haskell %}
import Web.Spock.Safe
{% endhighlight %}
You might wounder about the `.Safe` - there's also a counterpart called `.Simple`. The key difference is
in the way actions are mapped to URLs. The `.Safe`-Module allows us to define this mapping in a type safe way,
while the `.Simple` defers these checks to runtime. We recommend using the `.Safe` module, because it eliminates
many bugs at compile time and makes code much easier to follow.

In the `main` function, we define a new Spock application using the `spockT` function. The second argument
is for initializing the underlying monad of the `SpockT` and `ActionT` transformer. As we'll just use `IO` for
now, and the `main` function already runs in `IO`, the `id` function will do the job (`a -> a`).

Inside the `spockT` function, we'll wire URLs to actions. This is done using a monadic approach - you can think
of the `SpockT` monad as a [`Writer` monad](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Writer-Lazy.html). To connect an URL to an action, we use *routes*. A route is either

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
wire our action. Next we specify the route (e.g. `root`), and then the handler (e.g. `text "Hello world!"`). Handlers run in the `ActionT` transformer monad. You can use various functions to read headers and the HTTP body, and return content to the browser. Note that after a function returning content to the browser is called (such as `text`), the action is terminated.

After having defined the application, you can run it using `runSpock 8080` (choosing any port you like). This essentially is just a shortcut for converting the Spock application (which is represented as a [wai](https://hackage.haskell.org/package/wai) middleware) to a [wai](https://hackage.haskell.org/package/wai) application and then running it using the [warp](https://hackage.haskell.org/package/warp) web server.

## Next Steps

- Read the documentation on [Hackage](https://hackage.haskell.org/package/Spock)
- Check out and/or contribute to the [example project](https://github.com/agrafix/funblog)
- Contribute on [Github](https://github.com/agrafix/Spock) by reporting or pull-requesting missing features and bugs
- Check out the *Addons* and *Works great with* section in the pages footer
- Watch our [blog]({{ "/blog/" | prepend: site.baseurl }})
- And of course: build your Spock powered application
