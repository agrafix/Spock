---
layout: post
title:  "Type-safe routing in Spock"
date:   2015-04-19 17:10:00
author: Tim Baumann
---

Since version 0.7 Spock features a new routing system that enables more type-safe code while still being relatively simple and lightweight.

For backwards compatibility, Spock still supports old-style routing. Users can choose to use it by importing `Web.Spock.Simple` or to use the new one by going with `Web.Spock.Safe`.

In this post, I will show how to declare routes using the new routing system and discuss its advantages. For comparison, here's an example site implemented with old-style routing:

{% highlight haskell %}
{% raw %}{-# LANGUAGE OverloadedStrings #-}{% endraw %}

module Main where

import Web.Spock.Simple
import Data.Monoid ((<>))
import Data.Text (pack)

main :: IO ()
main = 
  runSpock 8080 $ spockT id $ do
    get "/" $
      html "<a href='/calculator/313/+/3'>Calculate 313 + 3</a>"
    get ("hello" <//> ":name") $ do
      name <- param' "name"
      text $ "Hello " <> name <> "!"
    get ("calculator" <//> ":a" <//> "+" <//> ":b") $ do
      a <- param' "a"
      b <- param' "b"
      text $ pack $ show (a + b :: Int)
{% endhighlight %}

This example contains three pages:

* `/hello/simon` – greets Simon
* `/calculator/313/+/3` – 316
* `/` – links to the calculator page above

The first problem with this code is that handling of parameters in URLs is too repetitive. Declaring a parameter requires one to choose an identifier, which you must later repeat to get the value of the argument with `param'`. Mistyping the identifier of a parameter results is a mistake which is not caught by the type checker.

The new routing system solves this issue:

{% highlight haskell %}
{% raw %}{-# LANGUAGE OverloadedStrings #-}{% endraw %}

module Main where

import Web.Spock.Safe
import Data.Monoid ((<>))
import Data.Text (pack)

main :: IO ()
main = 
  runSpock 8080 $ spockT id $ do
    get root $
      html "<a href='/calculator/313/+/3'>Calculate 313 + 3</a>"
    get ("hello" <//> var) $ \name ->
      text $ "Hello " <> name <> "!"
    get ("calculator" <//> var <//> "+" <//> var) $ \a b ->
      text $ pack $ show (a + b :: Int)
{% endhighlight %}

In this example, the position of parameters in paths are declared with `var`. When a route is matched, the parsed parameter values are passed as Haskell arguments to the handler function in the order in which they occur in the route.

But there's still something wrong with this code. Let's say, I've decided that I like prefix notation better and renamed `/calculator/313/+/3` to `calculator/plus/313/3`. But, of course, I forgot to update the target of the link! So now the visitors of my site are getting 404s and writing me lots of emails. Not good.

The solution to this problem are *bidirectional* routes, which can be used both for matching requests and for generating URLs. We can then declare routes only once, which ensures that links never get out of sync with the routing code. In Spock, the function `renderRoute` is used to instantiate a route with some parameters:

{% highlight haskell %}
{% raw %}{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}{% endraw %}

module Main where

import Web.Spock.Safe
import Data.Monoid
import Data.Text (pack, Text)

helloR :: Path '[Text]
helloR = "hello" <//> var

addR :: Path '[Int, Int]
addR = "calculator" <//> var <//> "+" <//> var

main :: IO ()
main =
  runSpock 8080 $ spockT id $ do
    get root $
      html $ "<a href='" <> renderRoute addR 313 3 <> "'>Calculate 313 + 3</a>"
    get helloR $ \name ->
      text $ "Hello " <> name <> "!"
    get addR $ \a b ->
      text $ pack $ show (a + b)
{% endhighlight %}


## How it works

The datatype of routes in Spock is indexed over the list of parameter types: `Path :: [*] -> *`. Here's how values of this type are constructed:

* The **empty path** is `root :: Path '[]`.
* For creating **static path segments** there is `static :: String -> Path '[]`. With `OverloadedStrings` string literals can be used: `"blog"` instead of `static "blog"`.
* **Parameter path segments** are created with `var :: (Typeable a, PathPiece a) => Path '[a]`. The parameter type `a` must instantiate [PathPiece](https://hackage.haskell.org/package/path-pieces-0.2.0/docs/Web-PathPieces.html) for serialization and deserialization. For implementation reasons, `Typeable a` is also required (which is automatically generated for all data types [in GHC >= 7.10](https://mail.haskell.org/pipermail/haskell-cafe/2015-March/118817.html)). 
* Paths can be **concatenated** with `<//> :: Path as -> Path bs -> Path (Append as bs)` (`Append` is `++` for type-level lists).

Route handlers for `GET` requests can be installed with

{% highlight haskell %}
get :: MonadIO m => Path as -> HVectElim as (ActionT m ()) -> SpockT m ()
{% endhighlight %}

There are also `post`, `delete`, `put`, `head`, `patch` functions corresponding to other HTTP methods. `HVectElim as x` is the type of a function that takes as arguments values of the types `as :: [*]` and produces an `x :: *`:

{% highlight haskell %}
HVectElim '[a,b,c,d] x ≡ a -> (b -> (c -> (d -> x)))
{% endhighlight %}

Finally, routes can be serialized with

{% highlight haskell %}
renderRoute :: Path as -> HVectElim as Text
{% endhighlight %}

Internally, all route handlers are stored in a tree-like structure which can be efficiently queried for matches.

## A note on type-checking

One small disadvantage of the new routing system is that code won't compile before all parameters are used in a handler. For example

{% highlight haskell %}
    get ("calculator" <//> var <//> "+" <//> var) $ \a b ->
      text "Haven't finished writing this handler!"
{% endhighlight %}

doesn't type check because the compiler can't infer a concrete type for the parameters `a` and `b`. One way to solve this is to use `ScopedTypeVariables`:

{% highlight haskell %}
    get ("calculator" <//> var <//> "+" <//> var) $ \(a :: Int) (b :: Int) ->
      text "Haven't finished writing this handler!"
{% endhighlight %}

Another possibility is to provide an explicit type annotation for the route (requires `DataKinds`):

{% highlight haskell %}
    get ("calculator" <//> var <//> "+" <//> var :: Path '[Int, Int]) $ \a b ->
      text "Haven't finished writing this handler!"
{% endhighlight %}

Finally, one can use the type synonym `Var a` for `Route '[a]` to annotate parameter segments:

{% highlight haskell %}
    get ("calculator" <//> (var :: Var Int) <//> "+" <//> (var :: Var Int)) $ \a b ->
      text "Haven't finished writing this handler!"
{% endhighlight %}

## Discussion

Many routing libraries are concerned with parsing and serializing URLs to an application-specific datatype of routes. For example, with 
[web-routes-boomerang](https://ocharles.org.uk/blog/posts/2013-12-20-24-days-of-hackage-web-routes-boomerang.html) one would define the following data type for the example site:

{% highlight haskell %}
data Example = Root
             | Hello Text
             | Add Int Int
             deriving (Eq, Show)
{% endhighlight %}

In [Yesod](http://www.yesodweb.com/book/routing-and-handlers), this data type and all corresponding functions are generated with Template Haskell:

{% highlight haskell %}
mkYesod "Example" [parseRoutes|
  / RootR GET
  /hello/#Text HelloR GET
  /calculator/#Int/+/#Int AddR GET
|]
{% endhighlight %}

In the case of `web-routes-boomerang`, the user has to define a reversible parsing function using the boomerang DSL. This approach is more flexible than Yesod's or Spock's. For [example](http://hackage.haskell.org/package/web-routes-boomerang-0.28.3/docs/Web-Routes-Boomerang.html), suppose one has defined

{% highlight haskell %}
data Sitemap = ... | Article Int String
{% endhighlight %}

It is totally possible to map this constructor to the route `/article/int-string` with `web-routes-boomerang`. In Yesod or Spock, each parameter position is delimited by a slash and one would have to define

{% highlight haskell %}
data ArticleIdentifier = ArtId Int String
{% endhighlight %}

and provide an instance of `PathPiece ArticleIdentifier` to get the same result. This in turn would require a change of the routes data type.

Another advantage of data type based routing systems is that the totality checker can ensure that every declared route is handled by the application.

## Conclusion

In large applications, data type based routing has some benefits. In comparison to the old system, Spock's new routing system leads to more concise and type safe code. It doesn't require enabling tons of Haskell extensions, learning a new syntax or a DSL for writing bidirectional parsers. Because of its small API, it fits Spock's goal to be a framework that users can get started with very quickly.

Type-safe routing is implemented in the [reroute](http://hackage.haskell.org/package/reroute) package and can be used independently of Spock.

## Meta

* Join the discussion on [Reddit](http://www.reddit.com/r/haskell/comments/334rr0/typesafe_routing_in_spock_explained/) or [Hacker News](https://news.ycombinator.com/item?id=9403476)
* Visit Tim Baumann on [GitHub](https://github.com/timjb)
