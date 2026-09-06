---
layout: page
title: "Building a REST API"
date: 2017-05-01 10:52:00
author: Bryn Edwards
permalink: /tutorials/rest-api
---

* TOC
{:toc}

# Overview

In this tutorial, we're going to use Spock to build a simple RESTful API
that will let us add people to a database and then retrieve a list of who
we've added. For example:

```
$ curl -H "Content-Type: application/json" -d '{ "name": "Walter", "age": 50 }' localhost:8080/people
{"result":"success","id":1}

$ curl -H "Content-Type: application/json" -d '{ "name": "Jesse", "age": 22 }' localhost:8080/people
{"result":"success","id":2}

$ curl localhost:8080/people
[{"age":50,"name":"Walter","id":1},{"age":22,"name":"Jesse","id":2}]
```

We'll be using [curl](https://curl.se/) to interact with our API so you should have
that or another way to perform HTTP requests. `curl` examples are provided throughout
the tutorial.

The [finished database example and tests](https://github.com/agrafix/Spock/tree/master/examples/rest-api)
are maintained in the Spock repository.

# Project Setup

Install GHC 9.14.1 and Stack 3.11.1 with [GHCup](https://www.haskell.org/ghcup/),
then create a project with the Cabal-only `simple` template:

```sh
stack new spock-rest simple --no-init
cd spock-rest
```

The explicit template ensures that `spock-rest.cabal` owns your dependencies.
Stack's default template uses Hpack: if your existing project contains
`package.yaml`, edit its `dependencies` instead of the generated `.cabal` file.

### Dependencies

Create `stack.yaml` with the same tested package set and source revision used
by the [getting started guide]({{ '/tutorials/getting-started' | relative_url }}):

{% highlight yaml %}
{% include tutorial-stack.yaml %}
{% endhighlight %}

In `spock-rest.cabal`, replace `build-depends` under `executable spock-rest` with:

<!-- setup:dependencies -->
```cabal
  build-depends: base >= 4.12 && < 5, Spock >= 0.16 && < 0.17, aeson, text
```

Add `-threaded` to the executable's `ghc-options`; Warp requires the threaded
runtime to serve requests.

Build with `stack build --fast --pedantic` and commit the generated
`stack.yaml.lock` with your project. Later sections add database dependencies
to this same `.cabal` file.

### Imports

Let's start by adding a couple of language extensions and imports. Open
`src/Main.hs` and replace the content with:

{% highlight haskell %}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson
import           Data.Text        (Text, pack)
import           GHC.Generics

{% endhighlight %}

We'll be using the `DeriveGeneric` extension along with `GHC.Generics`
to create `FromJSON` and `ToJSON` instances of our API type.
The `Data.Aeson` library provides our JSON type conversion; you can view its
[documentation](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html)
to learn more.

# API Type

Now we'll create a data type for our API. Add the following to your `Main.hs`,
below the import statements:

{% highlight haskell %}

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

{% endhighlight %}

We can try out our new Person type in GHCi: run `stack ghci` in your
project root directory to build your project and start a REPL session. Try
using `encode` and `decode` to serialise and deserialise `Person`s into
`ByteString`s like so:

```
λ> encode Person { name = "Leela", age = 25 }
"{\"age\":25,\"name\":\"Leela\"}"

-- So our string literal can inferred as a ByteString
λ> :set -XOverloadedStrings
λ> decode "{ \"name\": \"Amy\", \"age\": 30 }" :: Maybe Person
Just (Person {name = "Amy", age = 30}) 
```

Aeson's `decode` type signature is:

{% highlight haskell %}
FromJSON a => ByteString -> Maybe a
{% endhighlight %}

Give the result a type so GHC knows which parser to use. We can also decode
it as Aeson's generic `Value` type:

```
λ> decode "{ \"name\": \"Amy\", \"age\": 30 }" :: Maybe Value
Just (Object (fromList [("age",Number 30.0),("name",String "Amy")]))
```

# Serving JSON

Now we'll add a basic Spock application to serve our JSON. Add the
following to your `Main.hs`:

{% highlight haskell %}

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "people" $ do
    json $ Person { name = "Fry", age = 25 }

{% endhighlight %}

Our `Api` type represents route registration. In the second
part we'll be modifying it to add a database backend, but for now we'll
leave all the types as Unit. Our `ApiAction` type is similar and represents
actions in our application which are functions performed by route matches
(e.g. `get "people"`). We'll be using `ApiAction` later to explicitly declare
some types used in actions.

Spock includes a `json` function for serving any type that implements the
`ToJSON` typeclass, which means you can pass your `Person` and it will encode
it as JSON and set the HTTP Content-Type header to `application/json` for
you. You can start the server in GHCi by first reloading the project using
the `:reload` command then running your `main` function:

```
λ> :reload
[1 of 1] Compiling Main
Ok, modules loaded: Main.

λ> main
Spock is running on port 8080 
```

Go to [localhost:8080/people](http://localhost:8080/people) and you should
see your `Person` object in JSON.

REST APIs also need to serve lists of items; since `aeson` includes a `ToJSON
a => ToJSON [a]` instance, we can easily serve a list of `Person`s. Change
your `get` function to the following:

{% highlight haskell %}

  get "people" $ do
    json [Person { name = "Fry", age = 25 }, Person { name = "Bender", age = 4 }]

{% endhighlight %}

Enter `ctrl-c` in GHCi to interrupt the server and get back to the prompt. Then,
reload your project and start the server again. Refresh your browser to see
your two `Person`s in a JSON array.

# Parsing JSON

Now we'll write a second route that will attempt to parse a POST body into our
`Person` type. Add the following to the bottom of your `app` declaration:

{% highlight haskell %}

  post "people" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)
      
{% endhighlight %}

Reload your project and start the server again. We'll need to make a POST
request to try out our new code. Here's a way of doing so using `curl`:

```
$ curl -H "Content-Type: application/json" -d '{ "name": "Bart", "age": 10 }' localhost:8080/people
Parsed: Person {name = "Bart", age = 10}
```

You can also try adding extra keys to your JSON object or removing name and
age and seeing what happens.

# Adding a Database

The complete database example uses Persistent 2.18 and SQLite. It keeps the
same JSON fields, adds stable numeric IDs, and returns appropriate HTTP status
codes. The [Persistent book](https://www.yesodweb.com/book/persistent) explains
the schema declaration and generated types in more detail.

From the repository you can run the finished application directly:

```sh
cabal run spock-rest-example --project-file=cabal.project.tutorials --builddir=dist-newstyle-tutorials -- api.db 8080
```

Or continue in the `spock-rest` project created above. Replace its executable
dependencies with:

<!-- database:dependencies -->
```cabal
  build-depends: base >= 4.12 && < 5, Spock >= 0.16 && < 0.17, aeson, text, http-types, monad-logger, persistent >= 2.18.1 && < 2.19, persistent-sqlite >= 2.13.3 && < 2.14, transformers, wai, warp
```

Add `other-modules: People` to that executable stanza. `Database.Persist.TH`
is included in `persistent`; the former `persistent-template` dependency is
unnecessary.

Keep using the `stack.yaml` above: its fixed snapshot includes Persistent and
SQLite. Stack trusts the snapshot's dependency versions, so no `allow-newer`
setting is needed. For Cabal users, the repository's optional
`cabal.project.tutorials` relaxes only Persistent 2.18.1's Aeson and Template
Haskell upper bounds, which predate Aeson 2.3 and GHC 9.14. Both builds are tested.

## The database application

Create `src/People.hs` with this complete module. The code below is copied
automatically from the compiled and tested repository source.

{% highlight haskell %}
{% include examples/People.hs %}
{% endhighlight %}

`PersonInput` describes the incoming JSON; Persistent generates `Person` and
its database field accessors from the schema declaration. Keeping the HTTP
representation explicit avoids coupling clients to generated record names.
Typed route captures parse `Int64` IDs, and `toSqlKey` turns them into database
keys. Invalid captures and missing rows both produce JSON 404 responses.

`withPeopleApp` brackets a one-connection pool, migrates the demo database, and
closes the pool when its caller finishes. `runSQL` borrows a connection through
Spock's `runQuery` and runs a Persistent transaction on it. The update and
delete handlers check existence and modify the row within that transaction.
The example suppresses SQL debug logging, which may contain application data.

Use a new database file while learning. Review migrations and run them once
during deployment before changing a production schema. This example has no
authentication and disables sessions; add the authorization appropriate to
your application before exposing private data. See the
[browser security guide](security) when introducing cookie authentication.

## Start the server

Replace `src/Main.hs` with:

{% highlight haskell %}
{% include examples/PeopleMain.hs %}
{% endhighlight %}

The executable takes a database file and port, and binds to loopback:

```sh
stack build --fast --pedantic
stack exec spock-rest -- api.db 8080
```

Reusing `api.db` after restarting preserves its people. Commit your source and
`stack.yaml.lock`, not the local database file.

## Exercise the API

```sh
curl -i -H 'Content-Type: application/json' \
  -d '{"name":"Walter","age":50}' http://localhost:8080/people
curl -i http://localhost:8080/people
curl -i http://localhost:8080/people/1
curl -i -X PUT -H 'Content-Type: application/json' \
  -d '{"name":"Walter","age":51}' http://localhost:8080/people/1
curl -i -X DELETE http://localhost:8080/people/1
curl -i http://localhost:8080/people/1
```

| Request | Result |
| --- | --- |
| Create a valid person | 201, `Location: /people/1`, and `{"result":"success","id":1}` for the first row in a new database |
| List people or get one | 200 with JSON including `id`, `name`, and `age` |
| Update an existing person | 200 with the updated JSON representation |
| Delete an existing person | 204 with an empty body |
| Malformed JSON, blank name, or invalid age | 400 with a JSON error |
| Missing person or unmatched route | 404 with a JSON error |
| Consumed body larger than 16 KiB | 413 with a JSON error |

For example, after deleting person 1, the final GET responds with 404 rather
than returning an error object under a success status. `errorJson` sets the
HTTP status before calling `json`; a numeric code inside JSON alone does not
change the response status.

The configured `spc_errorHandler` receives a `Status` for generic framework
errors. It does not receive an exception or your database/session state. Use
error logging for diagnostics, and return an appropriate generic response to
the caller. Errors sent explicitly by a route are already responses; they do
not automatically pass through the generic error handler.

# Continue learning

- Follow the [request cookbook](requests) for headers, form bodies, uploads,
  middleware, CORS, and structured logging.
- Add HTTP tests with the [testing guide](testing). The
  [database example tests](https://github.com/agrafix/Spock/blob/master/examples/rest-api/test/Spec.hs)
  cover CRUD, validation, error statuses, limits, and persistence across reopen.
- Read the [security guide](security) before adding authentication or private data.
