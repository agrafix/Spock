---
layout: post
title:  "Shareable APIs and more"
date:   2016-08-25 13:00:00
author: Alexander Thiemann
---

I'm happy to announce the next [Spock release][spock-hackage]! This release comes with several cool new features:

* "Shareable" API definitions
* `wildcard` route pattern
* Custom error handlers
* Custom HTTP verbs
* New CSRF protection
* New session storage system
* Removal of untyped routing
* Split of packages
* Multiple bug fixes

A big thank you goes to all awesome contributors (in chronological order):
[lloucas-imvu][u:lloucas-imvu], [timjb][u:timjb], [dancingrobot84][u:dancingrobot84] and [cdepillabout][u:cdepillabout]!

## "Shareable" API definitions

When we started using [GHCJS][ghcjs] for a project's frontend where the backend was powered with `Spock`, a real pain point was that
calling JSON-APIs defined in the frontend didn't really feel good: One had to render the route defined in `Spock`, send the
correct JSON body (sending the correct JSON body was not enforced anywhere) and receive and parse the response JSON body. If you
are already using the same language, I think calling an endpoint from the frontend should rather feel just like a type-safe
function call. And that's what "shareable" API definitions try to solve. Basically, the idea is to split your project into three packages:

* `project-api`
* `project-frontend`
* `project-backend`

*(where 'project' is your project's name)*

Now in `project-api` we will use only `Spock-api` to describe our API. For example for a login endpoint:

{% highlight haskell %}
module MyProject.Api.User where

import Data.Int
import Web.Spock.Api
import qualified Data.Text as T

data LoginReq
   = LoginReq
   { lr_username :: !T.Text
   , lr_password :: !T.Text
   } deriving (Show, Eq, Generic, NFData, Typeable, ToJSON, FromJSON)

data LoginResp
   = LoginOkay !User
   | LoginFailed
   deriving (Show, Eq, Generic, NFData, Typeable, ToJSON, FromJSON)

data User
   = User
   { u_id :: !Int64
   , u_name :: !T.Text
   , u_email :: !T.Text
   , u_isSuperuser :: !Bool
   } deriving (Show, Eq, Generic, NFData, Typeable, ToJSON, FromJSON)

loginUser :: Endpoint '[] ('Just LoginReq) LoginResp
loginUser = MethodPost Proxy ("api" <//> "user" <//> "auth")
{% endhighlight %}

We can now go ahead and implement that endpoint in `project-backend` using the `Spock-api-server` package (together with `Spock-core` or `Spock`):

{% highlight haskell %}
module MyProject.Api.Server.User
    ( api )
where

import qualified MyProject.Api.User as A

import Web.Spock
import Web.Spock.Api.Server

api :: Application ()
api =
    defEndpoint A.loginUser loginHandler

loginHandler :: A.LoginReq -> Action A.LoginResp
loginHandler r =
    do auth <- runQuery $ \conn -> authUser conn (A.lr_username r) (A.lr_password r)
       case auth of
         Just user ->
             do modifySession $ \sess -> sess { s_user = Just (A.u_id user) }
                pure (A.LoginOkay user)
         Nothing ->
             pure A.LoginFailed
{% endhighlight %}

Note that `defEndpoint` is just another Spock combinator, like the ones you already know and love (`get`, `post`, ...).
Now we can use our API in our frontend in `project-frontend` using the `Spock-api-ghcjs` package:

{% highlight haskell %}
module Main where

import MyProject.Api.User

import Web.Spock.Api.Client

type SessionId = T.Text

main :: IO ()
main =
    do Just res <- callEndpoint loginUser (LoginReq "alex" "alexcool")
       putStrLn ("Login result was: " ++ show res)
{% endhighlight %}

The final step is to compile `project-backend` with GHC and `project-frontend` with GHCJS. The easiest way to go would be to write
two [stack][hstack] files, one `stack.yaml` and one `stack-frontend.yaml` and use the correct one. Example:

{% highlight yaml %}
# stack.yaml
resolver: lts-6.13
packages:
  - pkgs/project-api
  - pkgs/project-backend
  - location:
      git: https://github.com/agrafix/Spock
      commit: 77333a2de5dea0dc8eba9432ab16864e93e5d70e
    extra-dep: true
    subdirs:
      - Spock
      - Spock-core
      - Spock-api
      - Spock-api-server
      - reroute
{% endhighlight %}

{% highlight yaml %}
# stack-frontend.yaml
resolver: lts-6.13
packages:
  - pkgs/project-api
  - pkgs/project-frontend
  - location:
      git: https://github.com/agrafix/Spock
      commit: 77333a2de5dea0dc8eba9432ab16864e93e5d70e
    extra-dep: true
    subdirs:
      - Spock-api
      - Spock-api-ghcjs
      - reroute
compiler: ghcjs-0.2.0.20160414_ghc-7.10.3
compiler-check: match-exact
setup-info:
  ghcjs:
    source:
      ghcjs-0.2.0.20160414_ghc-7.10.3:
        url: https://s3.amazonaws.com/ghcjs/ghcjs-0.2.0.20160414_ghc-7.10.3.tar.gz
        sha1: 6d6f307503be9e94e0c96ef1308c7cf224d06be3
{% endhighlight %}

## `wildcard` route pattern

The `wildcard` route pattern lets you capture the leftover of a route without explicitly defining what it
looks like. For example a route `"foo" </> "bar"` will only match on requests to `foo/bar`, but a route
`"foo" </> wildcard` will match on `foo/baz`, `foo/baz/bim` and so on. The captured leftover will be passed
as `Text` as you are used to from matched paramters. Note that a new type parameter in `Path` will enforce
that `wildcard` can only be used at the end of a route pattern.

## Custom error handlers

You can now define your own error handler in the Spock configuration. An error handler is a function
`errorHandler :: Status -> ActionCtxT () IO ()`, so you could, for example, write out the error to some
event log and show a nice error page to the user. If an uncaught exception occurs in another request
handler your error handler will be called with `status500`. The default error handler will state the HTTP status
code and message along with a `Powered by Spock` :-)

## Custom HTTP verbs

Apart from the standard HTTP verbs `GET`, `POST`, ... one can now write servers accepting custom HTTP verbs,
as common in some IoT device settings. You can use the `hookRouteCustom` combinator for this.

## New CSRF protection

While I liked the `SafeAction` based CSRF protection a lot which generated custom (unguessable) endpoints for
sensitive requests on demand, it came with problems because the sessions were no longer serializable and
became quite large in some cases. Thus we now implement the common `csrf` protection mechanism by providing a
`csrf` token per session and (optionally) automatically checking for it in all HTTP requests that are not
`GET`, `HEAD`, `OPTIONS` or custom. Spock will look for the token in a configurable post parameter or in a
configurable HTTP header.

## New session storage system

We now allow interchangeable session storage systems, which allows you to replace the `stm` based implementation
with for instance a `redis` based one. This is very useful if you have a cluster of webservers running that
should share session state of users across workers. Essentially you will need to provide an implementation by filling
the record `SessionStore` and passing it to the Spock configuration.

{% highlight haskell %}
data SessionStore sess tx
   = SessionStore
   { ss_runTx :: forall a. tx a -> IO a
   , ss_loadSession :: SessionId -> tx (Maybe sess)
   , ss_deleteSession :: SessionId -> tx ()
   , ss_storeSession :: sess -> tx ()
   , ss_toList :: tx [sess]
   , ss_filterSessions :: (sess -> Bool) -> tx ()
   , ss_mapSessions :: (sess -> tx sess) -> tx ()
   }
{% endhighlight %}

## Other changes
### Removal of untyped routing

The untyped routing has fully been removed from `Spock`. There's really no downside on using the type-safe api.

### Split of packages

We've split the `Spock` package into `Spock` and `Spock-core`. If you are using features like sessions and/or
database pooling then `Spock` is the right package for you. `Spock-core` is only a small package providing
the basic wiring and combinators. We are planning on splitting out more features to make them reusable in
other projects.

### Multiple bug fixes

There are several bugs fixed and we've moved to some standard packages like `cookie` instead of our homegrown
solution which should improve the stability of Spock.

## Closing thoughts

Getting ready for a new release was a lot of work, but now it's ready and I am curious to know what you think! Please
send me [an email][atmail] or comment on [Reddit][reddit] or [Hacker News][hn]. We've also already migrated [TramCloud][tramcloud]
(sorry, German) to this version heavily relying on `Spock-api` and GHCJS.

[u:lloucas-imvu]: https://github.com/lloucas-imvu
[u:timjb]: https://github.com/timjb
[u:dancingrobot84]: https://github.com/dancingrobot84
[u:cdepillabout]: https://github.com/cdepillabout
[tramcloud]: https://www.tramcloud.net
[reddit]: https://www.reddit.com/r/haskell/comments/4zjh1z/spock_011_shareable_apis_and_more/
[hn]: https://news.ycombinator.com/item?id=12360463
[atmail]: mailto:mail@athiemann.net
[spock-hackage]: https://hackage.haskell.org/package/Spock-0.11.0.0
[hstack]: http://haskellstack.org/
[ghcjs]: https://github.com/ghcjs/ghcjs
