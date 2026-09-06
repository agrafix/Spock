---
layout: page
title: "Spock's monads and type parameters"
permalink: /tutorials/monads
---

A Spock program has two phases: registering routes when the application is
built, and running an action for each matching request. Both phases can use
the application's shared services. The monad in a type signature tells you
which of these jobs a function does.

| Type | When it runs | What it provides |
| --- | --- | --- |
| `SpockM conn sess st a` | Application setup | Register routes, hooks, and middleware |
| `SpockAction conn sess st a` | A request | Read the request, use the visitor's session, send a response |
| `WebStateM conn sess st a` | When a caller runs it | Shared application state and database services |

The parameters describe your application: `conn` is a database connection
(`()` with `PCNoDatabase`), `sess` is one visitor's session value, and `st` is
shared application state. The final `a` is the computation's result; a route
registration block or completed handler normally uses `()`.

`WebStateM` reads an existing application environment. It does not make an
immutable `st` mutable. Put an `IORef` or `TVar` inside `st`, with appropriate
synchronization, when requests need to update shared values.

## Sharing a helper between setup and requests

This helper reads the application's greeting prefix:

```haskell
prefix :: WebStateM () () Text Text
prefix = getState
```

Both registration and actions sit over `WebStateM`. `lift prefix` enters that
base monad. Its location determines when it runs:

```haskell
app :: SpockM () () Text ()
app = do
  configured <- lift prefix -- Once, when building the application.
  get "setup" $ text configured
  get "hello" $ do
    current <- lift prefix -- For each request to /hello.
    text current
```

Import `lift` from `Control.Monad.Trans.Class`. Use `liftIO`, from
`Control.Monad.IO.Class`, for ordinary IO such as reading an `IORef`; it works
through all the intervening layers. `lift` moves through one layer.
`getState` and `runQuery` themselves are polymorphic in `HasSpock`, so you can
also call them directly in either registration or an action. A helper with a
concrete `WebStateM` signature needs `lift` there.

`WebStateM` has no current request, response, hook context, or visitor session.
Keep code using `header`, `jsonBody`, `readSession`, or `text` in an action.
Sending a response ends the action: code after `text` or `json` does not run.

## Typed context from a prehook

`SpockCtxM ctx conn sess st a` and `SpockActionCtx ctx conn sess st a` add a
request-local `ctx`. Their shorter names above set `ctx` to `()`.
A `prehook` computes context for a matching request, and `getContext` reads it
inside the hooked routes:

```haskell
newtype VisitorName = VisitorName Text

visitor :: SpockAction () () Text VisitorName
visitor = VisitorName . fromMaybe "visitor" <$> header "X-Display-Name"

namedRoutes :: SpockCtxM VisitorName () () Text ()
namedRoutes = get "hello" $ do
  VisitorName name <- getContext
  greetingPrefix <- lift prefix
  text (greetingPrefix <> ", " <> name <> "!")

app :: SpockM () () Text ()
app = prehook visitor namedRoutes
```

The display-name header here is user input, not authentication. A real
authentication hook would validate credentials before producing its context.
Context is scoped to the hooked routes and the individual request. It is
different from both application-wide `st` and a visitor's persistent `sess`.
`lift` does not change the context type; use `prehook` to supply a new context.

## Core Spock and custom base monads

Full Spock specializes core Spock to `WebStateM conn sess st`:

```haskell
type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)
type SpockActionCtx ctx conn sess st = ActionCtxT ctx (WebStateM conn sess st)
```

In core Spock, `SpockT m a` registers routes and `ActionT m a` handles requests
over a base monad you choose. `SpockCtxT ctx m a` and `ActionCtxT ctx m a` are
their context-aware forms. For example, `SpockT (ReaderT Text IO) ()` can use
`lift ask` inside an action; pass a runner such as `flip runReaderT "hello"`
to `spockT`. The runner must work for every result type.

`WebStateT conn sess st m a` is the transformer used to build `WebStateM`,
whose underlying monad is `ResourceT IO`. Applications usually need neither
to construct this stack nor to unwrap it. For an existing full Spock
environment, `getSpockHeart` and `runSpockIO` let an IO caller run a shared
service computation; this still does not create a request context.

## Run the examples

The repository contains a [complete, tested example](https://github.com/agrafix/Spock/tree/master/Spock/example)
and [tests of both full and core Spock](https://github.com/agrafix/Spock/blob/master/Spock/test/Web/Spock/MonadTypesSpec.hs).
From the repository root:

```sh
cabal run spock-monads-example
curl -H 'X-Display-Name: Alex' http://localhost:8080/hello
cabal test Spock
```

See the [API reference](/reference/) for the complete types and available
request, session, and response helpers.
