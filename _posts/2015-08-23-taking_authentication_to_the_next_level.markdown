---
layout: post
title:  "Taking Authentication to the next Level"
date:   2015-08-23 21:20:00
author: Alexander Thiemann
---

I've just released a new version of Spock: [0.9.0.0][new-version]. Along with some small improvements a big feature has dropped: **Contexts**. Let's take a look at them and how they can help out in an authentication scenario.

## What are 'Contexts'?

A context is a value that lives during a request. When a request arrives at your application, the context will be initialized with `()`. Hence the type of the basic route wiring monad `SpockT m a` which is an alias for `SpockCtxT () m a`. Inside your wired routes (`ActionCtxT () m a`) monad you can now access the context using the `getContext :: MonadIO m => ActionCtxT ctx m ctx` function. Let's take a look at this:

{% highlight haskell %}
import Web.Spock.Safe

main :: IO ()
main =
   runSpock 5000 $ spockT $
   get "some-action" $
   do () <- getContext
      text "Context was (). Boring!"
{% endhighlight %}

Now this isn't really much news and it does not seem very useful. That's why you can now register `prehook`s, that is actions that run before contained wired routes:

{% highlight haskell %}
import Web.Spock.Safe
import Data.Monoid
import qualified Data.Text as T

main :: IO ()
main =
   runSpock 5000 $ spockT $
   do get "some-action" $
        do () <- getContext
           text "Context was (). Boring!"
      prehook (return 42) $
        get "other-action" $
        do magicNumber <- getContext
           text "And the magic number is: " <> (T.pack $ show magicNumber)
{% endhighlight %}

We've now hooked an action `return 42` before our `get "other-action"` route. The type of `prehook :: forall m ctx ctx'. MonadIO m => ActionCtxT ctx m ctx' -> SpockCtxT ctx' m () -> SpockCtxT ctx m ()` might seem a little bit complicated at first sight, but it really is not: You supply the action that runs in the current context `ctx` and return a new context `ctx'`. Then you supply new routes that will work on that context `ctx'`. The type "inside" the `prehook` in our example above would be `SpockCtxT Int m a` and `getContext` will thus return an `Int`.

## Authentication

### Prior contexts

Up to recently, authentication in Spock was tiring and not very safe from a programmers point of view. You had to define functions like

{% highlight haskell %}
requireUser :: (User -> ActionT m a) -> ActionT m a
requireUser action =
   do sess <- readSession
      mUser <- getUserFromSession sess
      case mUser of
         Nothing -> text "Sorry, no access!"
         Just user -> action user

requireAdmin :: (Admin -> ActionT m a) -> ActionT m a
-- ...
{% endhighlight %}

and remember to use them in all your routes that you wanted protected. It would even make your code a little bumpy when combined with route parameters:

{% highlight haskell %}
main :: IO ()
main =
   runSpock 5000 $ spockT $
   do get "public" $ text "Everyone can and should see me!"
      get "some-action" $ requireUser $ \_ ->
        text "Great, you are authed."
      get ("action" <//> var) $ \postId -> requireUser $ \user ->
        text ("User " <> userName user <> " requested " <> postId)
      get "admin-panel" $ requireAdmin $ \_ ->
        text "Hi admin!"
      get "all-customers" $
        do allCustomersWithSecretData <- getCustomers
           json allCustomersWithSecretData
      -- ...
{% endhighlight %}

Oops, we even forgot to write `requireUser` for the `/all-customers` route, so now even not logged in users can access all our customers and their secret data. Not good!

This approach has several problems:

* Not very DRY: we have to write `requireUser` around every route we want protected
* One can not easily see (types!) if a route should only run if the user is logged in (e.g. `getCustomers` has the type `ActionCtxT ctx m [Customers]` and can thus be run in any action)
* Error prone

### The new world

Contexts make authentication very elegant. Let's dive right into code - you'll need the [hvect package][hvect] in at least version 0.3.0.0.

#### Defining hooks

Instead of defining an `requireUser` like function, we will define a hook for use in the `prehook` function.

{% highlight haskell %}
import Data.HVect
import Web.Spock.Safe

authHook :: ActionCtxT (HVect xs) m (HVect (User ': xs))
authHook =
    do oldCtx <- getContext
       sess <- readSession
       mUser <- getUserFromSession sess
       case mUser of
           Nothing -> text "Sorry, no access!"
           Just user -> return (user :&: oldCtx)
{% endhighlight %}

*`HVect` is a heterogenous strict list, learn more about heterogenous collections [here][hs-wiki-hlist]*

This hook will *extend* any `HVect xs` context and insert the current `User` into it. If the current user is not logged in the request will be aborted with `text "Sorry, no access!"` like above. If we like to ensure that a user is administrator too, we can extend the hook by using another hook:

{% highlight haskell %}
data IsAdmin = IsAdmin

adminHook :: ListContains n User xs => ActionCtxT (HVect xs) m (HVect (IsAdmin ': xs))
adminHook =
    do oldCtx <- getContext
       let user = findFirst oldCtx
       if userIsAdmin user
       then return (IsAdmin :&: oldCtx)
       else text "Sorry, you are not administrator!"
{% endhighlight %}

We indicate at type level that this route needs a `User` in the context and will add `IsAdmin` to the context. Using `findFirst` we extract the first (and only) occurence of the `User` in our context and check if he is an administrator.

To bootstrap everything, we need to lift the default context `()` to `HVect '[]`:

{% highlight haskell %}
initHook :: ActionCtxT () m (HVect '[])
initHook = return HNil
{% endhighlight %}

#### Wiring it together

Now that we've written hooks it is time to wire them with our routes:

{% highlight haskell %}
main :: IO ()
main =
   runSpock 5000 $ spockT $
   prehook initHook $
   do get "public" $ text "Everyone can and should see me!"
      prehook authHook $
        do get "some-action" $
             text "Great, you are authed."
           get ("action" <//> var) $ \postId ->
             do user <- liftM findFirst getContext
                text ("User " <> userName user <> " requested " <> postId)
           get "all-customers" $
             do allCustomersWithSecretData <- getCustomers
                json allCustomersWithSecretData
           prehook adminHook $
             get "admin-panel" $ text "Hi admin!"
      -- ...
{% endhighlight %}

Great! No more `requireUser` all over the place. Now let's make sure that the `getCustomers` function can only be called when a user is logged in:

{% highlight haskell %}
getCustomers :: ListContains n User xs => ActionCtxT (HVect xs) m [Customers]
{% endhighlight %}

*`ListContains n t xs` is a [TypeFamily][typefamily] that indicates that the type `t` is contained at position `n` in the type level list `xs`.*

Now if we try to use `getCustomers` in an action/route outside of the `prehook authHook` (e.g. `"public"`), we will get a type error! And even better: We can use `getCustomers` inside the `prehook adminHook` too, because our `ListContains` predicates will simply compose. (Side note: you can also indicate that a type `t` should not be contained in the list using `NotInList t xs ~ 'True`)

## Closing Notes

That's it for now - if you are looking for a larger example check out [funblog's Web.Blog module][funblog-web-blog] which I've recently updated to demonstrate usage. If you find the [documentation of Spock][new-version] lacking in any way or you run into a bug, please feel free to create an [issue on Github][gh-issue].

## Comments

Looking forward to your Feedback on [Reddit][reddit-post] and [HackerNews][hn-post].

[new-version]: https://hackage.haskell.org/package/Spock-0.9.0.0
[hvect]: https://hackage.haskell.org/package/hvect
[hs-wiki-hlist]: https://wiki.haskell.org/Heterogenous_collections
[typefamily]: https://wiki.haskell.org/GHC/Type_families
[funblog-web-blog]: https://github.com/agrafix/funblog/blob/1324088e67fc14d471964763e95662195fe3e849/src/Web/Blog.hs
[gh-issue]: https://github.com/agrafix/Spock/issues
[reddit-post]: https://www.reddit.com/r/haskell/comments/3i4d0b/spock_taking_authentication_to_the_next_level/
[hn-post]: https://news.ycombinator.com/item?id=10106739