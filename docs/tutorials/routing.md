---
layout: page
title: "Routing"
date: 2021-11-06 08:36:06
author: Alexander Thiemann
permalink: /tutorials/routing
---

This page contains a collection of routing cookbooks.

# `var` matching on multiple types

You can use the `AltVar` `Either` alternative to allow a `var` that can match on different types:

```haskell

do get ("hello" <//> var) $ \(v :: AltVar Int T.Text) ->
      case v of
        AvLeft number -> text (T.pack (show (1 + number)))
        AvRight str -> text str

```

In this example, `/hello/1` would show `2`, and `/hello/alex` would return `alex`.
