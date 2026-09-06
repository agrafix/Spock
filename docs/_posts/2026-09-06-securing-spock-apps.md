---
layout: post
title: "Securing Spock browser applications"
date: 2026-09-06 00:00:00
author: Alexander Thiemann
---

The new [browser security guide](/tutorials/security) follows a complete login,
form, JSON update, and logout flow. Its runnable example uses Argon2id password
hashing, browser session settings, CSRF checks, session ID rotation, and Lucid
escaping. Tests cover rejected requests, stolen tokens, old session IDs, and
logout revocation.

The guide also explains the explicit CSRF hook needed when using
`Spock-api-server` with cookie authentication, along with database and reverse
proxy responsibilities. Start with the [example and its tests](https://github.com/agrafix/Spock/tree/master/examples/security),
or use the [FAQ](/faq/) for a shorter explanation of sessions and expiry.
