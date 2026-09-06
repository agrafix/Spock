---
layout: page
title: "Securing browser applications"
permalink: /tutorials/security
---

Start with `defaultBrowserSpockCfg`, then make authentication, authorization,
CSRF checks, and output escaping part of each request's path through the
application. This guide uses the repository's
[runnable security example](https://github.com/agrafix/Spock/tree/master/examples/security).
Its tests exercise the form and JSON flows described here with GHC 9.14.1.

## Run the example

From the repository root:

```sh
cabal run spock-security-example -- --local-http 8080
```

Choose a disposable password at the prompt and open `http://localhost:8080/`.
Sign in as `demo`, change the display name, then sign out. The executable hides
terminal password input and hashes it in memory; it has no default password
or credential file. Restarting resets this single-account demonstration.

For an HTTPS reverse proxy, use `--https 8080`. Both modes bind the backend to
loopback. Only the explicit `--local-http` mode disables Secure cookies.

## Configure the browser session

`defaultBrowserSpockCfg` enables CSRF checks, on-demand sessions, and Secure,
HttpOnly, SameSite=Lax browser-session cookies. `defaultSpockCfg` still leaves
CSRF checks off for compatibility. The example's HTTPS mode additionally uses
a `__Host-spock` cookie with `Path=/` and no `Domain` attribute.

HTTPS protects cookies in transit, HttpOnly restricts JavaScript access to
the cookie, and SameSite restricts when the browser sends it across sites.
Keep CSRF checks even with SameSite; these controls address different parts of
an attack. Session identifiers should remain opaque and private. See
[OWASP's session guidance](https://cheatsheetseries.owasp.org/cheatsheets/Session_Management_Cheat_Sheet.html)
and [CSRF guidance](https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html).

The default session store lives in process memory. For multiple workers or
restart persistence, use a shared store such as the
[PostgreSQL adapter](https://github.com/agrafix/Spock/tree/master/Spock-session-postgresql).
The [FAQ](/faq/#when-do-sessions-expire) explains sliding server expiry and
browser cookie lifetime. Choose an expiry policy appropriate to your app;
if you need both idle and absolute limits, retain an authenticated-at timestamp
and enforce the absolute deadline when authorizing requests.

## Protect the login form too

The GET handler renders `getCsrfToken` in a hidden input through Lucid:

```haskell
csrfInput token =
  H.input_ [H.type_ "hidden", H.name_ "__csrf_token", H.value_ token]
```

The surrounding form uses `method="post"` and `action="/login"`. The browser
sends the field and its session cookie together. Full Spock's `post` helper
checks the token before entering the login action. A missing or incorrect
token receives 403, including when an attacker supplies a token from a different
session. GET, HEAD, and OPTIONS must not perform application state changes.
Keep tokens out of URLs and logs.
[OWASP explains the synchronizer-token pattern](https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#synchronizer-token-pattern).

Read credentials from `paramsPost` and reject missing or duplicate fields.
The example's `postField` helper does this so query parameters cannot supply
or override credentials. `param` searches query and form parameters and is
less suitable when the source of a sensitive field matters.

The example uses `Data.Password.Argon2.hashPassword` and `checkPassword` from
the [`password` library](https://github.com/cdepillabout/password/tree/master/password).
Version 3.1 uses Argon2id by default with a fresh random salt, 64 MiB of memory,
two iterations, and one lane. Keep the encoded hash and its parameters in a
real credential store. Benchmark the work factor under your expected load and
review it over time. Use the library's verifier instead of comparing hashes
yourself. [OWASP recommends Argon2id for new password stores](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html).

In production, add login throttling, recovery and account-revocation policies,
and stronger authentication where needed. Return a generic failure for unknown
users and wrong passwords; perform an equivalent password check for unknown
accounts to reduce timing differences. The example does this for its single
account, but does not implement a production identity service. See
[OWASP's authentication guidance](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html).

## Rotate the ID before granting privileges

After successful credential verification, the example executes:

```haskell
sessionRegenerateId
writeSession $ Just $ LoginSession "demo" "demo"
```

Spock replaces the session ID and CSRF token and removes the old session ID.
Its regeneration API preserves existing session data; the example explicitly
replaces the anonymous data with its authenticated value. It then returns a
303 redirect to the fixed `/account` path. Fetch a fresh CSRF token after login;
forms rendered under the old session cannot be reused. Avoid redirects to an
unchecked user-supplied URL.

Rotate identifiers on other privilege changes as well. This prevents someone
who knew the anonymous session ID from retaining access after authentication.
[OWASP describes session fixation and regeneration](https://cheatsheetseries.owasp.org/cheatsheets/Session_Management_Cheat_Sheet.html#renew-the-session-id-after-any-privilege-level-change).

Every protected handler calls `requireUser` before accessing or changing
account data. A valid CSRF token alone does not authenticate a caller. For
applications with multiple users and objects, obtain identity from the validated
session and check that identity's permission for the specific object being
read or changed. A route's typed ID only establishes that the ID parsed.

## Send JSON with a header token

After signing in, a same-origin client can fetch `/csrf` and send the returned
token with its session cookie:

```javascript
const token = await fetch('/csrf', {credentials: 'same-origin'})
  .then(response => response.text());
const response = await fetch('/api/profile', {
  method: 'POST',
  credentials: 'same-origin',
  headers: {'Content-Type': 'application/json', 'X-Csrf-Token': token},
  body: JSON.stringify('Alex')
});
if (!response.ok) throw new Error(`Update failed: ${response.status}`);
```

This endpoint expects a JSON string; it validates the type and length before
updating the session. A token inside the JSON object does not satisfy Spock's
check. Keep token responses private and uncached, and avoid permissive
credentialed CORS rules that expose them to another origin.

### Spock-api-server and core routes need an explicit hook

`defEndpoint` and `defDocumentedEndpoint` register routes through Spock-core.
Setting `spc_csrfProtection` in a full Spock configuration does not automatically
wrap them. The example's equivalent `/typed/profile` endpoint applies the check
explicitly:

```haskell
prehook csrfCheck $
  defEndpoint (MethodPost (Proxy :: Proxy (Text -> Text)) ("typed" <//> "profile")) $ \name -> do
    _ <- requireUser
    updateProfile name
    pure name
```

This hook surrounds only the POST endpoint. If a hook encloses mixed methods,
check CSRF for the unsafe methods; do not require tokens on ordinary page GETs.
The test suite demonstrates that omitting this hook permits an authenticated
POST without a token, and verifies that the hooked route returns 403 instead.

## Escape output and constrain browser behavior

Use Lucid's `toHtml` and attribute combinators for user-controlled values, or
the equivalent escaping functions in your renderer. Calling Spock's `html`
does not escape a string. The sample tests a display name containing a script
tag, quotes, and an ampersand in both HTML text and an input attribute.
HTML escaping is specific to HTML: avoid interpolating input into JavaScript,
CSS, or unchecked URLs. See the [rendering guide](/tutorials/rendering) and
[OWASP's output-encoding guidance](https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html).

The example adds these headers, including on rejected requests:

| Header | Example policy |
| --- | --- |
| `Cache-Control` | `no-store`, including token and account pages |
| `Content-Security-Policy` | Block scripts and other resources by default; allow same-origin connections and form submissions; forbid framing and base URL changes |
| `X-Content-Type-Options` | `nosniff` |
| `Referrer-Policy` | `no-referrer` |

The HTML demonstration has no scripts. Adapt its CSP to the resources your
application serves; adding JavaScript requires a suitable script policy.
CSP supplements escaping and authorization. See
[the CSP reference](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Security-Policy).

## Revoke the session on logout

The logout form is a CSRF-protected POST calling `sessionDestroy`, followed by
a 303 redirect to `/`. Spock deletes the server-side session and expires its
cookie. Avoid another session action in that handler, which would create a
replacement. The next GET may create a new anonymous session for its login
form. The tests replay the logged-out cookie and verify it cannot access
`/account`, while another signed-in session remains usable.

## Keep database and proxy boundaries explicit

For database-backed actions, use parameterized queries or a typed query API.
Never concatenate form values or route captures into SQL. Bind values as
parameters; choose identifiers or sort directions from an application-defined
allowlist. [OWASP's SQL guidance gives the rationale](https://cheatsheetseries.owasp.org/cheatsheets/SQL_Injection_Prevention_Cheat_Sheet.html).

Enforce uniqueness in the database, for example with a unique constraint on
an account name. A separate “check then insert” sequence can race with another
request. Use a transaction for related writes and handle the database's
constraint violation as an expected conflict. PostgreSQL documents
[unique constraints](https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-UNIQUE-CONSTRAINTS).

Configure the TLS proxy to reach a private backend and overwrite forwarded
headers that it supplies. Trust `Forwarded` or `X-Forwarded-*` only from that
proxy, including when deciding HTTPS redirects, origin checks, or rate-limit
identity. Do not use arbitrary incoming host/proxy headers to construct a
login redirect. [Forwarded headers can be supplied by clients](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Forwarded).

The example limits consumed request bodies to 16 KiB. Set appropriate proxy
body limits and timeouts as well, plus endpoint-specific rate limits before
expensive password verification. Record useful outcomes and request IDs;
exclude passwords, authorization headers, raw cookies, and CSRF tokens from
logs. Keep dependencies current and run tests for rejected requests as well
as successful ones:

```sh
cabal test spock-security-example --test-show-details=direct
```
