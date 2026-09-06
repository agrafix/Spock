# Browser security example

Run from the repository root:

```sh
cabal run spock-security-example -- --local-http 8080
```

Choose a disposable password at the prompt, then sign in as `demo` at
http://localhost:8080/. The terminal hides password input. The application
hashes it with the `password` library's Argon2id defaults, retains only the hash
in application state, and has no password file or default password. Restarting
the example resets the account and sessions.

`--local-http` explicitly disables Secure cookies for this loopback demo.
Use `--https 8080` behind a TLS reverse proxy for Secure, HttpOnly, SameSite=Lax,
host-only `__Host-spock` cookies. The backend still listens only on loopback;
the proxy is responsible for TLS and access to the backend.

The [security guide](https://www.spock.li/tutorials/security) explains the
login/form/JSON/logout flow and deployment responsibilities. This single-account
example is not a user-management service: production applications need their
own credential store, authorization policy, login rate limits, recovery and
revocation rules. Keep the CSRF and authorization checks when adapting it.

`POST /api/profile` and `POST /typed/profile` accept a JSON string as the new
display name. Both require an authenticated session and its `X-Csrf-Token`.
The latter demonstrates the explicit CSRF hook needed with `Spock-api-server`.

```sh
cabal test spock-security-example --test-show-details=direct
```
