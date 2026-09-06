-- Middleware wraps requests and responses, including requests with no route.
middleware $ \application req respond -> application req $
  respond . Wai.mapResponseHeaders (("X-Cookbook", "Spock") :)
-- Public stateless demo: allow one development origin, without credentials.
middleware $ cors $ const $ Just simpleCorsResourcePolicy
  { corsOrigins = Just (["http://localhost:3000"], False),
    corsMethods = ["GET", "POST", "OPTIONS"],
    corsRequestHeaders = ["Content-Type", "X-Client"] }
