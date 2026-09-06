makeApp sink = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  let sessions = (spc_sessionCfg cfg) { sc_sessionMode = SessionsDisabled }
  spockAsApp $ spock (cfg
    { spc_sessionCfg = sessions,
      spc_maxRequestSize = Just (1024 * 1024),
      spc_logging = Just $ defaultLoggingConfig sink,
      spc_logError = const $ pure (), -- The structured sink receives diagnostics.
      spc_errorHandler = \status -> errorJson status "Request failed" }) routes
