module Main (main) where

import FAQ (app)
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  cfg <- defaultBrowserSpockCfg () PCNoDatabase ()
  let sessions = spc_sessionCfg cfg
      cookies = (sc_cookieSettings sessions) { cs_secure = False }
  -- Local HTTP demo only. Keep Secure cookies enabled when deploying on HTTPS.
  runSpock 8080 $ spock (cfg { spc_sessionCfg = sessions { sc_cookieSettings = cookies } }) app
