{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.Digestive
    ( runForm )
where

import Web.Spock.Types

import Control.Applicative
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View
import Web.Scotty.Trans
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Run a digestive functors form
runForm :: T.Text -- ^ form name
        -> Form v (SpockAction conn sess st) a
        -> SpockAction conn sess st (View v, Maybe a)
runForm formName form =
    do httpMethod <- requestMethod <$> request
       if httpMethod == methodGet
       then do f <- getForm formName form
               return (f, Nothing)
       else postForm formName form (const $ return localEnv)
    where
      localEnv :: Env (SpockAction conn sess st)
      localEnv path =
          do let name = TL.fromStrict $ fromPath $ path
                 applyParam f =
                     map (f . snd) . filter ((== name) . fst)
             vars <- (applyParam (TextInput . TL.toStrict)) <$> params
             sentFiles <- (applyParam (FileInput . BS.unpack . fileName)) <$> files
             return (vars ++ sentFiles)
