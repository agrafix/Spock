{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.Digestive
    ( runForm )
where

import Web.Spock.Types
import Web.Spock.Core

import Control.Applicative
import Network.HTTP.Types
import Network.Wai
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

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
          do let name = fromPath $ path
                 applyParam f =
                     map (f . snd) . filter ((== name) . fst)
             vars <- (applyParam (TextInput) . HM.toList) <$> params
             sentFiles <- (applyParam (FileInput . T.unpack . uf_name) . HM.toList) <$> files
             return (vars ++ sentFiles)
