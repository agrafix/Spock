{-# LANGUAGE DoAndIfThenElse #-}
module Web.Spock.Internal.Digestive
    ( runForm )
where

import Web.Spock.Internal.CoreAction

import Control.Applicative
import Control.Monad.Trans
import Network.HTTP.Types
import Network.Wai
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- | Run a digestive functors form
runForm :: (Functor m, MonadIO m)
        => T.Text -- ^ form name
        -> Form v (ActionT m) a
        -> ActionT m (View v, Maybe a)
runForm formName form =
    do httpMethod <- requestMethod <$> request
       if httpMethod == methodGet
       then do f <- getForm formName form
               return (f, Nothing)
       else postForm formName form (const $ return localEnv)
    where
      localEnv path =
          do let name = fromPath $ path
                 applyParam f =
                     map (f . snd) . filter ((== name) . fst)
             vars <- (applyParam (TextInput)) <$> params
             sentFiles <- (applyParam (FileInput . T.unpack . uf_name) . HM.toList) <$> files
             return (vars ++ sentFiles)
