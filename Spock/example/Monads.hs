{-# LANGUAGE OverloadedStrings #-}

module Monads (app, coreApp) where

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Web.Spock
import qualified Web.Spock.Core as Core

newtype VisitorName = VisitorName Text

type Routes = SpockM () () Text ()
type NamedRoutes = SpockCtxM VisitorName () () Text ()
type Action a = SpockAction () () Text a
type NamedAction a = SpockActionCtx VisitorName () () Text a

-- Shared services have no particular request or visitor context.
prefix :: WebStateM () () Text Text
prefix = getState

app :: Routes
app = do
  configured <- lift prefix -- Registration: runs when the app is built.
  get "setup" $ text configured
  prehook visitor namedRoutes
  get "outside" $ getContext >>= \() -> text "outside the hook"

-- This header is display text, not proof of identity.
visitor :: Action VisitorName
visitor = VisitorName . fromMaybe "visitor" <$> header "X-Display-Name"

namedRoutes :: NamedRoutes
namedRoutes = get "hello" greeting

greeting :: NamedAction ()
greeting = do
  VisitorName name <- getContext
  greetingPrefix <- lift prefix -- Action: runs for each request.
  text (greetingPrefix <> ", " <> name <> "!")

-- Core Spock can instead run over a base monad chosen by the application.
coreApp :: Core.SpockT (ReaderT Text IO) ()
coreApp = Core.get Core.root $ do
  message <- lift ask
  Core.text message
