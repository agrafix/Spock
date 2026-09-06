{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FAQ (app, Slug (..)) where

import qualified Data.Text as T
import Network.HTTP.Types.Status (status400)
import Text.Regex.TDFA
import Web.HttpApiData
import Web.Spock

newtype Slug = Slug T.Text deriving (Eq, Show)

-- Compile the constant expression once. Disable multiline anchors so an
-- embedded newline cannot turn a partial match into an accepted segment.
slugPattern :: Regex
slugPattern = makeRegexOpts (defaultCompOpt { multiline = False }) defaultExecOpt
  ("^[a-z][a-z0-9-]*$" :: String)

instance FromHttpApiData Slug where
  parseUrlPiece piece
    | matchTest slugPattern (T.unpack piece) = Right (Slug piece)
    | otherwise = Left "Expected a lowercase slug"

instance ToHttpApiData Slug where
  toUrlPiece (Slug piece) = piece

app :: SpockM () () () ()
app = do
  get ("number" <//> var) $ \(number :: Int) -> text (T.pack $ show number)
  get ("slug" <//> var) $ \(Slug slug) -> text slug
  get ("rest" <//> wildcard) text
  get "form" $ do
    token <- getCsrfToken
    -- The only interpolated value is Spock's generated base64url CSRF token.
    -- Use an escaping HTML renderer when rendering arbitrary application data.
    html ("<form method=post action=/form><input type=hidden name=__csrf_token value=\""
      <> token <> "\"><button>Submit</button></form>")
  get "csrf" $ getCsrfToken >>= text
  post "form" $ text "Accepted"
  post "json" $ do
    payload <- jsonBody :: SpockAction () () () (Maybe T.Text)
    case payload of
      Just value -> json value
      Nothing -> setStatus status400 >> text "Expected a JSON string"
