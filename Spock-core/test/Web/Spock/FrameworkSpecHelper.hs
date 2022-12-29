{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Spock.FrameworkSpecHelper where

#if MIN_VERSION_hspec_wai(0,8,0)
import Test.Hspec.Wai.Matcher
#endif

#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Network.Wai as Wai
import Test.Hspec
import Test.Hspec.Wai

statusBodyMatch :: Int -> BSLC.ByteString -> ResponseMatcher
#if MIN_VERSION_hspec_wai(0,8,0)
statusBodyMatch s b =
    ResponseMatcher
    { matchStatus = s
    , matchBody = bodyEquals b
    , matchHeaders = []
    }
#else
statusBodyMatch s b =
    ResponseMatcher { matchStatus = s, matchBody = Just b, matchHeaders = [] }
#endif

sizeLimitSpec :: (Word64 -> IO Wai.Application) -> Spec
sizeLimitSpec app =
  with (app maxSize) $
    describe "Request size limit" $
      do
        it "allows small enough requests the way" $
          do
            post "/size" okBs `shouldRespondWith` matcher 200 okBs
            post "/size" okBs2 `shouldRespondWith` matcher 200 okBs2
        it "denys large requests the way" $
          post "/size" tooLongBs `shouldRespondWith` 413
  where
    matcher = statusBodyMatch
    maxSize = 1024
    okBs = BSLC.replicate (fromIntegral maxSize - 50) 'i'
    okBs2 = BSLC.replicate (fromIntegral maxSize) 'j'
    tooLongBs = BSLC.replicate (fromIntegral maxSize + 100) 'k'

frameworkSpec :: IO Wai.Application -> Spec
frameworkSpec app =
  with app $
    do
      routingSpec
      actionSpec
      headerTest
      cookieTest
      fileTest

routingSpec :: SpecWith (st, Wai.Application)
routingSpec =
  describe "Routing Framework" $
    do
      it "allows root actions" $
        get "/" `shouldRespondWith` "root" {matchStatus = 200}
      it "allows access to get params" $
        get "/get-params?foo=bar" `shouldRespondWith` "[(\"foo\",\"bar\")]" {matchStatus = 200}
      it "supports wai app responses" $
        do
          get "/wai/foo" `shouldRespondWith` "[\"wai\",\"foo\"]" {matchStatus = 200}
          get "/wai/foo/bar" `shouldRespondWith` "[\"wai\",\"foo\",\"bar\"]" {matchStatus = 200}
      it "allows access to post params" $
        postHtmlForm "/post-params" [("foo", "bar")]
          `shouldRespondWith` "[(\"foo\",\"bar\")]" {matchStatus = 200}
      it "allows access to empty post params" $
        postHtmlForm "/post-params" []
          `shouldRespondWith` "[]" {matchStatus = 200}
      it "allows broken body for post params" $
        post "/post-params" ""
          `shouldRespondWith` "[]" {matchStatus = 200}
      it "allows json body" $
        post "/json" "{ \"sampleJson\": \"foo\"}"
          `shouldRespondWith` "foo" {matchStatus = 200}
      it "allows raw body" $
        post "/raw-body" "raw" `shouldRespondWith` "raw" {matchStatus = 200}
      it "allows empty raw body" $
        post "/raw-body" "" `shouldRespondWith` "" {matchStatus = 200}
      it "matches regardless of the VERB" $
        do
          get "/all/verbs" `shouldRespondWith` "ok" {matchStatus = 200}
          post "/all/verbs" "" `shouldRespondWith` "ok" {matchStatus = 200}
          request "FIZZBUZZ" "/all/verbs" [] "" `shouldRespondWith` "ok" {matchStatus = 200}
          request "NOTIFY" "/all/verbs" [] "" `shouldRespondWith` "ok" {matchStatus = 200}
      it "routes different HTTP-verbs to different actions" $
        do
          verbTest get "GET"
          verbTest (`post` "") "POST"
          verbTest (`put` "") "PUT"
          verbTest delete "DELETE"
          verbTest (`patch` "") "PATCH"
          verbTestGp get "GETPOST"
          verbTestGp (`post` "") "GETPOST"
      it "can extract params from routes" $
        get "/param-test/42" `shouldRespondWith` "int42" {matchStatus = 200}
      it "can handle multiple matching routes" $
        get "/param-test/static" `shouldRespondWith` "static" {matchStatus = 200}
      it "ignores trailing slashes" $
        get "/param-test/static/" `shouldRespondWith` "static" {matchStatus = 200}
      it "works with subcomponents" $
        do
          get "/subcomponent/foo" `shouldRespondWith` "foo" {matchStatus = 200}
          get "/subcomponent/subcomponent2/bar" `shouldRespondWith` "bar" {matchStatus = 200}
      it "allows the definition of a fallback handler" $
        get "/askldjas/aklsdj" `shouldRespondWith` "askldjas/aklsdj" {matchStatus = 200}
      it "allows the definition of a fallback handler for custom verb" $
        request "MYVERB" "/askldjas/aklsdj" [] "" `shouldRespondWith` "askldjas/aklsdj" {matchStatus = 200}
      it "detected the preferred format" $
        request "GET" "/preferred-format" [("Accept", "text/html,application/xml;q=0.9,image/webp,*/*;q=0.8")] "" `shouldRespondWith` "html" {matchStatus = 200}
      it "/test-slash and test-noslash are the same thing" $
        do
          get "/test-slash" `shouldRespondWith` "ok" {matchStatus = 200}
          get "test-slash" `shouldRespondWith` "ok" {matchStatus = 200}
          get "/test-noslash" `shouldRespondWith` "ok" {matchStatus = 200}
          get "test-noslash" `shouldRespondWith` "ok" {matchStatus = 200}
      it "allows custom verbs" $
        request "NOTIFY" "/notify/itnotifies" [] "" `shouldRespondWith` "itnotifies" {matchStatus = 200}
  where
    verbTestGp verb verbVerbose =
      verb "/verb-test-gp" `shouldRespondWith` (verbVerbose {matchStatus = 200})
    verbTest verb verbVerbose =
      verb "/verb-test" `shouldRespondWith` (verbVerbose {matchStatus = 200})

errorHandlerSpec :: IO Wai.Application -> Spec
errorHandlerSpec app =
  with app $
    describe "Error Handler" $
      do
        it "handles non-existing routes correctly" $
          do
            get "/non/existing/route" `shouldRespondWith` "NOT FOUND" {matchStatus = 404}
            post "/non/existing/route" "" `shouldRespondWith` "NOT FOUND" {matchStatus = 404}
            put "/non/existing/route" "" `shouldRespondWith` "NOT FOUND" {matchStatus = 404}
            patch "/non/existing/route" "" `shouldRespondWith` "NOT FOUND" {matchStatus = 404}
        it "handles server errors correctly" $
          get "/failing/route" `shouldRespondWith` "SERVER ERROR" {matchStatus = 500}
        it "does not interfere with user emitted errors" $
          get "/user/error" `shouldRespondWith` "UNAUTHORIZED" {matchStatus = 403}

actionSpec :: SpecWith (st, Wai.Application)
actionSpec =
  describe "Action Framework" $
    do
      it "handles auth correctly" $
        do
          request methodGet "/auth/user/pass" [mkAuthHeader "user" "pass"] "" `shouldRespondWith` "ok" {matchStatus = 200}
          request methodGet "/auth/user/pass" [mkAuthHeader "user" ""] "" `shouldRespondWith` "err" {matchStatus = 401}
          request methodGet "/auth/user/pass" [mkAuthHeader "" ""] "" `shouldRespondWith` "err" {matchStatus = 401}
          request methodGet "/auth/user/pass" [mkAuthHeader "asd" "asd"] "" `shouldRespondWith` "err" {matchStatus = 401}
          request methodGet "/auth/user/pass" [] "" `shouldRespondWith` "Authentication required. " {matchStatus = 401}
  where
    mkAuthHeader :: BS.ByteString -> BS.ByteString -> Header
    mkAuthHeader user pass =
      ("Authorization", "Basic " <> (B64.encode $ user <> ":" <> pass))

cookieTest :: SpecWith (st, Wai.Application)
cookieTest =
  describe "Cookies" $
    do
      it "sets single cookies correctly" $
        get "/cookie/single"
          `shouldRespondWith` "set"
            { matchStatus = 200,
              matchHeaders =
                [ matchCookie "single" "test"
                ]
            }
      it "sets multiple cookies correctly" $
        get "/cookie/multiple"
          `shouldRespondWith` "set"
            { matchStatus = 200,
              matchHeaders =
                [ matchCookie "multiple1" "test1",
                  matchCookie "multiple2" "test2"
                ]
            }

headerTest :: SpecWith (st, Wai.Application)
headerTest =
  describe "Headers" $
    do
      it "supports custom headers" $
        get "/set-header"
          `shouldRespondWith` "ok"
            { matchStatus = 200,
              matchHeaders =
                [ "X-FooBar" <:> "Baz"
                ]
            }
      it "supports multi headers" $
        get "/set-multi-header"
          `shouldRespondWith` "ok"
            { matchStatus = 200,
              matchHeaders =
                [ "Content-Language" <:> "de",
                  "Content-Language" <:> "en"
                ]
            }

matchCookie :: T.Text -> T.Text -> MatchHeader
matchCookie name val =
#if MIN_VERSION_hspec_wai(0,8,0)
    MatchHeader $ \headers _ ->
#else
    MatchHeader $ \headers ->
#endif
  let relevantHeaders = filter (\h -> fst h == "Set-Cookie") headers
      loop [] =
        Just
          ( "No cookie named " ++ T.unpack name ++ " with value "
              ++ T.unpack val
              ++ " found"
          )
      loop (x : xs) =
        let (cname, cval) = T.breakOn "=" $ fst $ T.breakOn ";" $ T.decodeUtf8 $ snd x
         in if cname == name && cval == "=" <> val
              then Nothing
              else loop xs
   in loop relevantHeaders

fileTest :: SpecWith (st, Wai.Application)
fileTest =
  describe "" $
    do
      it "receives a single file" $
        do
          request methodPost "file/upload" headers bodySingle `shouldRespondWith` "1" {matchStatus = 200}
      it "receives multiple files with different names" $
        do
          request methodPost "file/upload" headers bodyUnique `shouldRespondWith` "2" {matchStatus = 200}
      it "receives multiple files with similar names" $
        do
          request methodPost "file/upload/multi" headers bodyMulti `shouldRespondWith` "2" {matchStatus = 200}
  where
    bodySingle = BSLC.pack $
             boundary <> crlf
             <> "Content-Disposition: form-data; name=\"name\"" <> crlf <> crlf
             <> "file1.pdf" <> crlf
             <> boundary <> crlf
             <> "Content-Disposition: form-data; name=\"file\"; filename=\"file1.pdf\"" <> crlf
             <> "Content-Type: application/pdf" <> crlf
             <> "Content-Transfer-Encoding: base64" <> crlf <> crlf
             <> "aGFza2VsbA==" <> crlf
             <> boundary <> "--" <> crlf

    bodyUnique = BSLC.pack $
             boundary <> crlf
             <> "Content-Disposition: form-data; name=\"names\"" <> crlf <> crlf
             <> "file1.pdf; file2.pdf" <> crlf
             <> boundary <> crlf
             <> "Content-Disposition: form-data; name=\"file1\"; filename=\"file1.pdf\"" <> crlf
             <> "Content-Type: application/pdf" <> crlf
             <> "Content-Transfer-Encoding: base64" <> crlf <> crlf
             <> "aGFza2VsbA==" <> crlf
             <> boundary <> crlf
             <> "Content-Disposition: form-data; name=\"file2\"; filename=\"file2.pdf\"" <> crlf
             <> "Content-Type: application/pdf" <> crlf
             <> "Content-Transfer-Encoding: base64" <> crlf <> crlf
             <> "c3BvY2s=" <> crlf
             <> boundary <> "--" <> crlf

    bodyMulti = BSLC.pack $
             boundary <> crlf
             <> "Content-Disposition: form-data; name=\"name1\"" <> crlf <> crlf
             <> "file1.pdf" <> crlf
             <> "Content-Disposition: form-data; name=\"name2\"" <> crlf <> crlf
             <> "file2.pdf" <> crlf
             <> boundary <> crlf
             <> "Content-Disposition: form-data; name=\"file\"; filename=\"file1.pdf\"" <> crlf
             <> "Content-Type: application/pdf" <> crlf
             <> "Content-Transfer-Encoding: base64" <> crlf <> crlf
             <> "aGFza2VsbA==" <> crlf
             <> boundary <> crlf
             <> "Content-Disposition: form-data; name=\"file\"; filename=\"file2.pdf\"" <> crlf
             <> "Content-Type: application/pdf" <> crlf
             <> "Content-Transfer-Encoding: base64" <> crlf <> crlf
             <> "c3BvY2s=" <> crlf
             <> boundary <> "--" <> crlf

    boundary :: String
    boundary = "--__boundary"

    crlf :: String
    crlf = "\r\n"

    headers :: [Header]
    headers = [ mkHeader "Content-Type" "multipart/form-data; boundary=__boundary"
              , mkHeader "Accept-Encoding" "gzip"
              ]

    mkHeader :: HeaderName -> BS.ByteString -> Header
    mkHeader key val = (key, val)
