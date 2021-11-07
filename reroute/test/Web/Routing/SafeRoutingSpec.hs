{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Routing.SafeRoutingSpec where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Data.HVect hiding (singleton)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Test.Hspec
import Web.Routing.Combinators
import Web.Routing.Router
import Web.Routing.SafeRouting

data ReturnVar
  = IntVar Int
  | StrVar T.Text
  | BoolVar Bool
  | ListVar [ReturnVar]
  deriving (Show, Eq, Read, Ord)

defR :: (Monad m, m ReturnVar ~ x) => Path ts ps -> HVectElim ts x -> RegistryT m ReturnVar middleware Bool m ()
defR path action = hookRoute True (toInternalPath path) (HVectElim' action)

defRAny :: (Monad m, m ReturnVar ~ x) => Path ts ps -> HVectElim ts x -> RegistryT m ReturnVar middleware Bool m ()
defRAny path action = hookRouteAnyMethod (toInternalPath path) (HVectElim' action)

-- TODO: abstract this code, move into AbstractRouter
defSubComponent ::
  ( Monad m,
#if __GLASGOW_HASKELL__ <= 708
    , Functor m
#endif
    m ([T.Text] -> ReturnVar) ~ x
  ) =>
  Path ts ps ->
  HVectElim ts x ->
  RegistryT m ReturnVar middleware Bool m ()
defSubComponent path comp =
  do
    let reqType = True
    basePath <- ask
    modify $ \rs ->
      rs
        { rs_registry =
            let (reg, fb) = fromMaybe emptyRegistry (HM.lookup reqType (rs_registry rs))
                reg' = insertSubComponent (RouteHandle (basePath </!> toInternalPath path) comp) reg
             in HM.insert reqType (reg', fb) (rs_registry rs)
        }

spec :: Spec
spec =
  describe "SafeRouting Spec" $
    do
      it "should match known routes" $
        do
          checkRoute "" [StrVar "root"]
          checkRoute "/" [StrVar "root"]
          checkRoute "/bar" [StrVar "bar"]
      it "should match any routes" $
        do
          checkRoute' "/any" True [StrVar "any", StrVar "any"] -- two due to hookAny
          checkRoute' "/any" False [StrVar "any"]
      it "should capture variables in routes" $
        do
          checkRoute "/bar/23/baz" [IntVar 23]
          checkRoute "/bar/23/baz/" [IntVar 23]
          checkRoute "/bar/23/baz/100" [ListVar [IntVar 23, IntVar 100]]
          checkRoute "/bar/23/100" [ListVar [IntVar 23, IntVar 100]]
          checkRoute "/entry/344/2014-20-14T12:23" [ListVar [IntVar 344, StrVar "2014-20-14T12:23"]]
          checkRoute "/entry/bytags/344/2014-20-14T12:23" [ListVar [IntVar 344, StrVar "2014-20-14T12:23"]]
          checkRoute "/entry/2/rel/3" [ListVar [IntVar 2, IntVar 3]]
      it "should handle multiple possible matches correctly" $
        do
          checkRoute "/bar/5" [IntVar 5, StrVar "5"]
          checkRoute "/bar/bingo" [StrVar "bar/bingo", StrVar "bingo"]
          checkRoute "/entry/1/audit" [IntVar 1, ListVar [IntVar 1, StrVar "audit"]]
      it "should have a catch all route" $
        do
          checkRoute "/aslkdjk/asdaskl/aslkjd" [StrVar "aslkdjk/asdaskl/aslkjd"]
          checkRoute "/zuiasf/zuiasf" [StrVar "zuiasf/zuiasf"]
      it "should hand over remaining path pieces to subcomponents" $
        do checkRoute "/subcomponent/blog/foo/bar/nanana" [StrVar "blog:foo?bar?nanana"]
      it "should handle wildcard routes" $
        do
          checkRoute "/wildcard/" [StrVar ""]
          checkRoute "/wildcard/some/additional/data" [StrVar "some/additional/data"]
  where
    pieces :: T.Text -> [T.Text]
    pieces = filter (not . T.null) . T.splitOn "/"

    checkRoute' :: T.Text -> Bool -> [ReturnVar] -> Expectation
    checkRoute' r b x =
      let matches = handleFun b (pieces r)
       in sort (map runIdentity matches) `shouldBe` sort x

    checkRoute :: T.Text -> [ReturnVar] -> Expectation
    checkRoute = flip checkRoute' True

    handleFun :: Bool -> [T.Text] -> [Identity ReturnVar]
    handleFun = handleFun'

    (_, handleFun', _) = runIdentity (runRegistry handleDefs)

    handleDefs =
      do
        defR root $ return (StrVar "root")
        defRAny "any" $ pure (StrVar "any")
        defR "bar" $ return (StrVar "bar")
        defR ("bar" </> var) (return . IntVar)
        defR ("bar" </> var </> "baz") (return . IntVar)
        defR ("bar" </> var </> "baz" </> var) $ \i i2 ->
          return (ListVar [IntVar i, IntVar i2])
        defR ("bar" </> var </> var) $ \i i2 ->
          return (ListVar [IntVar i, IntVar i2])
        defR ("entry" </> var </> var) $ \i st ->
          return (ListVar [IntVar i, StrVar st])
        defR ("entry/bytags" </> var </> var) $ \i st ->
          return (ListVar [IntVar i, StrVar st])
        defR ("entry" </> var </> "rel" </> var) $ \i i2 ->
          return (ListVar [IntVar i, IntVar i2])
        defR ("bar" </> "bingo") $ return (StrVar "bar/bingo")
        defR ("bar" </> var) $ (return . StrVar . T.pack)
        defR ("entry" </> var </> "audit") (return . IntVar)
        defSubComponent ("subcomponent" </> var) $ \name ->
          return $ \ps -> StrVar $ name <> ":" <> T.intercalate "?" ps
        defR ("wildcard" </> wildcard) $ \rest ->
          return $ StrVar rest
        hookAny True (return . StrVar . T.intercalate "/")
