{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Routing.SafeRoutingSpec where

import Test.Hspec
import Data.HVect hiding (singleton)

import Control.Monad.Identity
import Web.Routing.SafeRouting
import Web.Routing.Router
import qualified Data.Text as T
import Control.Monad.RWS.Strict
import Data.Maybe
import qualified Data.HashMap.Strict as HM

data ReturnVar
   = IntVar Int
   | StrVar T.Text
   | BoolVar Bool
   | ListVar [ReturnVar]
   deriving (Show, Eq, Read)

defR :: (Monad m, m ReturnVar ~ x) => Path ts -> HVectElim ts x -> RegistryT m ReturnVar middleware Bool m ()
defR path action = hookRoute True path (HVectElim' action)

-- TODO: abstract this code, move into AbstractRouter
defSubComponent :: (Monad m, m ([T.Text] -> ReturnVar) ~ x)
                 => Path ts
                 -> HVectElim ts x
                 -> RegistryT m ReturnVar middleware Bool m ()
defSubComponent path comp =
    do let reqType = True
       basePath <- ask
       modify $ \rs ->
           rs { rs_registry =
                    let (reg, fb) = fromMaybe emptyRegistry (HM.lookup reqType (rs_registry rs))
                        reg' = insertSubComponent (RouteHandle (basePath </> path) comp) reg
                    in HM.insert reqType (reg', fb) (rs_registry rs)
              }


spec :: Spec
spec =
    describe "SafeRouting Spec" $
    do it "should match known routes" $
          do checkRoute "" [StrVar "root"]
             checkRoute "/" [StrVar "root"]
             checkRoute "/bar" [StrVar "bar"]
       it "should capture variables in routes" $
          do checkRoute "/bar/23/baz" [IntVar 23]
             checkRoute "/bar/23/baz/" [IntVar 23]
             checkRoute "/bar/23/baz/100" [ListVar [IntVar 23, IntVar 100]]
             checkRoute "/bar/23/100" [ListVar [IntVar 23, IntVar 100]]
             checkRoute "/entry/344/2014-20-14T12:23" [ListVar [IntVar 344, StrVar "2014-20-14T12:23"]]
             checkRoute "/entry/bytags/344/2014-20-14T12:23" [ListVar [IntVar 344, StrVar "2014-20-14T12:23"]]
             checkRoute "/entry/2/rel/3"  [ListVar [IntVar 2, IntVar 3]]
       it "should handle multiple possible matches correctly" $
          do checkRoute "/bar/5" [IntVar 5, StrVar "5"]
             checkRoute "/bar/bingo" [StrVar "bar/bingo", StrVar "bingo"]
             checkRoute "/entry/1/audit" [IntVar 1,ListVar [IntVar 1,StrVar "audit"]]
       it "should have a catch all route" $
          do checkRoute "/aslkdjk/asdaskl/aslkjd" [StrVar "aslkdjk/asdaskl/aslkjd"]
             checkRoute "/zuiasf/zuiasf" [StrVar "zuiasf/zuiasf"]
       it "should hand over remaining path pieces to subcomponents" $
          do checkRoute "/subcomponent/blog/foo/bar/nanana" [StrVar "blog:foo?bar?nanana"]
    where
      pieces :: T.Text -> [T.Text]
      pieces = filter (not . T.null) . T.splitOn "/"

      checkRoute :: T.Text -> [ReturnVar] -> Expectation
      checkRoute r x =
          let matches = handleFun (pieces r)
          in (map runIdentity matches) `shouldBe` x

      handleFun :: [T.Text] -> [Identity ReturnVar]
      handleFun = handleFun' True
      (_, handleFun', _) = runIdentity (runRegistry handleDefs)

      handleDefs =
          do defR root $ return (StrVar "root")
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
             hookAny True (return . StrVar . T.intercalate "/")
