{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Routing.SafeRoutingSpec where

import Test.Hspec
import Data.HVect hiding (singleton)

import Control.Monad.Identity
import Web.Routing.SafeRouting
import Web.Routing.AbstractRouter
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative (..))
import Data.Monoid (mconcat)
#endif
import qualified Data.Text as T

data ReturnVar
   = IntVar Int
   | StrVar T.Text
   | BoolVar Bool
   | ListVar [ReturnVar]
   deriving (Show, Eq, Read)

defR :: (Monad m, m ReturnVar ~ x) => Path ts -> HVectElim ts x -> RegistryT (SafeRouter m ReturnVar) middleware Bool m ()
defR path action = hookRoute True (SafeRouterPath path) (HVectElim' action)

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
       it "should provide an Applicative interface" $
          do let numbers =
                   mconcat
                   [ singleton var id
                   , singleton ("forty" </> "two") (42 :: Int)
                   ]
                 operators =
                   mconcat
                   [ singleton "plus" ((+) :: Int -> Int -> Int)
                   , singleton "mult" (*)
                   ]
                 routes = operators <*> numbers <*> numbers
                 check path val = match routes (pieces path) `shouldBe` [val]
             check "/plus/forty/two/forty/two" (42+42)
             check "/mult/forty/two/3" (42*3)
             check "/plus/5/89" 94
       it "should have a catch all route" $
          do checkRoute "/aslkdjk/asdaskl/aslkjd" [StrVar "aslkdjk/asdaskl/aslkjd"]
             checkRoute "/zuiasf/zuiasf" [StrVar "zuiasf/zuiasf"]
    where
      pieces :: T.Text -> [T.Text]
      pieces = filter (not . T.null) . T.splitOn "/"

      checkRoute :: T.Text -> [ReturnVar] -> Expectation
      checkRoute r x =
          let matches = handleFun (pieces r)
          in (map (runIdentity . snd) matches) `shouldBe` x

      handleFun :: [T.Text] -> [(ParamMap, Identity ReturnVar)]
      handleFun = handleFun' True
      (_, handleFun', _) =
          runIdentity (runRegistry SafeRouter handleDefs)

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
             hookAny True (return . StrVar . T.intercalate "/")
