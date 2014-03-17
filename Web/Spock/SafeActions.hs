{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Web.Spock.SafeActions where

import Web.Scotty.Trans
import Web.Spock.Types
import Web.Spock.Monad

import qualified Data.Text as T

-- | Wire up a safe action: Safe actions are actions that are protected from
-- csrf attacks. Here's a usage example:
--
-- > newtype DeleteUser = DeleteUser Int deriving (Hashable, Typeable, Eq)
-- >
-- > instance SafeAction DeleteUser where
-- >    runSafeAction (DeleteUser i) =
-- >       do runQuery $ deleteUserFromDb i
-- >          redirect "/user-list"
-- >
-- > get "/user-details/:userId" $
-- >   do userId <- param "userId"
-- >      deleteUrl <- safeActionPath (DeleteUser userId)
-- >      html $ TL.concat [ "Click <a href='", TL.fromStrict deleteUrl, "'>here</a> to delete user!" ]
--
-- Note that safeActions currently only support GET and POST requests.
--
safeActionPath :: forall a conn sess st. SafeAction a
               => a -> SpockAction conn sess st T.Text
safeActionPath safeAction =
    do mgr <- getSessMgr
       hash <- (sm_addSafeAction mgr) (PackedSafeAction safeAction)
       return $ T.concat [ "/h/", hash ]

hookSafeActions :: SpockM conn sess st ()
hookSafeActions =
    do get "/h/:spock-csurf-protection" run
       post "/h/:spock-csurf-protection" run
    where
      run :: SpockAction conn sess st ()
      run =
          do h <- param "spock-csurf-protection"
             mgr <- getSessMgr
             mAction <- (sm_lookupSafeAction mgr) h
             case mAction of
               Nothing ->
                   next
               Just (PackedSafeAction action) ->
                   runSafeAction action
