{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Spock.SafeActions where

import Web.Spock.Core
import Web.Spock.Types
import Network.HTTP.Types.Status

import qualified Data.Text as T

-- | Wire up a safe action: Safe actions are actions that are protected from
-- csrf attacks. Here's a usage example:
--
-- > newtype DeleteUser = DeleteUser Int deriving (Hashable, Typeable, Eq)
-- >
-- > instance SafeAction Connection () () DeleteUser where
-- >    runSafeAction (DeleteUser i) =
-- >       do runQuery $ deleteUserFromDb i
-- >          redirect "/user-list"
-- >
-- > get "/user-details/:userId" $
-- >   do userId <- param' "userId"
-- >      deleteUrl <- safeActionPath (DeleteUser userId)
-- >      html $ T.concat [ "Click <a href='", TL.fromStrict deleteUrl, "'>here</a> to delete user!" ]
--
-- Note that safeActions currently only support GET and POST requests.
--
safeActionPath :: forall conn sess st a.
                  ( SafeAction conn sess st a
                  , HasSpock(SpockAction conn sess st)
                  , SpockConn (SpockAction conn sess st) ~ conn
                  , SpockSession (SpockAction conn sess st) ~ sess
                  , SpockState (SpockAction conn sess st) ~ st)
               => a
               -> SpockAction conn sess st T.Text
safeActionPath safeAction =
    do mgr <- getSessMgr
       hash <- (sm_addSafeAction mgr) (PackedSafeAction safeAction)
       return $ T.concat [ "/h/", hash ]

hookSafeActions :: forall conn sess st.
                   ( HasSpock (SpockAction conn sess st)
                   , SpockConn (SpockAction conn sess st) ~ conn
                   , SpockSession (SpockAction conn sess st) ~ sess
                   , SpockState (SpockAction conn sess st) ~ st)
                => SpockM conn sess st ()
hookSafeActions =
    do get "/h/:spock-csurf-protection" run
       post "/h/:spock-csurf-protection" run
    where
      run =
          do Just h <- param "spock-csurf-protection"
             mgr <- getSessMgr
             mAction <- (sm_lookupSafeAction mgr) h
             case mAction of
               Nothing ->
                   do setStatus status404
                      text "File not found"
               Just p@(PackedSafeAction action) ->
                   do runSafeAction action
                      (sm_removeSafeAction mgr) p
