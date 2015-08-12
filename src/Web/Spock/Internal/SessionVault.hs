{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Web.Spock.Internal.SessionVault where

import Web.Spock.Internal.Types

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Concurrent.STM (STM)
import Control.Monad
import Data.Hashable
import Focus as F
import qualified ListT as L
import qualified STMContainers.Map as STMMap
import qualified Data.Text as T

class (Eq (SessionKey s), Hashable (SessionKey s)) => IsSession s where
    type SessionKey s :: *
    getSessionKey :: s -> SessionKey s

instance IsSession (Session conn sess st) where
    type SessionKey (Session conn sess st) = T.Text
    getSessionKey = sess_id

newtype SessionVault s
    = SessionVault { unSessionVault :: STMMap.Map (SessionKey s) s }

-- | Create a new session vault
newSessionVault :: IsSession s => STM (SessionVault s)
newSessionVault = SessionVault <$> STMMap.new

-- | Load a session
loadSession :: IsSession s => SessionKey s -> SessionVault s -> STM (Maybe s)
loadSession key (SessionVault smap) = STMMap.lookup key smap

-- | Store a session, overwriting any previous values
storeSession :: IsSession s => s -> SessionVault s -> STM ()
storeSession v (SessionVault smap) = STMMap.insert v (getSessionKey v) smap

-- | Removea session
deleteSession :: IsSession s => SessionKey s -> SessionVault s -> STM ()
deleteSession k (SessionVault smap) = STMMap.delete k smap

-- | Get all sessions as list
toList :: IsSession s => SessionVault s -> STM [s]
toList =  liftM (map snd) . L.toList . STMMap.stream . unSessionVault

-- | Remove all sessions that do not match the predicate
filterSessions :: IsSession s => (s -> Bool) -> SessionVault s -> STM ()
filterSessions cond sv =
    do allVals <- toList sv
       let deleteKeys =
               map getSessionKey $
               filter (not . cond) allVals
       forM_ deleteKeys $ flip deleteSession sv

-- | Perform action on all sessions
mapSessions :: IsSession s => (s -> STM s) -> SessionVault s -> STM ()
mapSessions f sv@(SessionVault smap) =
    do allVals <- toList sv
       forM_ allVals $ \sess ->
           STMMap.focus (F.adjustM f) (getSessionKey sess) smap
