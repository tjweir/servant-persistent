{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Friend where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import qualified Control.Monad.Metrics       as Metrics
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment, metricsCounters)
import           Data.HashMap.Lazy           (HashMap)
import           Data.IORef                  (readIORef)
import           Data.Text                   (Text)
import           Lens.Micro                  ((^.))
import           Models                      (Friend (Friend), runDb, friendEmail,
                                              friendName)
import qualified Models                      as Md
import qualified System.Metrics.Counter      as Counter

type FriendAPI =
         "friends" :> Get '[JSON] [Entity Friend]
    :<|> "friends" :> Capture "name" Text :> Get '[JSON] (Entity Friend)
    :<|> "friends" :> ReqBody '[JSON] Friend :> Post '[JSON] Int64

-- | The server that runs the UserAPI
friendServer :: MonadIO m => ServerT FriendAPI (AppT m)
friendServer = allFriends :<|> singleFriend :<|> createFriend 

-- | Returns all users in the database.
allFriends :: MonadIO m => AppT m [Entity Friend]
allFriends = do
    increment "allFriends"
    logDebugNS "web" "allFriends"
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleFriend :: MonadIO m => Text -> AppT m (Entity Friend)
singleFriend str = do
    increment "singleFriend"
    logDebugNS "web" "singleFriend"
    maybeFriend <- runDb (selectFirst [Md.FriendName ==. str] [])
    case maybeFriend of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createFriend :: MonadIO m => Friend -> AppT m Int64
createFriend p = do
    increment "createFriend"
    logDebugNS "web" "creating a user"
    newFriend <- runDb (insert (Friend (friendName p) (friendEmail p)))
    return $ fromSqlKey newFriend


-- | Generates JavaScript to query the Friend API.
generateFriendJavaScript :: IO ()
generateFriendJavaScript =
    writeJSForAPI (Proxy :: Proxy FriendAPI) vanillaJS "./assets/fapi.js"

