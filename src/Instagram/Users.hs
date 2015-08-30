{-# LANGUAGE FlexibleContexts #-}
-- | Users end point handling
-- <http://instagram.com/developer/endpoints/users/#>
module Instagram.Users (
    getUser
  , SelfFeedParams(..)
  , getSelfFeed
  , RecentParams(..)
  , getRecent
  , SelfLikedParams(..)
  , getSelfLiked
  , UserSearchParams(..)
  , searchUsers
) where

import Instagram.Monad
import Instagram.Types

import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)

import qualified Network.HTTP.Types as HT
import Data.Maybe (isJust)
import qualified Data.Text as T (Text)
import Data.Default



-- | Get basic information about a user.
getUser :: (MonadBaseControl IO m, MonadResource m)
        => UserID
        -> Maybe OAuthToken
        -> InstagramT m (Envelope (Maybe User))
getUser uid token = getGetEnvelopeM ["/v1/users/",uid] token ([]::HT.Query)

-- | Parameters for call to self feed
data SelfFeedParams = SelfFeedParams {
    sfpCount :: Maybe Integer,
    sfpMaxID :: Maybe T.Text,
    sfpMinId :: Maybe T.Text
  }
  deriving (Show,Typeable)

instance Default SelfFeedParams where
  def = SelfFeedParams Nothing Nothing Nothing

instance HT.QueryLike SelfFeedParams where
  toQuery (SelfFeedParams c maxI minI) = filter (isJust .snd)
    [ "count" ?+ c
    , "max_id" ?+ maxI
    , "min_id" ?+ minI
    ]


-- |  See the authenticated user's feed.
getSelfFeed :: (MonadBaseControl IO m, MonadResource m)
            => OAuthToken
            -> SelfFeedParams
            -> InstagramT m (Envelope [Media])
getSelfFeed = getGetEnvelope ["/v1/users/self/feed/"]

-- | Parameters for call to recent media
data RecentParams = RecentParams {
    rpCount :: Maybe Integer,
    rpMaxTimestamp :: Maybe POSIXTime,
    rpMinTimestamp :: Maybe POSIXTime,
    rpMaxID :: Maybe T.Text,
    rpMinId :: Maybe T.Text
  }
  deriving (Show,Typeable)

instance Default RecentParams where
  def = RecentParams Nothing Nothing Nothing Nothing Nothing

instance HT.QueryLike RecentParams where
  toQuery (RecentParams c maxT minT maxI minI) = filter (isJust .snd)
    [ "count" ?+ c
    , "max_timestamp" ?+ maxT
    , "min_timestamp" ?+ minT
    , "max_id" ?+ maxI
    , "min_id" ?+ minI
    ]


-- | Get the most recent media published by a user.
getRecent :: (MonadBaseControl IO m, MonadResource m)
          => UserID
          -> OAuthToken
          -> RecentParams
          -> InstagramT m (Envelope [Media])
getRecent uid = getGetEnvelope ["/v1/users/",uid,"/media/recent/"]

-- | parameters for self liked call
data SelfLikedParams = SelfLikedParams {
    slpCount :: Maybe Integer,
    slpMaxLikeID :: Maybe T.Text
  }
  deriving (Show,Typeable)

instance Default SelfLikedParams where
  def = SelfLikedParams Nothing Nothing

instance HT.QueryLike SelfLikedParams where
  toQuery (SelfLikedParams c maxI) = filter (isJust .snd)
    [ "count" ?+ c
    , "max_like_id" ?+ maxI
    ]

-- | See the authenticated user's list of media they've liked.
getSelfLiked :: (MonadBaseControl IO m, MonadResource m)
             => OAuthToken
             -> SelfLikedParams
             -> InstagramT m (Envelope [Media])
getSelfLiked = getGetEnvelope ["/v1/users/self/media/liked"]

-- | parameters for self liked call
data UserSearchParams = UserSearchParams {
    uspQuery :: T.Text,
    uspCount :: Maybe Integer
  }
  deriving (Show,Typeable)

instance HT.QueryLike UserSearchParams where
  toQuery (UserSearchParams q c ) = filter (isJust .snd)
    [ "count" ?+ c -- does not seem to be taken into account...
    , "q" ?+ q
    ]

-- | Search for a user by name.
searchUsers :: (MonadBaseControl IO m, MonadResource m)
            => Maybe OAuthToken
            -> UserSearchParams
            -> InstagramT m (Envelope [User])
searchUsers = getGetEnvelopeM ["/v1/users/search"]
