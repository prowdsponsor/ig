{-# LANGUAGE FlexibleContexts #-}
-- | relationship handling
-- <http://instagram.com/developer/endpoints/relationships/#>
module Instagram.Relationships (
  getFollows
  ,getFollowedBy
  ,getFollowsParams
  ,getFollowedByParams
  ,getRequestedBy
  ,getRelationship
  ,setRelationShip
  ,RelationShipAction(..)
  ,FollowParams(..)
)where

import Instagram.Monad
import Instagram.Types

import Data.Typeable (Typeable)

import qualified Network.HTTP.Types as HT
import Data.Char (toLower)


-- | Get the list of users this user follows.
getFollows ::     (MonadBaseControl IO m, MonadResource m) => UserID
  -> Maybe OAuthToken
  -> InstagramT m (Envelope [User])
getFollows uid token  =getGetEnvelopeM ["/v1/users/",uid,"/follows"] token ([]::HT.Query)

-- | Get the list of users this user is followed by.
getFollowedBy ::     (MonadBaseControl IO m, MonadResource m) => UserID
  -> Maybe OAuthToken
  -> InstagramT m (Envelope [User])
getFollowedBy uid token  =getGetEnvelopeM ["/v1/users/",uid,"/followed-by"] token ([]::HT.Query)

data FollowParams = FollowParams {
    fpCount :: Int
    } deriving (Show, Read, Eq, Ord)

instance HT.QueryLike FollowParams where
  toQuery FollowParams{fpCount=count} =
    ["count" ?+ show count]

-- | Get the list of users this user follows.
getFollowsParams :: (MonadBaseControl IO m, MonadResource m) => UserID
  -> Maybe OAuthToken
  -> FollowParams
  -> InstagramT m (Envelope [User])
getFollowsParams uid token fp =
  getGetEnvelopeM ["/v1/users/",uid,"/follows"] token fp

-- | Get the list of users this user is followed by.
getFollowedByParams :: (MonadBaseControl IO m, MonadResource m) => UserID
  -> Maybe OAuthToken
  -> FollowParams
  -> InstagramT m (Envelope [User])
getFollowedByParams uid token fp =
  getGetEnvelopeM ["/v1/users/",uid,"/followed-by"] token fp

-- | List the users who have requested this user's permission to follow.
getRequestedBy ::     (MonadBaseControl IO m, MonadResource m) =>
   OAuthToken
  -> InstagramT m (Envelope [User])
getRequestedBy token  =getGetEnvelope ["/v1/users/self/requested-by"] token ([]::HT.Query)

-- | Get information about a relationship to another user.
getRelationship ::     (MonadBaseControl IO m, MonadResource m) => UserID
  -> OAuthToken
  -> InstagramT m (Envelope Relationship)
getRelationship uid token  =getGetEnvelope ["/v1/users/",uid,"/relationship"] token ([]::HT.Query)

-- | relationship action
data RelationShipAction =  Follow
  | Unfollow
  | Block
  | Unblock
  | Approve
  | Deny
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

instance HT.QueryLike RelationShipAction where
  toQuery a=
    ["action" ?+ map toLower (show a)]

-- | Modify the relationship between the current user and the target user.
setRelationShip :: (MonadBaseControl IO m, MonadResource m) => UserID
  -> OAuthToken
  -> RelationShipAction
  -> InstagramT m (Envelope Relationship)
setRelationShip uid=getPostEnvelope ["/v1/users/",uid,"/relationship"]
