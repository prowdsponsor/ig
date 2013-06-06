{-# LANGUAGE FlexibleContexts #-}
-- | Users end point handling
module Instagram.Users (
  getUser
  ,SelfFeedParams(..)
  ,getSelfFeed
  ,RecentParams(..)
  ,getRecent
  ,SelfLikedParams(..)
  ,getSelfLiked
  ,UserSearchParams(..)
  ,searchUsers
) where

import Instagram.Monad
import Instagram.Types

import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)

import qualified Network.HTTP.Types as HT
import Data.ByteString.Char8 (pack)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text)
import Data.Conduit
import qualified Data.Text.Encoding as TE
import Data.Default

-- | User ID
type UserID = T.Text

-- | get user details
getUser ::     (MonadBaseControl IO m, MonadResource m) => UserID 
  -> Maybe AccessToken
  -> InstagramT m (Envelope (Maybe User))
getUser uid token  =getGetEnvelopeM ["/v1/users/",uid] token ([]::HT.Query)
  
-- | Parameters for call to recent media
data SelfFeedParams = SelfFeedParams {
    sfpCount :: Maybe Int,
    sfpMaxID :: Maybe T.Text,
    sfpMinId :: Maybe T.Text
  }
  deriving (Show,Typeable)
  
instance Default SelfFeedParams where
  def=SelfFeedParams Nothing Nothing Nothing
  
instance HT.QueryLike SelfFeedParams where
  toQuery (SelfFeedParams c maxI minI)=filter (isJust .snd) 
    [("count",fmap (pack . show) c)
    ,("max_id",fmap TE.encodeUtf8 maxI)
    ,("min_id",fmap TE.encodeUtf8 minI)]
    
    
-- | get recent media    
getSelfFeed :: (MonadBaseControl IO m, MonadResource m) =>
  OAuthToken
  -> SelfFeedParams 
  -> InstagramT m (Envelope [Media])
getSelfFeed token=getGetEnvelope ["/v1/users/self/feed/"] (oaAccessToken token) 

-- | Parameters for call to recent media
data RecentParams = RecentParams {
    rpCount :: Maybe Int,
    rpMaxTimestamp :: Maybe POSIXTime,
    rpMinTimestamp :: Maybe POSIXTime,
    rpMaxID :: Maybe T.Text,
    rpMinId :: Maybe T.Text
  }
  deriving (Show,Typeable)
  
instance Default RecentParams where
  def=RecentParams Nothing Nothing Nothing Nothing Nothing
  
instance HT.QueryLike RecentParams where
  toQuery (RecentParams c maxT minT maxI minI)=filter (isJust .snd) 
    [("count",fmap (pack . show) c)
    ,("max_timestamp",fmap (pack . show . round) maxT)
    ,("min_timestamp",fmap (pack . show . round) minT)
    ,("max_id",fmap TE.encodeUtf8 maxI)
    ,("min_id",fmap TE.encodeUtf8 minI)]
    
    
-- | get recent media    
getRecent :: (MonadBaseControl IO m, MonadResource m) => UserID 
  -> AccessToken
  -> RecentParams 
  -> InstagramT m (Envelope [Media])
getRecent uid=getGetEnvelope ["/v1/users/",uid,"/media/recent/"]

-- | parameters for self liked call
data SelfLikedParams = SelfLikedParams {
  slpCount :: Maybe Int,
  slpMaxLikeID :: Maybe T.Text
  }
  deriving (Show,Typeable)
  
instance Default SelfLikedParams where
  def=SelfLikedParams Nothing Nothing
  
instance HT.QueryLike SelfLikedParams where
  toQuery (SelfLikedParams c maxI)=filter (isJust .snd) 
    [("count",fmap (pack . show) c) 
    ,("max_like_id",fmap TE.encodeUtf8 maxI)] 

-- | get media liked by logged in user
getSelfLiked :: (MonadBaseControl IO m, MonadResource m) => OAuthToken 
  -> SelfLikedParams
  -> InstagramT m (Envelope [Media]) 
getSelfLiked token =getGetEnvelope ["/v1/users/self/media/liked"] (oaAccessToken token) 

-- | parameters for self liked call
data UserSearchParams = UserSearchParams {
  uspQuery :: T.Text,
  uspCount :: Maybe Int
  }
  deriving (Show,Typeable)
  
instance HT.QueryLike UserSearchParams where
  toQuery (UserSearchParams q c )=filter (isJust .snd) 
    [("count",fmap (pack . show) c) -- does not seem to be taken into account...
    ,("q",Just $ TE.encodeUtf8 q)] 

-- | get media liked by logged in user
searchUsers :: (MonadBaseControl IO m, MonadResource m) => Maybe AccessToken 
  -> UserSearchParams
  -> InstagramT m (Envelope [User]) 
searchUsers =getGetEnvelopeM ["/v1/users/search"]
