{-# LANGUAGE FlexibleContexts #-}
-- | tag operations
-- <http://instagram.com/developer/endpoints/tags/#>
module Instagram.Tags (
  getTag
  ,RecentTagParams(..)
  ,getRecentTagged
  ,searchTags
)where

import Instagram.Monad
import Instagram.Types

import qualified Network.HTTP.Types as HT

import qualified Data.Text as T (Text)
import Data.Typeable
import Data.Default
import Data.Maybe (isJust)

-- | Get information about a tag object.
getTag :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe OAuthToken
  ->InstagramT m (Envelope (Maybe Tag))
getTag name token=getGetEnvelopeM ["/v1/tags/",name] token ([]::HT.Query)

-- | Get a list of recently tagged media.
getRecentTagged :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe OAuthToken
  -> RecentTagParams
  ->InstagramT m (Envelope [Media])
getRecentTagged name=getGetEnvelopeM ["/v1/tags/",name,"/media/recent/"]

-- | Search for tags by name.
searchTags :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe OAuthToken
  ->InstagramT m (Envelope [Tag])
searchTags name token=getGetEnvelopeM ["/v1/tags/search"] token ["q" ?+ name]

-- | parameters for recent tag pagination
data RecentTagParams=RecentTagParams{
  rtpMaxID :: Maybe T.Text
  ,rtpMinID :: Maybe T.Text
  }deriving (Show,Typeable)

instance Default RecentTagParams where
  def=RecentTagParams Nothing Nothing

instance HT.QueryLike RecentTagParams where
  toQuery (RecentTagParams maxI minI)=filter (isJust .snd)
    ["max_tag_id" ?+ maxI
    ,"min_tag_id" ?+ minI]
