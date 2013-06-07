{-# LANGUAGE FlexibleContexts #-}
-- | tag operations
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
import Data.Conduit
import Data.Typeable
import Data.Default
import Data.Maybe (isJust)

-- | get a tag by name
getTag :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe OAuthToken
  ->InstagramT m (Envelope (Maybe Tag))
getTag name token=getGetEnvelopeM ["/v1/tags/",name] token ([]::HT.Query)
  
-- | get media recently tagged by the given tag
getRecentTagged :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe OAuthToken
  -> RecentTagParams
  ->InstagramT m (Envelope [Media])
getRecentTagged name=getGetEnvelopeM ["/v1/tags/",name,"/media/recent/"]

-- | search tags with given prefix   
searchTags :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe OAuthToken
  ->InstagramT m (Envelope [Tag])
searchTags name token=getGetEnvelopeM ["/v1/tags/search"] token ["q" ?+ name]
   
-- | parameters for tag pagination   
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

