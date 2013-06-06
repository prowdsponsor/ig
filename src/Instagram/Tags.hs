{-# LANGUAGE FlexibleContexts #-}
-- | tag operations
module Instagram.Tags (
  TagName
  ,getTag
  ,RecentTagParams(..)
  ,getRecentTagged
  ,searchTags
)where

import Instagram.Monad
import Instagram.Types

import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT 

import qualified Data.Text as T (Text,concat)
import Data.Conduit
import Data.Typeable
import Data.Default
import Data.Maybe (isJust)

-- | Tag Name
type TagName = T.Text

-- | get a tag by name
getTag :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe AccessToken
  ->InstagramT m (Envelope (Maybe Tag))
getTag name token=do
  let url=TE.encodeUtf8 $ T.concat ["/v1/tags/",name]
  addTokenM token ([]::HT.Query) >>= getGetRequest url >>= getJSONEnvelope
  
-- | get media recently tagged by the given tag
getRecentTagged :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe AccessToken
  -> RecentTagParams
  ->InstagramT m (Envelope [Media])
getRecentTagged name token rtp=do
   let url=TE.encodeUtf8 $ T.concat ["/v1/tags/",name,"/media/recent/"]
   addTokenM token rtp >>= getGetRequest url >>= getJSONEnvelope

-- | search tags with given prefix   
searchTags :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> Maybe AccessToken
  ->InstagramT m (Envelope [Tag])
searchTags name token=
 addTokenM token ([("q",TE.encodeUtf8 name)]::HT.SimpleQuery) >>= getGetRequest "/v1/tags/search" >>= getJSONEnvelope   
   
-- | parameters for tag pagination   
data RecentTagParams=RecentTagParams{
  rtpMaxID :: Maybe T.Text
  ,rtpMinID :: Maybe T.Text
  }deriving (Show,Typeable)
  
instance Default RecentTagParams where
  def=RecentTagParams Nothing Nothing
 
instance HT.QueryLike RecentTagParams where
  toQuery (RecentTagParams maxI minI)=filter (isJust .snd) 
    [("max_tag_id",fmap TE.encodeUtf8 maxI)
    ,("min_tag_id",fmap TE.encodeUtf8 minI)]

