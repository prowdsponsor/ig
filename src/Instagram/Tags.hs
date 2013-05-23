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
import Data.ByteString.Char8 (pack)
import Data.Maybe (isJust)

-- | Tag Name
type TagName = T.Text

-- | get a tag by name
getTag :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> AccessToken
  ->InstagramT m (Envelope (Maybe Tag))
getTag name token=do
  let url=TE.encodeUtf8 $ T.concat ["/v1/tags/",name]
  getGetRequest url (addToken token ([]::HT.Query))>>= getJSONEnvelope
  
-- | get media recently tagged by the given tag
getRecentTagged :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> AccessToken
  -> RecentTagParams
  ->InstagramT m (Envelope [Media])
getRecentTagged name token rtp=do
   let url=TE.encodeUtf8 $ T.concat ["/v1/tags/",name,"/media/recent/"]
   getGetRequest url (addToken token rtp)>>= getJSONEnvelope

-- | search tags with given prefix   
searchTags :: (MonadBaseControl IO m, MonadResource m) =>
  TagName
  -> AccessToken
  ->InstagramT m (Envelope [Tag])
searchTags name token=
 getGetRequest "/v1/tags/search" (addToken token ([("q",TE.encodeUtf8 name)]::HT.SimpleQuery))>>= getJSONEnvelope   
   
-- | parameters for tag pagination   
data RecentTagParams=RecentTagParams{
  rtpMaxID :: Maybe String
  ,rtpMinID :: Maybe String
  }deriving (Show,Typeable)
  
instance Default RecentTagParams where
  def=RecentTagParams Nothing Nothing
 
instance HT.QueryLike RecentTagParams where
  toQuery (RecentTagParams maxI minI)=filter (isJust .snd) 
    [("max_id",fmap (pack . show) maxI)
    ,("min_id",fmap (pack . show) minI)]

