{-# LANGUAGE FlexibleContexts #-}
-- | geographies handling
-- <http://instagram.com/developer/endpoints/geographies/#>
module Instagram.Geographies (
  GeographyMediaParams(..)
  ,getGeographyRecentMedia
)where

import Instagram.Monad
import Instagram.Types

import qualified Network.HTTP.Types as HT
import qualified Data.Text as T (Text)
import Data.Default
import Data.Typeable
import Data.Maybe (isJust)

-- | Parameters for call to recent media in geography search
data GeographyMediaParams = GeographyMediaParams {
    gmpCount :: Maybe Integer
    ,gmpMinId :: Maybe T.Text
  }
  deriving (Show,Typeable)

instance Default GeographyMediaParams where
  def=GeographyMediaParams Nothing Nothing

instance HT.QueryLike GeographyMediaParams where
  toQuery (GeographyMediaParams cnt minI)=filter (isJust .snd)
    ["count" ?+ cnt
    ,"min_id" ?+ minI   ]

-- | Get very recent media from a geography subscription that you created
getGeographyRecentMedia :: (MonadBaseControl IO m, MonadResource m) =>
  GeographyID
  -> GeographyMediaParams
  ->InstagramT m (Envelope [Media])
getGeographyRecentMedia gid = getGetEnvelopeM ["/v1/geographies/", gid,"/media/recent"] Nothing
