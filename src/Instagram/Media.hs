{-# LANGUAGE FlexibleContexts #-}
module Instagram.Media (
  getMedia
  ,getPopularMedia
  ,MediaSearchParams(..)
  ,searchMedia
)where

import Instagram.Monad
import Instagram.Types

import Data.Conduit
import qualified Network.HTTP.Types as HT
import Data.Default (Default(..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import Data.ByteString.Char8(pack)

-- | get information about a media object
getMedia ::     (MonadBaseControl IO m, MonadResource m) => MediaID 
  -> Maybe OAuthToken
  -> InstagramT m (Envelope (Maybe Media))
getMedia mid token  =getGetEnvelopeM ["/v1/media/",mid] token ([]::HT.Query)

-- | get a list of what media is most popular at the moment
getPopularMedia ::     (MonadBaseControl IO m, MonadResource m) =>
  Maybe OAuthToken
  -> InstagramT m (Envelope [Media])
getPopularMedia token  =getGetEnvelopeM ["/v1/media/popular"] token ([]::HT.Query)  

-- | Parameters for call to recent media
data MediaSearchParams = MediaSearchParams {
    mspLatitude ::  Maybe Double
    ,mspLongitude ::  Maybe Double
    ,mspDistance ::  Maybe Integer
    ,mspMaxTimestamp :: Maybe POSIXTime
    ,mspMinTimestamp :: Maybe POSIXTime
  }
  deriving (Show,Typeable)
  
instance Default MediaSearchParams where
  def=MediaSearchParams Nothing Nothing Nothing Nothing Nothing
  
instance HT.QueryLike MediaSearchParams where
  toQuery (MediaSearchParams lat long dis maxT minT )=filter (isJust .snd) 
    [("lat",fmap (pack . show) lat)
    ,("lng",fmap (pack . show) long)
    ,("distance",fmap (pack . show) dis)
    ,("max_timestamp",fmap (pack . show . round) maxT)
    ,("min_timestamp",fmap (pack . show . round) minT)
   ]
    
-- | search for media in a given area
searchMedia :: (MonadBaseControl IO m, MonadResource m) =>
  Maybe OAuthToken
  -> MediaSearchParams
  -> InstagramT m (Envelope [Media])
searchMedia =getGetEnvelopeM ["/v1/media/search"]
  