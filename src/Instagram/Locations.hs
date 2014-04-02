{-# LANGUAGE FlexibleContexts #-}
-- | locations handling
-- <http://instagram.com/developer/endpoints/locations/#>
module Instagram.Locations (
  getLocation
  ,LocationMediaParams(..)
  ,getLocationRecentMedia
  ,LocationSearchParams(..)
  ,searchLocations
) where

import Instagram.Monad
import Instagram.Types

import qualified Network.HTTP.Types as HT
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T (Text)
import Data.Typeable
import Data.Default
import Data.Maybe (isJust)


-- | Get information about a location.
getLocation :: (MonadBaseControl IO m, MonadResource m) =>
  LocationID
  -> Maybe OAuthToken
  ->InstagramT m (Envelope (Maybe Location))
getLocation lid token=getGetEnvelopeM ["/v1/locations/",lid] token ([]::HT.Query)


-- | Parameters for call to recent media in location search
data LocationMediaParams = LocationMediaParams {
    lmspMaxTimestamp :: Maybe POSIXTime,
    lmspMinTimestamp :: Maybe POSIXTime,
    lmspMaxID :: Maybe T.Text,
    lmspMinId :: Maybe T.Text
  }
  deriving (Show,Typeable)

instance Default LocationMediaParams where
  def=LocationMediaParams Nothing Nothing Nothing Nothing

instance HT.QueryLike LocationMediaParams where
  toQuery (LocationMediaParams maxT minT maxI minI)=filter (isJust .snd)
    ["max_timestamp" ?+ maxT
    ,"min_timestamp" ?+ minT
    ,"max_id" ?+ maxI
    ,"min_id" ?+ minI   ]

-- | Get a list of recent media objects from a given location.
getLocationRecentMedia ::  (MonadBaseControl IO m, MonadResource m) =>
  LocationID
  -> Maybe OAuthToken
  -> LocationMediaParams
  ->InstagramT m (Envelope [Media])
getLocationRecentMedia lid = getGetEnvelopeM ["/v1/locations/", lid,"/media/recent"]


-- | Parameters for call to media search
data LocationSearchParams = LocationSearchParams {
    lspLatitude ::  Maybe Double
    ,lspLongitude ::  Maybe Double
    ,lspDistance ::  Maybe Integer
    ,lspFoursquareIDv2 :: Maybe T.Text
    ,lspFoursquareID :: Maybe T.Text
  }
  deriving (Show,Typeable)

instance Default LocationSearchParams where
  def=LocationSearchParams Nothing Nothing Nothing Nothing Nothing

instance HT.QueryLike LocationSearchParams where
  toQuery (LocationSearchParams lat long dis fsidV2 fsid )=filter (isJust .snd)
    ["lat" ?+ lat
    ,"lng" ?+ long
    ,"distance" ?+ dis
    ,"foursquare_v2_id" ?+ fsidV2
    ,"foursquare_id" ?+ fsid
   ]

-- | Search for a location by geographic coordinate.
searchLocations ::  (MonadBaseControl IO m, MonadResource m) =>
   Maybe OAuthToken
  -> LocationSearchParams
  ->InstagramT m (Envelope [Location])
searchLocations = getGetEnvelopeM ["/v1/locations/search"]
