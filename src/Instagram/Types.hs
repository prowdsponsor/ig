{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
-- | Types definitions
module Instagram.Types (
  Credentials(..)
  ,clientIDBS
  ,clientSecretBS
  ,OAuthToken(..)
  ,AccessToken(..)
  ,User(..)
  ,Scope(..)
  ,IGException(..)
  ,Envelope(..)
  ,IGError
  ,Pagination(..)
  ,Media(..)
)where

import Control.Applicative
import Data.Text
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)

import Data.Aeson
import Control.Monad (mzero)

import qualified Data.Text.Encoding as TE
import Control.Exception.Base (Exception)
import Data.Time.Clock.POSIX (POSIXTime)


-- | the app credentials
data Credentials = Credentials {
  cClientID :: Text -- ^ client id
  ,cClientSecret :: Text -- ^ client secret
  }
  deriving (Show,Read,Eq,Ord,Typeable)
      
-- | get client id in ByteString form
clientIDBS :: Credentials -> ByteString
clientIDBS=TE.encodeUtf8 . cClientID

-- | get client secret in ByteString form
clientSecretBS :: Credentials -> ByteString
clientSecretBS=TE.encodeUtf8 . cClientSecret
      
-- | the oauth token returned after authentication
data OAuthToken = OAuthToken {
  oaAccessToken :: AccessToken -- ^ the access token
  ,oaUser :: User -- ^ the user structure returned
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON OAuthToken  where
    toJSON oa=object ["access_token" .= oaAccessToken oa, "user" .= oaUser oa] 

-- | from json as per Instagram format        
instance FromJSON OAuthToken where
    parseJSON (Object v) =OAuthToken <$>
                         v .: "access_token" <*>
                         v .: "user" 
    parseJSON _= mzero

-- | the access token is simply a Text
newtype AccessToken=AccessToken Text
    deriving (Eq, Ord, Read, Show, Typeable)
 
-- | simple string        
instance ToJSON AccessToken  where
        toJSON (AccessToken at)=String at         

-- | simple string
instance FromJSON  AccessToken where
        parseJSON (String s)=pure $ AccessToken s
        parseJSON _= mzero

-- | the User partial profile returned by the authentication        
data User = User {
        uID :: Text,
        uUsername :: Text,
        uFullName :: Text,
        uProfilePicture :: Maybe Text
        }        
        deriving (Show,Read,Eq,Ord,Typeable)
    
-- | to json as per Instagram format    
instance ToJSON User  where
    toJSON u=object ["id" .= uID u, "username" .= uUsername u , "full_name" .= uFullName u, "profile_picture" .= uProfilePicture u] 

-- | from json as per Instagram format
instance FromJSON User where
    parseJSON (Object v) =User <$>
                         v .: "id" <*>
                         v .: "username" <*>
                         v .: "full_name" <*>
                         v .:? "profile_picture"
    parseJSON _= mzero
    
-- | the scopes of the authentication
data Scope=Basic | Comments | Relationships | Likes
        deriving (Show,Read,Eq,Ord,Enum,Bounded,Typeable)

-- | an error returned to us by Instagram
data IGError = IGError {
  igeCode :: Int
  ,igeType :: Maybe Text
  ,igeMessage :: Maybe Text
  }
  deriving (Show,Read,Eq,Ord,Typeable)
  
 -- | to json as per Instagram format    
instance ToJSON IGError  where
    toJSON e=object ["code" .= igeCode e, "error_type" .= igeType e , "error_message" .= igeMessage e] 

-- | from json as per Instagram format
instance FromJSON IGError where
    parseJSON (Object v) =IGError <$>
                         v .: "code" <*>
                         v .:? "error_type" <*>
                         v .:? "error_message"
    parseJSON _= mzero

-- | an exception that a call to instagram may throw
data IGException = JSONException String -- ^ JSON parsingError
  | IGAppException IGError -- ^ application exception
  deriving (Show,Typeable)
  
instance Exception IGException 

data Envelope d=Envelope{
  eMeta :: IGError,
  eData :: d,
  ePagination :: Maybe Pagination
  }
  deriving (Show,Read,Eq,Ord,Typeable)
  
-- | to json as per Instagram format    
instance (ToJSON d)=>ToJSON (Envelope d)  where
    toJSON e=object ["meta" .= eMeta e, "data" .= eData e, "pagination" .= ePagination e]  
  
-- | from json as per Instagram format
instance (FromJSON d)=>FromJSON (Envelope d) where
    parseJSON (Object v) =Envelope <$>
                         v .: "meta" <*>
                         v .: "data" <*>
                         v .:? "pagination"
    parseJSON _= mzero
  
data Pagination = Pagination {
   pNextUrl :: Maybe Text
   ,pNextMaxID :: Maybe Text
   }
  deriving (Show,Read,Eq,Ord,Typeable)
  
 -- | to json as per Instagram format    
instance ToJSON Pagination  where
    toJSON p=object ["next_url" .= pNextUrl p, "next_max_id" .= pNextMaxID p] 

-- | from json as per Instagram format
instance FromJSON Pagination where
    parseJSON (Object v) =Pagination <$>
                         v .:? "next_url" <*>
                         v .:? "next_max_id"
    parseJSON _= mzero  
  
data Media = Media {
  mID :: Text
  ,mCreated :: POSIXTime
  ,mType :: Text
  ,mUsers :: [User]
  ,mFilter :: Text
  ,mTags :: [Text]
  }  
  deriving (Show,Eq,Ord,Typeable)
  
-- | to json as per Instagram format    
instance ToJSON Media  where
    toJSON m=object ["id" .= mID m,"created_time" .= (toJSON $ show $ round $ mCreated m),"type" .= mType m,"users_in_photo" .= mUsers m, "filter" .= mFilter m,"tags" .= mTags m] 

-- | from json as per Instagram format
instance FromJSON Media where
    parseJSON (Object v) =do
      ct::String<-v .: "created_time"
      Media <$>
                         v .: "id" <*>
                         (pure $ fromIntegral $ read ct) <*>
                         v .: "type" <*>
                         v .: "users_in_photo" <*>
                         v .: "filter" <*>
                         v .: "tags"
    parseJSON _= mzero  
  
--{
--        "comments": {
--            "data": [],
--            "count": 0
--        },
--        "caption": {
--            "created_time": "1296710352",
--            "text": "Inside le truc #foodtruck",
--            "from": {
--                "username": "kevin",
--                "full_name": "Kevin Systrom",
--                "type": "user",
--                "id": "3"
--            },
--            "id": "26621408"
--        },
--        "likes": {
--            "count": 15,
--            "data": [{
--                "username": "mikeyk",
--                "full_name": "Mike Krieger",
--                "id": "4",
--                "profile_picture": "..."
--            }, {...subset of likers...}]
--        },
--        "link": "http://instagr.am/p/BWrVZ/",
--        "user": {
--            "username": "kevin",
--            "profile_picture": "http://distillery.s3.amazonaws.com/profiles/profile_3_75sq_1295574122.jpg",
--            "id": "3"
--        },
--        "created_time": "1296710327",
--        "images": {
--            "low_resolution": {
--                "url": "http://distillery.s3.amazonaws.com/media/2011/02/02/6ea7baea55774c5e81e7e3e1f6e791a7_6.jpg",
--                "width": 306,
--                "height": 306
--            },
--            "thumbnail": {
--                "url": "http://distillery.s3.amazonaws.com/media/2011/02/02/6ea7baea55774c5e81e7e3e1f6e791a7_5.jpg",
--                "width": 150,
--                "height": 150
--            },
--            "standard_resolution": {
--                "url": "http://distillery.s3.amazonaws.com/media/2011/02/02/6ea7baea55774c5e81e7e3e1f6e791a7_7.jpg",
--                "width": 612,
--                "height": 612
--            }
--        },
--        "type": "image",
--        "users_in_photo": [],
--        "filter": "Earlybird",
--        "tags": ["foodtruck"],
--        "id": "22721881",
--        "location": {
--            "latitude": 37.778720183610183,
--            "longitude": -122.3962783813477,
--            "id": "520640",
--            "street_address": "",
--            "name": "Le Truc"
--        }
--    }

