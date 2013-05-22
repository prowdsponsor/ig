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
  ,Location(..)
  ,ImageData(..)
  ,Images(..)
  ,Caption(..)
  ,Collection(..)
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

-- | envelope for Instagram response
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

-- | pagination info for responses that can return a lot of data  
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
  
-- | instagram media object
data Media = Media {
  mID :: Text
  ,mCaption :: Caption
  ,mLink :: Text
  ,mUser :: User 
  ,mCreated :: POSIXTime
  ,mImages :: Images
  ,mType :: Text
  ,mUsersInPhoto :: [User]
  ,mFilter :: Text
  ,mTags :: [Text]
  ,mLocation :: Location
  ,mComments :: Collection Caption
  ,mLikes :: Collection User
  ,mUserHasLiked :: Bool
  ,mAttribution :: Maybe Object -- ^ seems to be open format https://groups.google.com/forum/?fromgroups#!topic/instagram-api-developers/KvGH1cnjljQ
  }  
  deriving (Show,Eq,Typeable)
  
-- | to json as per Instagram format    
instance ToJSON Media  where
    toJSON m=object ["id" .= mID m,"caption" .= mCaption m,"user".= mUser m,"link" .= mLink m, "created_time" .= toJSON (show ((round $ mCreated m) :: Integer))
      ,"images" .= mImages m,"type" .= mType m,"users_in_photo" .= mUsersInPhoto m, "filter" .= mFilter m,"tags" .= mTags m
      ,"location" .= mLocation m,"comments" .= mComments m,"likes" .= mLikes m,"user_has_liked" .= mUserHasLiked m,"attribution" .= mAttribution m] 

-- | from json as per Instagram format
instance FromJSON Media where
    parseJSON (Object v) =do
      ct::String<-v .: "created_time"
      Media <$>
                         v .: "id" <*>
                         v .: "caption" <*>
                         v .: "link" <*>
                         v .: "user" <*>
                         pure (fromIntegral (read ct::Integer)) <*>
                         v .: "images" <*>
                         v .: "type" <*>
                         v .: "users_in_photo" <*>
                         v .: "filter" <*>
                         v .: "tags" <*>
                         v .: "location" <*>
                         v .:? "comments" .!= Collection 0 [] <*>
                         v .:? "likes" .!= Collection 0 [] <*>
                         v .:? "user_has_liked" .!= False <*>
                         v .:? "attribution"
    parseJSON _= mzero  

-- | geographical location info
data Location = Location {
  lID :: Maybe Text
  ,lLatitude :: Double
  ,lLongitude :: Double
  ,lStreetAddress :: Maybe Text
  ,lName :: Maybe Text
  }  
  deriving (Show,Eq,Ord,Typeable)
  
-- | to json as per Instagram format      
instance ToJSON Location where
  toJSON l=object ["id" .= lID l,"latitude" .= lLatitude l,"longitude" .= lLongitude l, "street_address" .= lStreetAddress l,"name" .= lName l]
  
-- | from json as per Instagram format
instance FromJSON Location where
  parseJSON (Object v) = Location <$>
    v .:? "id" <*>
    v .: "latitude" <*>
    v .: "longitude" <*>
    v .:? "street_address" <*>
    v .:? "name"
  parseJSON _= mzero
  
-- | data for a single image
data ImageData = ImageData {
  idURL :: Text,
  idWidth :: Integer,
  idHeight :: Integer
  }  
  deriving (Show,Eq,Ord,Typeable)
  
-- | to json as per Instagram format      
instance ToJSON ImageData where
  toJSON i=object ["url" .= idURL i,"width" .= idWidth i,"height" .= idHeight i]
  
-- | from json as per Instagram format
instance FromJSON ImageData where
  parseJSON (Object v) = ImageData <$>
    v .: "url" <*>
    v .: "width" <*>
    v .: "height"
  parseJSON _= mzero  
  
-- | different images for the same media
data Images = Images {
  iLowRes :: ImageData
  ,iThumbnail :: ImageData
  ,iStandardRes :: ImageData
  }
  deriving (Show,Eq,Ord,Typeable) 
 
-- | to json as per Instagram format      
instance ToJSON Images where
  toJSON i=object ["low_resolution" .= iLowRes i,"thumbnail" .= iThumbnail i,"standard_resolution" .= iStandardRes i]
  
-- | from json as per Instagram format
instance FromJSON Images where
  parseJSON (Object v) = Images <$>
    v .: "low_resolution" <*>
    v .: "thumbnail" <*>
    v .: "standard_resolution"
  parseJSON _= mzero  
 
-- | caption on a medium
data Caption = Caption {
  cID :: Text
  ,cCreated :: POSIXTime
  ,cText :: Text
  ,cFrom :: User
  }
  deriving (Show,Eq,Ord,Typeable) 

-- | to json as per Instagram format    
instance ToJSON Caption  where
    toJSON c=object ["id" .= cID c,"created_time" .= toJSON (show ((round $ cCreated c) :: Integer))
      ,"text" .= cText c,"from" .= cFrom c] 

-- | from json as per Instagram format
instance FromJSON Caption where
    parseJSON (Object v) =do
      ct::String<-v .: "created_time"
      Caption <$>
                         v .: "id" <*>
                         pure (fromIntegral (read ct::Integer)) <*>
                         v .: "text" <*>
                         v .: "from" 
    parseJSON _= mzero  

-- | a collection of items (count + data)
-- data can only be a subset
data Collection a= Collection {
  cCount :: Integer
  ,cData :: [a]
  }
  deriving (Show,Eq,Ord,Typeable) 

 
-- | to json as per Instagram format    
instance (ToJSON a)=>ToJSON (Collection a)  where
    toJSON igc=object ["count" .= cCount igc,"data" .= cData igc]

-- | from json as per Instagram format
instance (FromJSON a)=>FromJSON (Collection a) where
    parseJSON (Object v) = Collection <$>
                         v .: "count" <*>
                         v .: "data" 
    parseJSON _= mzero  
 
