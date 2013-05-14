module Instagram.Types (
  Credentials(..)
  ,clientIDBS
  ,clientSecretBS
  ,OAuthToken
  ,AccessToken
  ,User
  ,Scope(..)
)where

import Control.Applicative
import Data.Text
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)

import Data.Aeson
import Control.Monad (mzero)

import qualified Data.Text.Encoding as TE

data Credentials = Credentials {
        cClientID :: Text,
        cClientSecret :: Text
        }
        deriving (Show,Read,Eq,Ord,Typeable)
      
clientIDBS :: Credentials -> ByteString
clientIDBS=TE.encodeUtf8 . cClientID

clientSecretBS :: Credentials -> ByteString
clientSecretBS=TE.encodeUtf8 . cClientSecret
      
data OAuthToken = OAuthToken {
        oaAccessToken :: AccessToken,
        oaUser :: User
        }
        deriving (Show,Read,Eq,Ord,Typeable)

instance ToJSON OAuthToken  where
    toJSON oa=object ["access_token" .= oaAccessToken oa, "user" .= oaUser oa] 
        
instance FromJSON OAuthToken where
    parseJSON (Object v) =OAuthToken <$>
                         v .: "access_token" <*>
                         v .: "user" 
    parseJSON _= mzero


newtype AccessToken=AccessToken Text
    deriving (Eq, Ord, Read, Show, Typeable)
         
instance ToJSON AccessToken  where
        toJSON (AccessToken at)=String at         
        
instance FromJSON  AccessToken where
        parseJSON (String s)=pure $ AccessToken s
        parseJSON _= mzero
         
data User = User {
        uID :: Text,
        uUsername :: Text,
        uFullName :: Text,
        uProfilePicture :: Maybe Text
        }        
        deriving (Show,Read,Eq,Ord,Typeable)
    
instance ToJSON User  where
    toJSON u=object ["id" .= uID u, "username" .= uUsername u , "full_name" .= uFullName u, "profile_picture" .= uProfilePicture u] 
        
instance FromJSON User where
    parseJSON (Object v) =User <$>
                         v .: "id" <*>
                         v .: "username" <*>
                         v .: "full_name" <*>
                         v .:? "profile_picture"
    parseJSON _= mzero
    
data Scope=Basic | Comments | Relationships | Likes
        deriving (Show,Read,Eq,Ord,Enum,Bounded,Typeable)
