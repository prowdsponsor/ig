-- | Types definitions
module Instagram.Types (
  Credentials(..)
  ,clientIDBS
  ,clientSecretBS
  ,OAuthToken
  ,AccessToken
  ,User
  ,Scope(..)
  ,ISException(..)
)where

import Control.Applicative
import Data.Text
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)

import Data.Aeson
import Control.Monad (mzero)

import qualified Data.Text.Encoding as TE
import Control.Exception.Base (Exception)

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
data ISError = ISError {
  iseCode :: Int
  ,iseType :: Text
  ,iseMessage :: Text
  }
  deriving (Show,Read,Eq,Ord,Typeable)
  
 -- | to json as per Instagram format    
instance ToJSON ISError  where
    toJSON e=object ["code" .= iseCode e, "error_type" .= iseType e , "error_message" .= iseMessage e] 

-- | from json as per Instagram format
instance FromJSON ISError where
    parseJSON (Object v) =ISError <$>
                         v .: "code" <*>
                         v .: "error_type" <*>
                         v .: "error_message"
    parseJSON _= mzero

-- | an exception that a call to instagram may throw
data ISException = JSONException String -- ^ JSON parsingError
  | ISAppException ISError -- ^ application exception
  deriving (Show,Typeable)
  
instance Exception ISException 
