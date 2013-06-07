{-# LANGUAGE FlexibleContexts #-}
-- | Real time subscription management
-- <http://instagram.com/developer/realtime/#>
module Instagram.RealTime (
  createSubscription
  ,listSubscriptions
  ,deleteSubscriptions
  ,SubscriptionRequest(..)
  ,SubscriptionParams(..)
  ,DeletionParams(..)
  ,verifySignature
)

where

import Instagram.Monad
import Instagram.Types

import Data.Text (Text)
import Data.Typeable
import qualified Network.HTTP.Types as HT 
import Data.Maybe (isJust)
import Data.Conduit
import Data.Aeson (Value(..))

import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Classes as Crypto
import qualified Crypto.HMAC as Crypto
import Crypto.Hash.CryptoAPI (SHA1)
import Control.Monad (liftM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | create a subscription 
createSubscription :: (MonadBaseControl IO m, MonadResource m) => 
  SubscriptionParams -- ^ the subscription parameters
  -> InstagramT m (Envelope Subscription) -- ^ the created subscription
createSubscription params=do
  let url="/v1/subscriptions/"
  addClientInfos params >>= getPostRequest url >>= getJSONEnvelope  

-- | list all subscriptions for the application
listSubscriptions :: (MonadBaseControl IO m, MonadResource m) => 
  InstagramT m (Envelope [Subscription]) -- ^ the ID of the subscription
listSubscriptions =do
  let url="/v1/subscriptions/"
  addClientInfos ([]::HT.Query) >>= getGetRequest url >>= getJSONEnvelope  

-- | delete subscriptions based on criteria
deleteSubscriptions :: (MonadBaseControl IO m, MonadResource m) => 
  DeletionParams -- ^ the parameters for the deletion
  -> InstagramT m (Envelope Value) -- ^ the ID of the subscription
deleteSubscriptions params=do
  let url="/v1/subscriptions/"
  addClientInfos params >>= getDeleteRequest url >>= getJSONEnvelope  
 
-- | parameters for the subscription creation
data SubscriptionParams= SubscriptionParams {
  spRequest :: SubscriptionRequest -- ^ the actual subscription request
  ,spCallback :: CallbackUrl -- ^ the url Instagram will post notifications to
  ,spAspect :: Aspect  -- ^ the subscription aspect
  ,spVerifyToken :: Maybe Text -- ^ the verification token
  }
  deriving (Read,Show,Eq,Ord,Typeable)

-- | to HTTP query  
instance HT.QueryLike SubscriptionParams where
  toQuery (SubscriptionParams req cb (Aspect asp) tok)=filter (isJust .snd) $ HT.toQuery req ++ 
    ["aspect" ?+ asp
    ,"callback_url" ?+ cb
    ,"verify_token" ?+ tok]

-- | details of subscription request  
data SubscriptionRequest
  -- | when a user uploads a picture
  =UserRequest
  -- | when a picture is tagged with the given tag
  | TagRequest {
    trTag ::Text
    }
  -- | when a picture is tagged with a specific location
  | LocationRequest {
    lrID :: Text
     }
  -- | when a picture is tagged with a location inside the given region
  | GeographyRequest {
    grLatitude :: Double
    ,grLongitude :: Double
    ,grRadius :: Integer
    }  
    deriving (Read,Show,Eq,Ord,Typeable)

-- | to HTTP query    
instance HT.QueryLike SubscriptionRequest where
  toQuery UserRequest=[("object",Just "user")]
  toQuery (TagRequest tag)=[("object",Just "tag"),"object_id" ?+ tag]
  toQuery (LocationRequest i)=[("object",Just "location"),"object_id" ?+ i]
  toQuery (GeographyRequest lat lng rad)=[("object",Just "geography"),"lat" ?+ lat
    ,"lng" ?+ lng
    ,"radius" ?+ rad]
 
-- | deletion parameters 
data DeletionParams
  -- | delete all subscriptions
  =DeleteAll
  -- | delete one subscription, given its ID
  | DeleteOne {
    doID :: Text
    }
  -- | delete all user subscriptions
  | DeleteUsers
  -- | delete all tag subscriptions
  | DeleteTags
  -- | delete all location subscriptions  
  | DeleteLocations
  -- | delete all geography subscriptions  
  | DeleteGeographies
   deriving (Read,Show,Eq,Ord,Typeable)

-- | to HTTP query    
instance HT.QueryLike DeletionParams where
  toQuery DeleteAll=[("object",Just "all")]
  toQuery (DeleteOne i)=["id" ?+ i]
  toQuery DeleteUsers=[("object",Just "user")]
  toQuery DeleteTags=[("object",Just "tag")]
  toQuery DeleteLocations=[("object",Just "location")]
  toQuery DeleteGeographies=[("object",Just "geography")]
  
-- | verify the signature with the content, using the secret as the key
verifySignature :: Monad m =>
                              BS.ByteString -- ^ the signature
                              -> BSL.ByteString -- ^ the content
                              -> InstagramT m Bool
verifySignature sig content=do
  csecret<-liftM clientSecretBS getCreds
  let key :: Crypto.MacKey ctx SHA1
      key = Crypto.MacKey csecret -- secret is the key
      hash = Crypto.hmac key content
      expected = Base16.encode (Crypto.encode hash)
  return $! sig `Crypto.constTimeEq` expected
