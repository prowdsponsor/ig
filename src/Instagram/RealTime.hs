{-# LANGUAGE FlexibleContexts #-}
-- | Real time subscription management
module Instagram.RealTime (
  createSubscription
  ,listSubscriptions
  ,deleteSubscriptions
  ,SubscriptionRequest(..)
  ,SubscriptionParams(..)
  ,DeletionParams(..)
)

where

import Instagram.Monad
import Instagram.Types

import Data.ByteString.Char8(pack)
import Data.Text (Text)
import Data.Typeable
import qualified Network.HTTP.Types as HT 
import qualified Data.Text.Encoding as TE
import Data.Maybe (isJust)
import Data.Conduit
import Data.Aeson (Value(..))

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
    [("aspect",Just $ TE.encodeUtf8 asp)
    ,("callback_url",Just $ TE.encodeUtf8 cb)
    ,("verify_token",fmap TE.encodeUtf8 tok)]

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
  toQuery (TagRequest tag)=[("object",Just "tag"),("object_id",Just $ TE.encodeUtf8 tag)]
  toQuery (LocationRequest i)=[("object",Just "location"),("object_id",Just $ TE.encodeUtf8 i)]
  toQuery (GeographyRequest lat lng rad)=[("object",Just "geography"),("lat",Just $ pack $ show lat)
    ,("lng",Just $ pack $ show lng)
    ,("radius",Just $ pack $ show rad)]
 
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
  toQuery (DeleteOne i)=[("id",Just $ TE.encodeUtf8  i)]
  toQuery DeleteUsers=[("object",Just "user")]
  toQuery DeleteTags=[("object",Just "tag")]
  toQuery DeleteLocations=[("object",Just "location")]
  toQuery DeleteGeographies=[("object",Just "geography")]
  
