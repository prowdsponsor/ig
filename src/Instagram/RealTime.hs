{-# LANGUAGE FlexibleContexts #-}
-- | Real time subscription management
module Instagram.RealTime (
  createSubscription
  ,listSubscriptions
  ,deleteSubscription
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

createSubscription :: (MonadBaseControl IO m, MonadResource m) => 
  SubscriptionParams
  -> InstagramT m (Envelope [Subscription]) -- ^ the ID of the subscription
createSubscription params=do
  let url="/v1/subscriptions/"
  getPostRequest url params>>= getJSONEnvelope  

listSubscriptions :: (MonadBaseControl IO m, MonadResource m) => 
  InstagramT m (Envelope [Subscription]) -- ^ the ID of the subscription
listSubscriptions =do
  let url="/v1/subscriptions/"
  getGetRequest url ([]::HT.Query)>>= getJSONEnvelope  

deleteSubscription :: (MonadBaseControl IO m, MonadResource m) => 
  DeletionParams -- ^ the parameters for the deletion
  -> InstagramT m (Envelope ()) -- ^ the ID of the subscription
deleteSubscription params=do
  let url="/v1/subscriptions/"
  getDeleteRequest url params>>= getJSONEnvelope  
 
data SubscriptionParams= SubscriptionParams {
  spRequest :: SubscriptionRequest
  ,spCallback :: CallbackUrl
  ,spAspect :: Aspect
  ,spVerifyToken :: Maybe Text
  }
  deriving (Read,Show,Eq,Ord,Typeable)
  
instance HT.QueryLike SubscriptionParams where
  toQuery (SubscriptionParams req cb (Aspect asp) tok)=filter (isJust .snd) $ HT.toQuery req ++ 
    [("aspect",Just $ TE.encodeUtf8 asp)
    ,("callback_url",Just $ TE.encodeUtf8 cb)
    ,("verify_token",fmap TE.encodeUtf8 tok)]
  
data SubscriptionRequest=UserRequest
  | TagRequest {
    trTag ::Text
    }
  | LocationRequest {
    lrID :: Text
     }
  | GeographyRequest {
    grLatitude :: Double
    ,grLongitude :: Double
    ,grRadius :: Integer
    }  
    deriving (Read,Show,Eq,Ord,Typeable)
  
instance HT.QueryLike SubscriptionRequest where
  toQuery UserRequest=[("object",Just "user")]
  toQuery (TagRequest tag)=[("object",Just "tag"),("object_id",Just $ TE.encodeUtf8 tag)]
  toQuery (LocationRequest i)=[("object",Just "location"),("object_id",Just $ TE.encodeUtf8 i)]
  toQuery (GeographyRequest lat lng rad)=[("object",Just "geography"),("lat",Just $ pack $ show lat)
    ,("lng",Just $ pack $ show lng)
    ,("radius",Just $ pack $ show rad)]
  
data DeletionParams=DeleteAll
  | DeleteOne {
    doID :: Text
    }
  | DeleteUsers
  | DeleteTags
  | DeleteLocations
  | DeleteGeography
   deriving (Read,Show,Eq,Ord,Typeable)
  
instance HT.QueryLike DeletionParams where
  toQuery DeleteAll=[("object",Just "all")]
  toQuery (DeleteOne i)=[("id",Just $ TE.encodeUtf8  i)]
  toQuery DeleteUsers=[("object",Just "user")]
  toQuery DeleteTags=[("object",Just "tag")]
  toQuery DeleteLocations=[("object",Just "location")]
  toQuery DeleteGeography=[("object",Just "geography")]
  
