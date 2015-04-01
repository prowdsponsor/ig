{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
-- | Types definitions
module Instagram.Types (
  Credentials(..)
  ,clientIDBS
  ,clientSecretBS
  ,OAuthToken(..)
  ,AccessToken(..)
  ,UserID
  ,User(..)
  ,Counts(..)
  ,Scope(..)
  ,IGException(..)
  ,Envelope(..)
  ,ErrEnvelope(..)
  ,IGError(..)
  ,Pagination(..)
  ,MediaID
  ,Media(..)
  ,Position(..)
  ,UserPosition(..)
  ,LocationID
  ,Location(..)
  ,ImageData(..)
  ,Images(..)
  ,CommentID
  ,Comment(..)
  ,Collection(..)
  ,Aspect(..)
  ,media
  ,CallbackUrl
  ,Subscription(..)
  ,Update(..)
  ,TagName
  ,Tag(..)
  ,OutgoingStatus(..)
  ,IncomingStatus(..)
  ,Relationship(..)
  ,NoResult
  ,GeographyID
)where

import Control.Applicative
import Data.Text
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.ByteString (ByteString)

import Data.Aeson


import qualified Data.Text.Encoding as TE
import Control.Exception.Base (Exception)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T (pack)
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM (lookup)

-- | the app credentials
data Credentials = Credentials {
  cClientID :: Text -- ^ client id
  ,cClientSecret :: Text -- ^ client secret
  }
  deriving (Show,Read,Eq,Ord,Typeable, Data)

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
    parseJSON _= fail "OAuthToken"

-- | the access token is simply a Text
newtype AccessToken=AccessToken Text
    deriving (Eq, Ord, Read, Show, Typeable)

-- | simple string
instance ToJSON AccessToken  where
        toJSON (AccessToken at)=String at

-- | simple string
instance FromJSON  AccessToken where
        parseJSON (String s)=pure $ AccessToken s
        parseJSON _= fail "AccessToken"

-- | User ID
type UserID = Text

-- | the User partial profile returned by the authentication
data User = User {
        uID :: UserID,
        uUsername :: Text,
        uFullName :: Text,
        uProfilePicture :: Maybe Text,
        uWebsite :: Maybe Text,
        uCounts :: Maybe Counts,
        uBio :: Maybe Text
      } deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON User  where
    toJSON u=object ["id" .= uID u, "username" .= uUsername u , "full_name" .= uFullName u, "profile_picture" .= uProfilePicture u, "website" .= uWebsite u, "counts" .= uCounts u, "bio" .= uBio u]

-- | from json as per Instagram format
instance FromJSON User where
    parseJSON (Object v) =
      User
        <$> v .: "id"
        <*> v .: "username"
        <*> v .: "full_name"
        <*> v .:? "profile_picture"
        <*> v .:? "website"
        <*> v .:? "counts"
        <*> v .:? "bio"
    parseJSON _= fail "User"

data Counts = Counts {
    ctMedia :: Int,
    ctFollows :: Int,
    ctFollowedBy :: Int
  } deriving (Show,Read,Eq,Ord,Typeable)

instance FromJSON Counts where
    parseJSON (Object v) =
      Counts
        <$> v .: "media"
        <*> v .: "follows"
        <*> v .: "followed_by"
    parseJSON _= fail "Counts"

instance ToJSON Counts where
  toJSON ct = object ["media" .= ctMedia ct, "follows" .= ctFollows ct, "followed_by" .= ctFollowedBy ct]


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
    parseJSON _= fail "IGError"

-- | an exception that a call to instagram may throw
data IGException = JSONException String -- ^ JSON parsingError
  | IGAppException IGError -- ^ application exception
  deriving (Show,Typeable)

-- | make our exception type a normal exception
instance Exception IGException

-- | envelope for Instagram OK response
data Envelope d=Envelope{
  eMeta :: IGError -- ^ this should only say 200, no error, but put here for completeness
  ,eData :: d -- ^ data, garanteed to be present (otherwise we get an ErrEnvelope)
  ,ePagination :: Maybe Pagination
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
    parseJSON _= fail "Envelope"

-- | error envelope for Instagram error response
data ErrEnvelope=ErrEnvelope{
  eeMeta :: IGError
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON ErrEnvelope  where
    toJSON e=object ["meta" .= eeMeta e]

-- | from json as per Instagram format
instance FromJSON ErrEnvelope where
    parseJSON (Object v) =ErrEnvelope <$>
                         v .: "meta"
    parseJSON _= fail "ErrEnvelope"

-- | pagination info for responses that can return a lot of data
data Pagination = Pagination {
   pNextUrl :: Maybe Text
   ,pNextMaxID :: Maybe Text
   ,pNextMinID :: Maybe Text
   ,pNextMaxTagID :: Maybe Text
   ,pMinTagID :: Maybe Text
   }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON Pagination  where
    toJSON p=object ["next_url" .= pNextUrl p, "next_max_id" .= pNextMaxID p, "next_min_id" .= pNextMinID p, "next_max_tag_id" .= pNextMaxTagID p,"min_tag_id" .= pMinTagID p]

-- | from json as per Instagram format
instance FromJSON Pagination where
    parseJSON (Object v) =Pagination <$>
                         v .:? "next_url" <*>
                         v .:? "next_max_id" <*>
                         v .:? "next_min_id" <*>
                         v .:? "next_max_tag_id" <*>
                         v .:? "min_tag_id"
    parseJSON _= fail "Pagination"

-- | Media ID
type MediaID=Text

-- | instagram media object
data Media = Media {
  mID :: MediaID
  ,mCaption :: Maybe Comment
  ,mLink :: Text
  ,mUser :: User
  ,mCreated :: POSIXTime
  ,mImages :: Images
  ,mType :: Text
  ,mUsersInPhoto :: [UserPosition]
  ,mFilter :: Maybe Text
  ,mTags :: [Text]
  ,mLocation :: Maybe Location
  ,mComments :: Collection Comment
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
                         v .:? "caption" <*>
                         v .: "link" <*>
                         v .: "user" <*>
                         pure (fromIntegral (read ct::Integer)) <*>
                         v .: "images" <*>
                         v .: "type" <*>
                         v .: "users_in_photo" <*>
                         v .:? "filter" <*>
                         v .: "tags" <*>
                         v .:? "location" <*>
                         v .:? "comments" .!= Collection 0 [] <*>
                         v .:? "likes" .!= Collection 0 [] <*>
                         v .:? "user_has_liked" .!= False <*>
                         v .:? "attribution"
    parseJSON _= fail "Media"

-- | position in picture
data Position = Position {
  pX ::Double
  ,pY :: Double
} deriving (Show,Eq,Typeable)


-- | to json as per Instagram format
instance ToJSON Position where
  toJSON p=object ["x" .= pX p,"y" .= pY p]

-- | from json as per Instagram format
instance FromJSON Position where
  parseJSON (Object v) = Position <$>
    v .: "x" <*>
    v .: "y"
  parseJSON _=fail "Position"

-- | position of a user
data UserPosition = UserPosition {
  upPosition :: Position
  ,upUser :: User
  } deriving (Show,Eq,Typeable)


-- | to json as per Instagram format
instance ToJSON UserPosition where
  toJSON p=object ["position" .= upPosition p,"user" .= upUser p]

-- | from json as per Instagram format
instance FromJSON UserPosition where
  parseJSON (Object v) = UserPosition <$>
    v .: "position" <*>
    v .: "user"
  parseJSON _=fail "UserPosition"

-- | location ID
type LocationID = Text

-- | geographical location info
data Location = Location {
  lID :: Maybe LocationID
  ,lLatitude :: Maybe Double
  ,lLongitude :: Maybe Double
  ,lStreetAddress :: Maybe Text
  ,lName :: Maybe Text
  }
  deriving (Show,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON Location where
  toJSON l=object ["id" .= lID l,"latitude" .= lLatitude l,"longitude" .= lLongitude l, "street_address" .= lStreetAddress l,"name" .= lName l]

-- | from json as per Instagram format
instance FromJSON Location where
  parseJSON (Object v) =
    Location <$>
      parseID v <*>
      v .:? "latitude" <*>
      v .:? "longitude" <*>
      v .:? "street_address" <*>
      v .:? "name"
    where
      -- | the Instagram API hasn't made its mind up, sometimes location id is an int, sometimes a string
      parseID :: Object -> Parser (Maybe LocationID)
      parseID obj=case HM.lookup "id" obj of
        Just (String s)->pure $ Just s
        Just (Number n)->pure $ Just $ T.pack $ show n
        Nothing->pure Nothing
        _->fail "LocationID"
  parseJSON _= fail "Location"

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
  parseJSON _= fail "ImageData"

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
  parseJSON _= fail "Images"

-- | comment id
type CommentID = Text

-- | Commenton on a medium
data Comment = Comment {
  cID :: CommentID
  ,cCreated :: POSIXTime
  ,cText :: Text
  ,cFrom :: User
  }
  deriving (Show,Eq,Ord,Typeable)

-- | to json asCommentstagram format
instance ToJSON Comment  where
    toJSON c=object ["id" .= cID c,"created_time" .= toJSON (show ((round $ cCreated c) :: Integer))
      ,"text" .= cText c,"from" .= cFrom c]

-- | from json asCommentstagram format
instance FromJSON Comment where
    parseJSON (Object v) =do
      ct::String<-v .: "created_time"
      Comment <$>
                         v .: "id" <*>
                         pure (fromIntegral (read ct::Integer)) <*>
                         v .: "text" <*>
                         v .: "from"
    parseJSON _= fail "Caption"

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
    parseJSON _= fail "Collection"


-- | the URL to receive notifications to
type CallbackUrl = Text

-- | notification aspect
data Aspect = Aspect Text
  deriving (Show, Read, Eq, Ord, Typeable)

-- | to json as per Instagram format
instance ToJSON Aspect  where
    toJSON (Aspect t)=String t

-- | from json as per Instagram format
instance FromJSON Aspect where
    parseJSON (String t) = pure $ Aspect t
    parseJSON _= fail "Aspect"

-- | the media Aspect, the only one supported for now
media :: Aspect
media = Aspect "media"

-- | a subscription to a real time notification
data Subscription= Subscription {
  sID :: Text
  ,sType :: Text
  ,sObject :: Text
  ,sObjectID :: Maybe Text
  ,sAspect :: Aspect
  ,sCallbackUrl :: CallbackUrl
  ,sLatitude :: Maybe Double
  ,sLongitude :: Maybe Double
  ,sRadius :: Maybe Integer
  }
  deriving (Show,Eq,Typeable)

-- | to json as per Instagram format
instance ToJSON Subscription  where
    toJSON s=object ["id" .= sID s,"type" .= sType s,"object" .= sObject s,"object_id" .= sObjectID s,"aspect" .= sAspect s
      ,"callback_url".=sCallbackUrl s,"lat".= sLatitude s,"lng".=sLongitude s,"radius".=sRadius s]

-- | from json as per Instagram format
instance FromJSON Subscription where
    parseJSON (Object v) = Subscription <$>
                         v .: "id" <*>
                         v .: "type" <*>
                         v .: "object" <*>
                         v .:? "object_id" <*>
                         v .: "aspect" <*>
                         v .: "callback_url" <*>
                         v .:? "lat" <*>
                         v .:? "lng" <*>
                         v .:? "radius"
    parseJSON _= fail "Subscription"

-- | an update from a subscription
data Update = Update {
  uSubscriptionID :: Integer
  ,uObject :: Text
  ,uObjectID :: Text
  ,uChangedAspect :: Aspect
  ,uTime :: POSIXTime
  }
  deriving (Show,Eq,Typeable)

-- | to json as per Instagram format
instance ToJSON Update  where
    toJSON u=object ["subscription_id" .= uSubscriptionID u      ,"object" .= uObject u,"object_id" .= uObjectID u
      ,"changed_aspect" .= uChangedAspect u,"time" .= toJSON ((round $ uTime u) :: Integer)]

-- | from json as per Instagram format
instance FromJSON Update where
    parseJSON (Object v) =do
      ct::Integer<-v .: "time"
      Update <$>
                         v .: "subscription_id" <*>
                         v .: "object" <*>
                         v .: "object_id" <*>
                         v .: "changed_aspect" <*>
                         pure (fromIntegral ct)
    parseJSON _= fail "Update"

-- | Tag Name
type TagName = Text

-- | a Tag
data Tag = Tag {
  tName :: TagName,
  tMediaCount :: Integer
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON Tag  where
    toJSON t=object ["name" .= tName t,"media_count" .= tMediaCount t]

-- | from json as per Instagram format
instance FromJSON Tag where
    parseJSON (Object v) = Tag <$>
                         v .: "name" <*>
                         v .:? "media_count" .!= 0
    parseJSON _= fail "Tag"

-- | outgoing relationship status
data OutgoingStatus = Follows | Requested | OutNone
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per Instagram format
instance ToJSON OutgoingStatus  where
    toJSON Follows = String "follows"
    toJSON Requested = String "requested"
    toJSON OutNone = String "none"

-- | from json as per Instagram format
instance FromJSON OutgoingStatus where
  parseJSON (String "follows")=pure Follows
  parseJSON (String "requested")=pure Requested
  parseJSON (String "none")=pure OutNone
  parseJSON _= fail "OutgoingStatus"

-- | incoming relationship status
data IncomingStatus = FollowedBy | RequestedBy | BlockedByYou | InNone
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

-- | to json as per Instagram format
instance ToJSON IncomingStatus  where
    toJSON FollowedBy = String "followed_by"
    toJSON RequestedBy = String "requested_by"
    toJSON BlockedByYou = String "blocked_by_you"
    toJSON InNone = String "none"

-- | from json as per Instagram format
instance FromJSON IncomingStatus where
  parseJSON (String "followed_by")=pure FollowedBy
  parseJSON (String "requested_by")=pure RequestedBy
  parseJSON (String "blocked_by_you")=pure BlockedByYou
  parseJSON (String "none")=pure InNone
  parseJSON _= fail "IncomingStatus"

-- | a relationship between two users
data Relationship = Relationship {
  rOutgoing :: OutgoingStatus
  ,rIncoming :: IncomingStatus
  ,rTargetUserPrivate :: Bool -- ^ not present in doc
  }
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON Relationship  where
    toJSON r=object ["outgoing_status" .= rOutgoing r,"incoming_status" .= rIncoming r,"target_user_is_private" .= rTargetUserPrivate r]

-- | from json as per Instagram format
instance FromJSON Relationship where
    parseJSON (Object v) = Relationship <$>
                         v .:? "outgoing_status" .!= OutNone <*>
                         v .:? "incoming_status" .!= InNone <*>
                         v .:? "target_user_is_private" .!= False
    parseJSON _= fail "Relationship"

-- | Instagram returns data:null for nothing, but Aeson considers that () maps to an empty array...
-- so we model the fact that we expect null via NoResult
data NoResult = NoResult
  deriving (Show,Read,Eq,Ord,Typeable)

-- | to json as per Instagram format
instance ToJSON NoResult  where
  toJSON _=Null

-- | from json as per Instagram format
instance FromJSON NoResult where
    parseJSON Null = pure NoResult
    parseJSON _= fail "NoResult"

-- | geography ID
type GeographyID = Text
