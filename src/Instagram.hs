-- | the public API for Instagram access
module Instagram
  (
  -- generic types and functions
  InstagramT
  ,runInstagramT
  ,runResourceInIs
  ,IGException(..)
  ,IGError(..)

  -- authentication
  ,RedirectUri
  ,getUserAccessTokenURL1
  ,getUserAccessTokenURL2
  ,Credentials(..)
  ,OAuthToken(..)
  ,AccessToken(..) -- we open this type so that the api client can just use the token data outside of your type (as a simple Text)
  ,User(..)
  ,UserCounts(..)
  ,Scope(..)

  -- data
  ,Envelope(..)
  ,getNextPage
  ,Pagination(..)
  ,Media(..)
  ,Position(..)
  ,UserPosition(..)
  ,Location(..)
  ,ImageData(..)
  ,Images(..)
  ,Comment(..)
  ,Collection(..)
  ,NoResult

  -- user
  ,UserID
  ,getUser
  ,SelfFeedParams(..)
  ,getSelfFeed
  ,RecentParams(..)
  ,getRecent
  ,SelfLikedParams(..)
  ,getSelfLiked
  ,UserSearchParams(..)
  ,searchUsers

  -- real time
  ,Aspect -- do not export constructor since only media is supported
  ,media
  ,CallbackUrl
  ,Subscription(..)
  ,createSubscription
  ,listSubscriptions
  ,deleteSubscriptions
  ,SubscriptionRequest(..)
  ,SubscriptionParams(..)
  ,DeletionParams(..)
  ,Update(..)
  ,verifySignature

  -- Tags
  ,Tag(..)
  ,TagName
  ,getTag
  ,RecentTagParams(..)
  ,getRecentTagged
  ,searchTags

  -- relationships
   ,OutgoingStatus(..)
  ,IncomingStatus(..)
  ,Relationship(..)
  ,getFollows
  ,getFollowedBy
  ,FollowParams(..)
  ,getFollowsParams
  ,getFollowedByParams
  ,getRequestedBy
  ,getRelationship
  ,setRelationShip
  ,RelationShipAction(..)

  -- media
  ,MediaID
  ,getMedia
  ,getPopularMedia
  ,MediaSearchParams(..)
  ,searchMedia

  -- comments
  ,CommentID
  ,getComments
  ,postComment
  ,deleteComment

  -- likes
  ,getLikes
  , getLikesMaxId
  ,like
  ,unlike

  -- locations
  ,LocationID
  ,getLocation
  ,LocationMediaParams(..)
  ,getLocationRecentMedia
  ,LocationSearchParams(..)
  ,searchLocations

  -- geographies
  ,GeographyID
  ,GeographyMediaParams(..)
  ,getGeographyRecentMedia

  ) where

import Instagram.Auth
import Instagram.Comments
import Instagram.Geographies
import Instagram.Likes
import Instagram.Locations
import Instagram.Media
import Instagram.Monad
import Instagram.RealTime
import Instagram.Relationships
import Instagram.Tags
import Instagram.Types
import Instagram.Users


-- debugging
--import Data.Aeson
--import qualified Data.ByteString.Lazy as BS
--
--parse :: IO()
--parse = do
--  t<-BS.readFile "env1.json"
--  let d=eitherDecode t
--  print (d::Either String (Envelope [Media]))
--  return()
