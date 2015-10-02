{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Likes handling
-- <http://instagram.com/developer/endpoints/likes/#>
module Instagram.Likes (
  getLikes
  ,getLikesMaxId
  ,like
  ,unlike
)where

import Instagram.Monad
import Instagram.Types
import Data.String
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT


-- | Get a list of users who have liked this media.
getLikes ::     (MonadBaseControl IO m, MonadResource m) => MediaID
  -> Maybe OAuthToken
  -> InstagramT m (Envelope [User])
getLikes mid token  =getGetEnvelopeM ["/v1/media/",mid,"/likes"] token ([]::HT.Query)

getLikesMaxId :: (MonadBaseControl IO m, MonadResource m) => MediaID
  -> Maybe UserID
  -> Maybe OAuthToken
  -> InstagramT m (Envelope [User])
getLikesMaxId mid Nothing token  =getGetEnvelopeM ["/v1/media/",mid,"/likes"] token ([]::HT.Query)
getLikesMaxId mid (Just maxUserId) token =
  let maxid = (T.unpack maxUserId) :: String
  in getGetEnvelopeM ["/v1/media/",mid,"/likes"] token ([("MAX_ID", Just $ fromString $ maxid)]::HT.Query)

-- | Set a like on this media by the currently authenticated user.
like ::     (MonadBaseControl IO m, MonadResource m) => MediaID
  -> OAuthToken
  -> InstagramT m (Envelope NoResult)
like mid token =getPostEnvelope ["/v1/media/",mid,"/likes"] token ([]::HT.Query)

-- | Remove a like on this media by the currently authenticated user.
unlike ::     (MonadBaseControl IO m, MonadResource m) => MediaID
  -> OAuthToken
  -> InstagramT m (Envelope NoResult)
unlike mid token =getDeleteEnvelope ["/v1/media/",mid,"/likes"] token ([]::HT.Query)
