{-# LANGUAGE FlexibleContexts #-}
module Instagram.Comments (
  getComments
  ,postComment
  ,deleteComment
)where

import Instagram.Monad
import Instagram.Types

import Data.Conduit
import qualified Network.HTTP.Types as HT
import Data.Text (Text)

-- | get a full list of comments on a media
getComments ::     (MonadBaseControl IO m, MonadResource m) => MediaID 
  -> Maybe OAuthToken
  -> InstagramT m (Envelope [Comment])
getComments mid token  =getGetEnvelopeM ["/v1/media/",mid,"/comments"] token ([]::HT.Query)

-- | create a comment on a media
postComment ::     (MonadBaseControl IO m, MonadResource m) => MediaID 
  -> OAuthToken
  -> Text
  -> InstagramT m (Envelope ())
postComment mid token txt =getPostEnvelope ["/v1/media/",mid,"/comments"] token ["text" ?+ txt]

-- | create a comment on a media
deleteComment ::     (MonadBaseControl IO m, MonadResource m) => MediaID 
  -> CommentID
  -> OAuthToken
  -> InstagramT m (Envelope ())
deleteComment mid cid token =getDeleteEnvelope ["/v1/media/",mid,"/comments/",cid] token ([]::HT.Query)