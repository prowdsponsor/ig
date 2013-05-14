{-# LANGUAGE FlexibleContexts #-}

module Instagram.Auth (
  getUserAccessTokenURL1,
  getUserAccessTokenURL2
) where

import Instagram.Monad
import Instagram.Types

import Data.Text hiding (map)
import qualified Network.HTTP.Conduit as H
import Control.Monad (liftM)
import qualified Data.ByteString as BS (ByteString,intercalate)
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT
import Data.Aeson (json,fromJSON,Result(..))

import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit

type RedirectUrl = Text

getUserAccessTokenURL1 :: Monad m => RedirectUrl -> [Scope]
                        -> InstagramT m (H.Request a)
getUserAccessTokenURL1 url scopes=  do
  cid<-liftM clientIDBS getCreds
  getSimpleQueryRequest "/oauth/authorize/" $ buildQuery cid ++ buildScopes scopes
  where
    buildQuery :: BS.ByteString -> HT.SimpleQuery
    buildQuery cid=[("client_id",cid),("redirect_uri",TE.encodeUtf8 url),("response_type","code")]
    buildScopes ::  [Scope] ->  HT.SimpleQuery
    buildScopes []=[]
    buildScopes l =[("scope",BS.intercalate "+" $ map (TE.encodeUtf8 . toLower . pack . show) l)]
                
getUserAccessTokenURL2 :: (MonadBaseControl IO m, MonadResource m) =>
  RedirectUrl -> BS.ByteString -> InstagramT m OAuthToken
getUserAccessTokenURL2 url code= do
  cid<-liftM clientIDBS getCreds
  csecret<-liftM clientSecretBS getCreds
  req<-getSimpleQueryRequest "/oauth/access_token" $ buildQuery cid csecret
  mgr<-getManager
  res <- H.http req mgr
  value<-H.responseBody res $$+- sinkParser json
  case fromJSON value of
    Success ot->return ot
    Error err->fail err
  where
     buildQuery :: BS.ByteString ->BS.ByteString -> HT.SimpleQuery
     buildQuery cid csecret=[("client_id",cid),("client_secret",csecret)
        ,("redirect_uri",TE.encodeUtf8 url),("grant_type","authorization_code"),
        ("code",code)]
     