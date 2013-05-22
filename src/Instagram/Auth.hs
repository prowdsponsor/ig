{-# LANGUAGE FlexibleContexts #-}
-- | handle the authorization mecanism for Instagram
module Instagram.Auth (
  RedirectUri,
  getUserAccessTokenURL1,
  getUserAccessTokenURL2
) where

import Instagram.Monad
import Instagram.Types

import Data.Text hiding (map)
import Control.Monad (liftM)
import qualified Data.ByteString as BS (ByteString,intercalate)
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HT

import Data.Conduit

-- | the URI to redirect the user after she accepts/refuses to authorize the app
type RedirectUri = Text

-- | get the authorize url to redirect your user to
getUserAccessTokenURL1 :: Monad m => 
  RedirectUri -- ^ the URI to redirect the user after she accepts/refuses to authorize the app
  -> [Scope] -- ^ the requested scopes (can be empty for Basic)
  -> InstagramT m Text -- ^ the URL to redirect the user to
getUserAccessTokenURL1 url scopes=  do
  cid<-liftM clientIDBS getCreds
  bsurl<-getQueryURL "/oauth/authorize/" $ buildQuery cid ++ buildScopes scopes
  return $ TE.decodeUtf8 bsurl
  where
    -- | build the query with client id and redirect URI
    buildQuery :: BS.ByteString -> HT.SimpleQuery
    buildQuery cid=[("client_id",cid),("redirect_uri",TE.encodeUtf8 url),("response_type","code")]
    buildScopes ::  [Scope] ->  HT.SimpleQuery
    buildScopes []=[]
    buildScopes l =[("scope",BS.intercalate "+" $ map (TE.encodeUtf8 . toLower . pack . show) l)]
                
-- | second step of authorization: get the access token once the user has been redirected with a code
getUserAccessTokenURL2 :: (MonadBaseControl IO m, MonadResource m) =>
  RedirectUri -- ^ the redirect uri
  -> Text -- ^ the code sent back to your app
  -> InstagramT m OAuthToken -- ^ the auth token
getUserAccessTokenURL2 url code= do
  cid<-liftM clientIDBS getCreds
  csecret<-liftM clientSecretBS getCreds
  getPostRequest "/oauth/access_token" (buildQuery cid csecret) >>= getJSONResponse
  where
    -- | build query parameters, including the secret
    buildQuery :: BS.ByteString ->BS.ByteString -> HT.SimpleQuery
    buildQuery cid csecret=[("client_id",cid),("client_secret",csecret) 
        ,("redirect_uri",TE.encodeUtf8 url),("grant_type","authorization_code"),
        ("code",TE.encodeUtf8 code)]
     
     
     