{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, FlexibleInstances,
  MultiParamTypeClasses, UndecidableInstances, TypeFamilies,
  FlexibleContexts, RankNTypes, CPP, StandaloneDeriving #-}
-- | the instagram monad stack and helper functions
module Instagram.Monad (
  InstagramT
  ,runInstagramT
  ,getCreds
  ,getHost
  ,getPostRequest
  ,getGetRequest
  ,getDeleteRequest
  ,getQueryURL
  ,getJSONResponse
  ,getJSONEnvelope
  ,getGetEnvelope
  ,getGetEnvelopeM
  ,getPostEnvelope
  ,getPostEnvelopeM
  ,getDeleteEnvelope
  ,getDeleteEnvelopeM
  ,getNextPage
  ,getManager
  ,runResourceInIs
  ,mapInstagramT
  ,addToken
  ,addTokenM
  ,addClientInfos
  ,ToHtQuery(..)

  , MonadBaseControl
  , R.MonadResource
  ) where

import Instagram.Types

import Control.Applicative
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control ( MonadTransControl(..), MonadBaseControl(..)
                                   , ComposeSt, defaultLiftBaseWith
#if MIN_VERSION_monad_control(1,0,0)
                                   , defaultLiftWith, defaultRestoreT
#endif
                                   , defaultRestoreM )
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Data.Default (def)
import Data.Typeable (Typeable)
import qualified Control.Monad.Trans.Resource as R
import qualified Control.Exception.Lifted as L

import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Aeson (json,fromJSON,Result(..),FromJSON)
import Data.Conduit.Attoparsec (sinkParser, ParseError)
import Control.Exception.Base (throw)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T (Text,concat, unpack)
import Data.Time.Clock.POSIX (POSIXTime)

#if DEBUG
import Control.Monad.IO.Class (liftIO)
import Data.Conduit.Binary (sinkHandle)
import System.IO (stdout)
import Data.Conduit.Util (zipSinks)
#endif

-- | the instagram monad transformer
-- this encapsulates the data necessary to pass the app credentials, etc
newtype InstagramT m a = Is { unIs :: ReaderT IsData m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadFix
             , MonadPlus, MonadIO, MonadTrans, R.MonadThrow )

deriving instance R.MonadResource m => R.MonadResource (InstagramT m)

instance MonadBase b m => MonadBase b (InstagramT m) where
    liftBase = lift . liftBase

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl InstagramT where
    type StT InstagramT a = StT (ReaderT IsData) a
    liftWith = defaultLiftWith Is unIs
    restoreT = defaultRestoreT Is

instance MonadBaseControl b m => MonadBaseControl b (InstagramT m) where
    type StM (InstagramT m) a = ComposeSt InstagramT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
#else
instance MonadTransControl InstagramT where
    newtype StT InstagramT a = FbStT { unFbStT :: StT (ReaderT IsData) a }
    liftWith f = Is $ liftWith (\run -> f (liftM FbStT . run . unIs))
    restoreT = Is . restoreT . liftM unFbStT

instance MonadBaseControl b m => MonadBaseControl b (InstagramT m) where
    newtype StM (InstagramT m) a = StMT {unStMT :: ComposeSt InstagramT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM = defaultRestoreM unStMT
#endif

-- | Run a computation in the 'InstagramT' monad transformer with
-- your credentials.
runInstagramT :: Credentials -- ^ Your app's credentials.
             -> H.Manager -- ^ Connection manager (see 'H.withManager').
             -> InstagramT m a -- ^ the action to run
             -> m a -- ^ the result
runInstagramT creds manager (Is act) =
    runReaderT act (IsData creds manager "api.instagram.com") -- potentially we could open to other hosts if there were test endpoints, etc...

-- | Get the user's credentials.
getCreds :: Monad m => InstagramT m Credentials
getCreds = isCreds `liftM` Is ask

-- | Get the instagram host
getHost :: Monad m => InstagramT m ByteString
getHost = isHost `liftM` Is ask

-- | build a post request to Instagram
getPostRequest :: (Monad m,HT.QueryLike q) => ByteString -- ^ the url path
  -> q -- ^ the query parameters
  -> InstagramT m H.Request -- ^ the properly configured request
getPostRequest path query=do
  host<-getHost
  return $ def {
                     H.secure=True
                     , H.host = host
                     , H.port = 443
                     , H.path = path
                     , H.method=HT.methodPost
                     , H.requestBody=H.RequestBodyBS $ HT.renderQuery False $ HT.toQuery query
                }

-- | build a get request to Instagram
getGetRequest :: (Monad m,MonadIO m,HT.QueryLike q) => ByteString -- ^ the url path
  -> q -- ^ the query parameters
  -> InstagramT m H.Request -- ^ the properly configured request
getGetRequest path query=do
  host<-getHost
  let qs=HT.renderQuery True $ HT.toQuery query
#if DEBUG
  liftIO $ BSC.putStrLn $ BS.append path qs
#endif
  return $ def {
                     H.secure=True
                     , H.host = host
                     , H.port = 443
                     , H.path = path
                     , H.method=HT.methodGet
                     , H.queryString=qs
                }

-- | build a delete request  to Instagram
getDeleteRequest :: (Monad m,MonadIO m,HT.QueryLike q) => ByteString -- ^ the url path
  -> q -- ^ the query parameters
  -> InstagramT m H.Request -- ^ the properly configured request
getDeleteRequest path query=do
  get<-getGetRequest path query
  return $ get {H.method=HT.methodDelete}

-- | build a URL for a get operation with a single query
getQueryURL :: (Monad m,HT.QueryLike q) => ByteString -- ^ the url path
  -> q -- ^ the query parameters
  -> InstagramT m ByteString  -- ^ the URL
getQueryURL path query=do
  host<-getHost
  return $ BS.concat ["https://",host,path,HT.renderQuery True  $ HT.toQuery query]

-- | perform a HTTP request and deal with the JSON result
igReq :: forall b (m :: * -> *) wrappedErr .
                    (MonadBaseControl IO m, R.MonadResource m,FromJSON b,FromJSON wrappedErr) =>
                    H.Request
                    -> (wrappedErr -> IGError) -- ^ extract the error from the JSON
                    -> InstagramT m b
igReq req extractError=do
   -- we check the status ourselves
  let req' = req { H.checkStatus = \_ _ _ -> Nothing }
  mgr<-getManager
  res<-H.http req' mgr
  let status = H.responseStatus res
      headers = H.responseHeaders res
      cookies = H.responseCookieJar res
      ok=isOkay status
      err=H.StatusCodeException status headers cookies
  L.catch (do
#if DEBUG
    (value,_)<-H.responseBody res C.$$+- zipSinks (sinkParser json) (sinkHandle stdout)
    liftIO $ BSC.putStrLn ""
#else
    value<-H.responseBody res C.$$+- sinkParser json
#endif
    if ok
      then
          -- parse response as the expected value
          case fromJSON value of
            Success ot->return ot
            Error jerr->throw $ JSONException jerr -- got an ok response we couldn't parse
      else
          -- parse response as an error
          case fromJSON value of
            Success ise-> throw $ IGAppException $ extractError ise
            _ -> throw err -- we can't even parse the error, throw the HTTP error
    ) (\(_::ParseError)->throw err) -- the error body wasn't even json

-- | get a JSON response from a request to Instagram
-- instagram returns either a result, or an error
getJSONResponse :: forall (m :: * -> *) v.
                                 (MonadBaseControl IO m, R.MonadResource m,FromJSON v) =>
                                 H.Request
                                 -> InstagramT
                                      m v
getJSONResponse req=igReq req id

-- | get an envelope from a request to Instagram
-- the error is wrapped inside the envelope
getJSONEnvelope :: forall (m :: * -> *) v.
                                 (MonadBaseControl IO m, R.MonadResource m,FromJSON v) =>
                                 H.Request
                                 -> InstagramT
                                      m (Envelope v)
getJSONEnvelope req=igReq req eeMeta

-- | get an envelope from Instagram
getGetEnvelope :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  [T.Text] -- ^ the URL components, will be concatenated
  -> OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getGetEnvelope urlComponents token=getGetEnvelopeM urlComponents (Just token)

-- | get an envelope from Instagram, with optional authentication
getGetEnvelopeM :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  [T.Text]  -- ^ the URL components, will be concatenated
  -> Maybe OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getGetEnvelopeM=getEnvelopeM getGetRequest

-- | send a delete and get an envelope from Instagram
getDeleteEnvelope :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  [T.Text] -- ^ the URL components, will be concatenated
  -> OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getDeleteEnvelope urlComponents token=getDeleteEnvelopeM urlComponents (Just token)

-- | send a delete and get an envelope from Instagram, with optional authentication
getDeleteEnvelopeM :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  [T.Text]  -- ^ the URL components, will be concatenated
  -> Maybe OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getDeleteEnvelopeM=getEnvelopeM getDeleteRequest

-- | post a request and get back an envelope from Instagram
getPostEnvelope :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  [T.Text] -- ^ the URL components, will be concatenated
  -> OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getPostEnvelope urlComponents token=getPostEnvelopeM urlComponents (Just token)

-- | post a request and get back an envelope from Instagram, with optional authentication
getPostEnvelopeM :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  [T.Text]  -- ^ the URL components, will be concatenated
  -> Maybe OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getPostEnvelopeM=getEnvelopeM getPostRequest

-- | utility function to get an envelop, independently of how the request is built
getEnvelopeM :: (MonadBaseControl IO m, R.MonadResource m,HT.QueryLike ql,FromJSON v) =>
  (ByteString -> HT.Query -> InstagramT m H.Request) -- ^ the request building method
  -> [T.Text]  -- ^ the URL components, will be concatenated
  -> Maybe OAuthToken -- ^ the access token
  -> ql -- ^ the query parameters
  -> InstagramT m (Envelope v) -- ^ the resulting envelope
getEnvelopeM f urlComponents token ql=do
   let url=TE.encodeUtf8 $ T.concat urlComponents
   addTokenM token ql >>= f url >>= getJSONEnvelope

-- | Use the pagination links in an 'Envelope' to fetch the next page of
-- results.
--
-- If the Envelope has no pagination, or we have reached the final page
-- (indicated by the pNextUrl field being missing), returns Nothing.
getNextPage :: (MonadBaseControl IO m, R.MonadResource m, FromJSON v)
            => Envelope v
            -> InstagramT m (Maybe (Envelope v))
getNextPage e = case maybeRequest of
    Nothing -> return Nothing
    Just req -> Just <$> getJSONEnvelope req
  where
    maybeRequest = do  -- Maybe monad
        nextUrl <- pNextUrl =<< ePagination e
        H.parseUrl $ T.unpack nextUrl

-- | Get the 'H.Manager'.
getManager :: Monad m => InstagramT m H.Manager
getManager = isManager `liftM` Is ask

-- | Run a 'ResourceT' inside a 'InstagramT'.
runResourceInIs :: (R.MonadResource m, MonadBaseControl IO m) =>
                   InstagramT (R.ResourceT m) a
                -> InstagramT m a
runResourceInIs (Is inner) = Is $ ask >>= lift . R.runResourceT . runReaderT inner

-- | Transform the computation inside a 'InstagramT'.
mapInstagramT :: (m a -> n b) -> InstagramT m a -> InstagramT n b
mapInstagramT f = Is . mapReaderT f . unIs

-- | the data kept through the computations
data IsData = IsData {
        isCreds::Credentials -- ^ app credentials
        ,isManager::H.Manager -- ^ HTTP connection manager
        ,isHost:: ByteString -- ^ host name
        }
        deriving (Typeable)

-- | @True@ if the the 'Status' is ok (i.e. @2XX@).
isOkay :: HT.Status -> Bool
isOkay status =
  let sc = HT.statusCode status
  in 200 <= sc && sc < 300

-- | add the access token to the query
addToken :: HT.QueryLike ql=> OAuthToken -> ql -> HT.Query
addToken (OAuthToken{oaAccessToken=(AccessToken t)}) ql=("access_token", Just $ TE.encodeUtf8 t) : HT.toQuery ql

-- | add an optional access token to the query
-- if we don't have a token, we'll pass the client_id
addTokenM :: (R.MonadResource m, MonadBaseControl IO m,HT.QueryLike ql)=> Maybe OAuthToken -> ql -> InstagramT m HT.Query
addTokenM (Just oat) ql=return $ addToken oat ql
addTokenM _ ql= do
  cid<-liftM clientIDBS getCreds
  return $ ("client_id",Just cid) : HT.toQuery ql

-- | add application client info to the query
addClientInfos :: (R.MonadResource m, MonadBaseControl IO m,HT.QueryLike ql) =>
    ql ->
    InstagramT m HT.Query
addClientInfos ql= do
  cid<-liftM clientIDBS getCreds
  csecret<-liftM clientSecretBS getCreds
  return $ ("client_id",Just cid):("client_secret", Just csecret) : HT.toQuery ql

-- | simple class used to hide the serialization of parameters ansd simplify the calling code
class ToHtQuery a where
  (?+) :: ByteString -> a -> (ByteString,Maybe ByteString)

instance ToHtQuery Double where
  n ?+ d=n ?+ show d

instance ToHtQuery (Maybe Double) where
  n ?+ d=n ?+ fmap show d

instance ToHtQuery Integer where
  n ?+ d=n ?+ show d

instance ToHtQuery (Maybe Integer) where
  n ?+ d=n ?+ fmap show d

instance ToHtQuery (Maybe POSIXTime) where
  n ?+ d=n ?+ fmap (show . (round :: POSIXTime -> Integer)) d

instance ToHtQuery (Maybe T.Text) where
  n ?+ d=(n,fmap TE.encodeUtf8 d)

instance ToHtQuery T.Text where
  n ?+ d=(n,Just $ TE.encodeUtf8 d)

instance ToHtQuery (Maybe String) where
  n ?+ d=(n,fmap BSC.pack d)

instance ToHtQuery String where
  n ?+ d=(n,Just $ BSC.pack d)
