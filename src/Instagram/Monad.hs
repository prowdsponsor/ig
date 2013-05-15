{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, FlexibleContexts #-}
module Instagram.Monad (
  InstagramT
  ,runInstagramT
  ,getCreds
  ,getHost
  ,getSimpleQueryPostRequest
  ,getSimpleQueryURL
  ,getManager
  ,runResourceInIs
  ,mapInstagramT
  ) where

import Instagram.Types

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control ( MonadTransControl(..), MonadBaseControl(..)
                                   , ComposeSt, defaultLiftBaseWith
                                   , defaultRestoreM )
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Data.Typeable (Typeable)
import qualified Control.Monad.Trans.Resource as R

import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

newtype InstagramT m a = Is { unIs :: ReaderT IsData m a }
    deriving ( Functor, Applicative, Alternative, Monad
             , MonadFix, MonadPlus, MonadIO, MonadTrans
             , R.MonadThrow, R.MonadActive, R.MonadResource )
             
instance MonadBase b m => MonadBase b (InstagramT m) where
    liftBase = lift . liftBase

instance MonadTransControl InstagramT where
    newtype StT InstagramT a = FbStT { unFbStT :: StT (ReaderT IsData) a }
    liftWith f = Is $ liftWith (\run -> f (liftM FbStT . run . unIs))
    restoreT = Is . restoreT . liftM unFbStT

instance MonadBaseControl b m => MonadBaseControl b (InstagramT m) where
    newtype StM (InstagramT m) a = StMT {unStMT :: ComposeSt InstagramT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM = defaultRestoreM unStMT
    
-- | Run a computation in the 'InstagramT' monad transformer with
-- your credentials.
runInstagramT :: Credentials -- ^ Your app's credentials.
             -> H.Manager -- ^ Connection manager (see 'H.withManager').
             -> InstagramT m a
             -> m a
runInstagramT creds manager (Is act) =
    runReaderT act (IsData creds manager "api.instagram.com")
    
-- | Get the user's credentials.
getCreds :: Monad m => InstagramT m Credentials
getCreds = isCreds `liftM` Is ask

-- | Get the base url
getHost :: Monad m => InstagramT m ByteString
getHost = isHost `liftM` Is ask

getSimpleQueryPostRequest :: Monad m => ByteString -> HT.SimpleQuery -> InstagramT m (H.Request a)
getSimpleQueryPostRequest path query=do
  host<-getHost
  return $ H.def {
                     H.secure=True
                     , H.host = host
                     , H.port = 443
                     , H.path = path
                     , H.method=HT.methodPost
                     , H.requestBody=H.RequestBodyBS $ HT.renderSimpleQuery False query
                }

getSimpleQueryURL :: Monad m => ByteString -> HT.SimpleQuery -> InstagramT m ByteString 
getSimpleQueryURL path query=do
  host<-getHost
  return $ BS.concat ["https://",host,path,HT.renderSimpleQuery True query]

-- | Get the 'H.Manager'.
getManager :: Monad m => InstagramT m H.Manager
getManager = isManager `liftM` Is ask

-- | Run a 'ResourceT' inside a 'InstagramT'.
runResourceInIs :: (C.MonadResource m, MonadBaseControl IO m) =>
                   InstagramT (C.ResourceT m) a
                -> InstagramT m a
runResourceInIs (Is inner) = Is $ ask >>= lift . C.runResourceT . runReaderT inner    
    
-- | Transform the computation inside a 'InstagramT'.
mapInstagramT :: (m a -> n b) -> InstagramT m a -> InstagramT n b
mapInstagramT f = Is . mapReaderT f . unIs    
    
data IsData = IsData {
        isCreds::Credentials,
        isManager::H.Manager,
        isHost:: ByteString
        } 
        deriving (Typeable)
        
