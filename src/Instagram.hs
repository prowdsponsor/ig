-- | the public API for Instagram access
module Instagram
  (
  -- generic types and functions
  InstagramT
  ,runInstagramT
  ,runResourceInIs
  ,IGException

  -- authentication
  ,RedirectUri
  ,getUserAccessTokenURL1
  ,getUserAccessTokenURL2
  ,Credentials(..)
  ,OAuthToken(..)
  ,AccessToken
  ,User(..)
  ,Scope(..)
  
  -- data
  ,Envelope(..)
  ,Pagination(..)
  ,Media(..)
  ,Location(..)
  ,ImageData(..)
  ,Images(..)
  ,Caption(..)
  ,Collection(..)
    
  -- user
  ,RecentParams(..)
  ,getRecent
  ,SelfLikedParams(..)
  ,getSelfLiked
  ) where

import Instagram.Auth
import Instagram.Monad
import Instagram.Types
import Instagram.Users