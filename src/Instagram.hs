-- | the public API for Instagram access
module Instagram
  (
  -- generic types and functions
  InstagramT
  ,runInstagramT
  ,runResourceInIs
  ,ISException

  -- authentication
  ,RedirectUri
  ,getUserAccessTokenURL1
  ,getUserAccessTokenURL2
  ,Credentials(..)
  ,OAuthToken
  ,AccessToken
  ,User
  ,Scope(..)
  ) where

import Instagram.Auth
import Instagram.Monad
import Instagram.Types