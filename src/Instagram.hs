module Instagram
  (
  RedirectUrl,
  getUserAccessTokenURL1,
  getUserAccessTokenURL2
  ,InstagramT
  ,runInstagramT
  ,runResourceInIs
  ,Credentials(..)
  ,OAuthToken
  ,AccessToken
  ,User
  ,Scope(..)
  ) where

import Instagram.Auth
import Instagram.Monad
import Instagram.Types