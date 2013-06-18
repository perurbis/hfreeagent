{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Users where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

type Users = [ User ]
data User = User {
    _created_at       :: BS.ByteString -- 2011-07-28T11:25:11Z
  , _url              :: BS.ByteString -- https://api.freeagent.com/v2/users/1
  , _permission_level :: Double
  , _role             :: BS.ByteString -- Director
  , _last_name        :: BS.ByteString -- Team
  , _opening_mileage  :: Double
  , _email            :: BS.ByteString -- dev@example.com
  , _updated_at       :: BS.ByteString -- 2011-08-24T08:10:23Z
  , _first_name       :: BS.ByteString -- Development

} deriving (Show, Data, Typeable)



instance FromJSON User where
  parseJSON (Object v) = User <$>
                     v .: "created_at" <*>
                     v .: "url" <*>
                     v .: "permission_level" <*>
                     v .: "role" <*>
                     v .: "last_name" <*>
                     v .: "opening_mileage" <*>
                     v .: "email" <*>
                     v .: "updated_at" <*>
                     v .: "first_name"

  parseJSON _            = empty



$(deriveToJSON tail ''User)
$(makeLenses      ''User)
