{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.FreeAgent.Types.Notes where

import qualified Data.ByteString as BS
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data

data Note = Note {
    _created_at :: BS.ByteString -- 2012-05-30T10:22:34Z
  , _url        :: BS.ByteString -- https://api.freeagent.com/v2/notes/1
  , _parent_url :: BS.ByteString -- https://api.freeagent.com/v2/contact/1
  , _author     :: BS.ByteString -- Development Team
  , _note       :: BS.ByteString -- A new note
  , _updated_at :: BS.ByteString -- 2012-05-30T10:22:34Z

} deriving (Show, Data, Typeable)

type Notes = [ Note ]

instance FromJSON Note where
  parseJSON (Object v) = Note <$>
                     v .: "created_at" <*>
                     v .: "url" <*>
                     v .: "parent_url" <*>
                     v .: "author" <*>
                     v .: "note" <*>
                     v .: "updated_at"

  parseJSON _            = empty




$(deriveToJSON tail ''Note)
$(makeLenses      ''Note)
