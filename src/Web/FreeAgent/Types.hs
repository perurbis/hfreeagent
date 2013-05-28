{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}



module Web.FreeAgent.Types where

import           Control.Monad.Trans.State
import           Data.Aeson hiding (fromJSON)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (toUpper)
import           Data.Data
import qualified Data.HashMap.Strict   as HMS
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Vector           as V

type Token = BS.ByteString
type Key = T.Text
type FieldRep = T.Text

data AccessTokenResp = AccessTokenResp {
       access_token  :: BS.ByteString
     , token_type    :: BS.ByteString
     , expires_in    :: Int
     --, refresh_token :: BS.ByteString
     } deriving (Show, Data, Typeable)

data DataDecl = DD {
     dataName :: T.Text
   , fields :: [(Key, FieldRep)]
} deriving Eq

data Collection a = Col a

type FieldLookup = HMS.HashMap (S.Set Key) DataDecl

data ApiParseState = APS {
     fieldLookup :: FieldLookup
   , resolved    :: [DataDecl]
   , unreolved   :: [Value]
}

newtype ApiState = State ApiParseState

dataDeclToText :: DataDecl -> T.Text
dataDeclToText (DD name fields) = T.unlines $ [nameLine, fieldLines, derivingLine]
  where
    nameLine                        = T.unwords ["data", name, "=", name, "{"]
    fieldLines                      = T.unlines . map fieldToLine $ zip [(0 :: Int)..] fields
    fieldToLine (0, (fname, ftype)) = T.unwords ["    ", fname, "::", ftype]
    fieldToLine (_, (fname, ftype)) = T.unwords ["  ,", fname, "::", ftype]
    derivingLine                    = "} deriving (Show, Data, Typeable)"

collectionToText :: Collection DataDecl -> T.Text
collectionToText (Col (DD name _)) = T.unwords ["[", name, "]"]

instance Show DataDecl where
  show = T.unpack . dataDeclToText

instance Show (Collection DataDecl) where
  show = T.unpack . collectionToText

parseData :: Key -> Value -> Maybe DataDecl
parseData name (Object o) = return $ DD name fields
  where
    fields             = reverse $ HMS.foldrWithKey getFields [] o
    getFields k v seed = (k, parseField k v) : seed
    
parseData name (Array a) = parseData name $ V.head a
parseData _ _            = Nothing

parseField :: Key -> Value -> FieldRep
parseField _ (String s) = "BS.ByteString --" `T.append` s
parseField k arr@(Array _)  = case parseData (unCamel k) arr of
  Just dd -> collectionToText $ Col dd
  Nothing -> "[SubObject]"
parseField k (Object _) = "SubObject"
parseField _ (Number _) = "Double"
parseField _ (Bool _)   = "Bool"
parseField _ Null       = "Maybe a"


-- UTILS
unCamel :: T.Text -> T.Text
unCamel = T.concat . map capFirst . T.splitOn "_"
  where capFirst word = (toUpper $ T.head word) `T.cons` T.tail word

toKeySet :: [(Key, FieldRep)] -> S.Set Key
toKeySet fields = S.fromList $ map fst fields
