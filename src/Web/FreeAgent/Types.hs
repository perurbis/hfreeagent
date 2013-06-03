{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}



module Web.FreeAgent.Types where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.State
import           Data.Aeson                hiding (fromJSON)
import qualified Data.ByteString.Char8     as BS
import           Data.Char                 (toUpper)
import           Data.Data
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe (catMaybes)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Vector               as V

type Token = BS.ByteString
type Key = T.Text
type FieldRep = T.Text

data AccessTokenResp = AccessTokenResp {
       access_token  :: BS.ByteString
     , token_type    :: BS.ByteString
     , expires_in    :: Int
     --, refresh_token :: BS.ByteString
     } deriving (Show, Data, Typeable)

data DataDecl val = DD {
       dataName :: T.Text
     , fields :: [(Key, val)]
     }
   | Col {
       colName :: T.Text
     , colType :: val
   } 
   deriving Eq

type FieldLookup a = HMS.HashMap [Key] (DataDecl a)

data ApiParseState a = APS {
     resolved    :: [DataDecl a]
   , subObjects  :: FieldLookup a
}

initParseState :: ApiParseState a
initParseState = APS [] HMS.empty

type ApiState = StateT (ApiParseState Value) IO

dataDeclToText :: DataDecl T.Text -> T.Text
dataDeclToText (DD name fields) = T.unlines $ [nameLine, fieldLines, derivingLine]
  where
    nameLine                        = T.unwords ["data", name, "=", name, "{"]
    fieldLines                      = T.unlines . map fieldToLine $ zip [(0 :: Int)..] fields
    fieldToLine (0, (fname, ftype)) = T.unwords ["    ", fname, "::", ftype]
    fieldToLine (_, (fname, ftype)) = T.unwords ["  ,", fname, "::", ftype]
    derivingLine                    = "} deriving (Show, Data, Typeable)"
dataDeclToText (Col name _) = T.unwords ["[", name, "]"]

instance Show (DataDecl T.Text) where
  show = T.unpack . dataDeclToText


instance Show (DataDecl Value) where
  show (DD name fields) = unwords ["DD :", T.unpack name, (show $ map fst fields)]
  show (Col name _) = "Col : " ++ T.unpack name
  
parseData :: Key -> Value -> ApiState (Maybe (DataDecl Value))
parseData name obj@(Object o) = do
  (APS resolved subs) <- get
  let dd = DD name fields
  put $ APS (dd `cons` resolved) subs
  putLookup obj dd
  return $ Just dd
  where
    fields = HMS.toList o

parseData name arr@(Array a) = do
  (APS resolved subs) <- get
  let col = Col name arr
  put $ APS (col `cons` resolved) subs
  putLookup arr col
  return $ Just col
  where
    fields = HMS.toList . HMS.unions . map getHMS $ V.toList a
    getHMS (Object obj) = obj
    getHMS _ = HMS.empty
parseData _ _            = return Nothing


putLookup :: Value -> DataDecl Value -> ApiState ()
putLookup val dataDecl = do
  (APS resolved subs) <- get
  case val of
    Object _  -> doPut dataDecl val
    Array arr -> mapM_ (doPut dataDecl) $ V.toList arr
    _         -> return ()
  where
    doPut dd obj@(Object o) = do
      (APS resolved subs) <- get
      put $ APS resolved (HMS.insert fields dd subs)
      where
        fields = HMS.keys o

parseTopLevel :: Value -> ApiState (HMS.HashMap T.Text (Maybe (DataDecl Value)))
parseTopLevel (Object o) = do
  liftIO (print o)
  HMS.traverseWithKey extract o
  where
    extract k v = parseData (toCamelCase k) v

parseTopLevel _ = return HMS.empty

parseField :: Key -> Value -> ApiState FieldRep
parseField _ (String s) = return $ "BS.ByteString            --" `T.append` s
parseField k (Array arr)  = do
  (APS resolved subs) <- get
  let possibles = V.map (lookup subs) arr
  case catMaybes $ V.toList possibles of
    (Col name _):_ -> return $ T.unwords ["[", name, "]"]
    []             -> return "[UNKNOWN]"
  where
    lookup hm (Object o) = HMS.lookup (HMS.keys o) hm
-- case parseData (toCamelCase k) arr of
--   Just dd -> collectionToText $ Col dd
--   Nothing -> "[SubObject]"
parseField k (Object obj) = do
  (APS resolved subs) <- get
  case HMS.lookup (HMS.keys obj) subs of
    Just (DD name _) -> return name
    _                -> return "UNKNOWN"
parseField _ (Number _) = return "Double"
parseField _ (Bool _)   = return "Bool"
parseField _ Null       = return "Maybe a"


resolveFields :: ApiState -> 

-- UTILS
toCamelCase :: T.Text -> T.Text
toCamelCase = T.concat . map capFirst . T.splitOn "_"
  where capFirst word = (toUpper $ T.head word) `T.cons` T.tail word

toKeySet :: [(Key, FieldRep)] -> [Key]
toKeySet fields = S.toList . S.fromList $ map fst fields

cons :: (Eq a) => a -> [a] -> [a]
cons a as = case a `elem` as of
  True  -> as
  False -> a:as