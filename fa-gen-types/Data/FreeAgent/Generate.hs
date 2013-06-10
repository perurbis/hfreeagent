{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}



module Data.FreeAgent.Generate where

import           Control.Arrow             (second)
import           Control.Monad             (forM)
import           Control.Monad.Trans.State
import           Data.Aeson                hiding (fromJSON)
import           Data.Char                 (toUpper)
import           Data.Function             (on)
import qualified Data.HashMap.Strict       as HMS
import           Data.List                 (nubBy)
import           Data.Maybe                (catMaybes)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Vector               as V

type Key = T.Text
type FieldRep = T.Text

data DataDecl val = DD {
       dataName :: T.Text
     , fields :: [(Key, val)]
     }
   | Col {
       colName :: T.Text
     , colType :: val
   }
   deriving Eq

getName :: DataDecl t -> T.Text
getName (DD  name _) = name
getName (Col name _) = name

type FieldLookup a = HMS.HashMap [Key] (DataDecl a)

data ApiParseState a = APS {
     resolved    :: [DataDecl a]
   , subObjects  :: FieldLookup a
}

type ApiState      = StateT (ApiParseState Value) IO
type ResolvedState = StateT (ApiParseState T.Text) IO

initParseState :: ApiParseState a
initParseState = APS [] HMS.empty

dataDeclToText :: DataDecl T.Text -> T.Text
dataDeclToText (DD name fields) = T.unlines $ [nameLine, fieldLines, derivingLine]
  where
    nameLine                        = T.unwords ["data", name, "=", name, "{"]
    maxLen                          = maximum . map T.length
    fMax = maxLen [f | (f, _) <- fields]
    fieldLines                      = T.unlines . map fieldToLine $ zip [(0 :: Int)..] fields
    fieldToLine (0, (fname, ftype)) = T.unwords ["   ", T.justifyLeft fMax ' ' fname, "::", ftype]
    fieldToLine (_, (fname, ftype)) = T.unwords ["  ,", T.justifyLeft fMax ' ' fname, "::", ftype]
    derivingLine                    = "} deriving (Show, Data, Typeable)"
dataDeclToText (Col name val) = T.unwords ["type", name, "=", val]

instance Show (DataDecl T.Text) where
  show = T.unpack . dataDeclToText


instance Show (DataDecl Value) where
  show (DD name fields) = unwords ["DD :", T.unpack name, (show $ map fst fields)]
  show (Col name _) = "Col : " ++ T.unpack name

-- | Make a @DataDecl from a name and an Aeson @Value@
parseData :: Key -> Value -> ApiState ()
parseData name (Object o) = do
  (APS resolved subs) <- get
  let dd = DD name fields
  put $ APS (dd `cons` resolved) (HMS.insert fieldNames dd subs)
  _ <- HMS.traverseWithKey extract o
  return ()
  where
    fields = HMS.toList o
    fieldNames = HMS.keys o
    extract k v = parseData (toCamelCase k) v

parseData name arr@(Array a) = do
  _ <- V.forM a $ \obj ->
         parseData (toCamelCase $ unpluralize name) obj
  (APS resolved subs) <- get
  let col = Col name arr
  put $ APS (col `cons` resolved) subs
  return ()
  where unpluralize txt = if T.last txt == 's'
                            then T.init txt
                            else txt

parseData _ _            = return ()

-- | Extract from a top level @Object@
-- For each key in the @Object@ call @parseData@
parseTopLevel :: Value -> ApiState () -- (HMS.HashMap T.Text (Maybe (DataDecl Value)))
parseTopLevel (Object o) = do
  _ <- HMS.traverseWithKey extract o
  return ()
  where
    extract k v = parseData (toCamelCase k) v
parseTopLevel _ = return ()

-- | Get the @T.Text@ name for a field's type.
--
-- Resolves it from the state built up by @parseData@ and @parseTopLevel@
resolveField :: FieldLookup Value -> Value -> FieldRep
resolveField _ (String s) = T.unwords ["BS.ByteString", "--", s]
resolveField subs (Array arr)  =
  case catMaybes $ V.toList possibles of
    (DD name _):_ -> T.unwords ["[", name, "]"]
    _              -> "[UNKNOWN]"
  where
    possibles = V.map (lookup subs) arr
    lookup hm (Object o) = HMS.lookup (HMS.keys o) hm
    lookup _ _           = Nothing
resolveField subs (Object obj) =
  case HMS.lookup (HMS.keys obj) subs of
    Just (DD name _) -> name
    _                -> "UNKNOWN"
resolveField _ (Number _) = "Double"
resolveField _ (Bool _)   = "Bool"
resolveField _ Null       = "Maybe a"

-- | Convert an @ApiParseState@ of @Value@s to a list of printable @DataDecl@s
resolveDependencies :: Monad m => ApiParseState Value -> m [DataDecl FieldRep]
resolveDependencies (APS valDecls subs) = do
  forM (nubBy nameEq valDecls) $ \dataDecl ->
          return $ convertFieldType subs dataDecl
  where nameEq = (==) `on` getName


convertFieldType :: FieldLookup Value -> DataDecl Value -> DataDecl FieldRep
convertFieldType subs (DD name fields) = DD name strFields
  where
    strFields = map (second $ resolveField subs) fields
convertFieldType subs (Col name val) = Col name $ resolveField subs val

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
