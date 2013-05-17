{-# LANGUAGE OverloadedStrings          #-}
module Main where

--import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import           Data.Monoid
import           Network.Http.Client
import           OpenSSL                       (withOpenSSL)
import           System.IO.Streams             (InputStream)
import qualified System.IO.Streams             as S
import           Text.HandsomeSoup
import           Text.XML.HXT.Core


--import qualified Data.ByteString.Char8 as B
--import Control.Monad.Maybe

faDocsUrl = "https://" `mappend` faDocsHost
faDocsHost = "dev.freeagent.com"

getUrl :: C8.ByteString -> IO [Char]
getUrl url = withOpenSSL $ do
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx faDocsHost 443
    r <- req
    sendRequest c r emptyBody
    resp <- receiveResponse c concatHandler
    closeConnection c
    return $ C8.unpack resp
    where req = buildRequest $ do
          http GET $ faDocsUrl `BS.append` url
          setHostname faDocsHost 443

main = do
  doc <- getUrl "/docs" >>= return . parseHtml
  links <- runX $ doc >>> css ".subnav a" ! "href"
  print "got links"
  mapM_ print links
  
    