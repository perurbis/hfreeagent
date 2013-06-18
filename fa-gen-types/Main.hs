{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Blaze.ByteString.Builder      as Builder (fromByteString, toByteString)

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State     (evalStateT, execStateT)
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as LC8
import qualified Data.ByteString.Lazy.Internal as BLI

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T

import           Network.Http.Client
import           OpenSSL                       (withOpenSSL)
import           System.Directory
import           System.FilePath               (splitPath, takeDirectory, (</>))
import           System.IO
import           System.IO.Streams             (InputStream)
import qualified System.IO.Streams             as S
import           System.IO.Streams.Debug
import           System.IO.Streams.File
import           Text.HandsomeSoup
import           Text.Hastache
import           Text.Hastache.Context
import           Text.XML.HXT.Core             hiding (getName)

import           Data.FreeAgent.Generate


faDocsUrl, faDocsHost :: C8.ByteString
faDocsUrl = "https://" `mappend` faDocsHost
faDocsHost = "dev.freeagent.com"

type Url = C8.ByteString
type PageFetch a b = a -> IO b

-- | Get the contents of a webpage from a url
getUrl :: Url -> IO C8.ByteString
getUrl url = withOpenSSL $ do
    ctx <- baselineContextSSL
    let c = openConnectionSSL ctx faDocsHost 443
    r <- req
    withConnection c $ \con -> do
        sendRequest con r emptyBody
        receiveResponse con handle
    where
          req = buildRequest $ do
              http GET $ faDocsUrl `BS.append` url
              setHostname faDocsHost 443
          handle _ input = do
              i2 <- S.map Builder.fromByteString input
              x <- S.fold mappend mempty i2
              return $ Builder.toByteString x

-- | Get a list of links to process from FreeAgent's homepage
docLinks :: IO (InputStream Url)
docLinks = getUrl >=> getLinks >=> S.fromList $ "/docs"

-- | Get a list of links to process from locally stored FreeAgent pages
docLinksLocal :: FilePath -> IO [FilePath]
docLinksLocal topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  return $ map (topdir </>) properNames


-- JSON EXTRACTION

-- | Extract "code" blocks from html and try to parse to a 'Value'
getJsonBlocks :: C8.ByteString -> IO [Maybe Value]
getJsonBlocks doc' = fmap (map extractJson) jsonBlocks
  where doc = parseHtml $ C8.unpack doc'
        jsonBlocks = runX $ doc >>> css "code" //> getText
        extractJson :: String -> Maybe Value
        extractJson = decode' . LC8.pack

-- | Extract internal documentation links from the subnav list
getLinks :: C8.ByteString -> IO [C8.ByteString]
getLinks doc' = fmap (map C8.pack) links
  --doc <- getUrl "/docs" >>= return . parseHtml
  where doc = parseHtml $ C8.unpack doc'
        links = runX $ doc >>> css ".subnav a" ! "href"


-- | Extract json 'Values' from a link to a web page.
-- Web page can be local or remode with the appropriate fetchAction
getJson :: PageFetch a C8.ByteString -> InputStream a -> IO (InputStream Value)
getJson fetchAction = S.mapM (fetchAction >=> getJsonBlocks)
                             -- >=> debugInput lenMaybes "JSON-BLOCKS" S.stdout
                             >=> S.map catMaybes
                             >=> S.concatLists

extractJsonLocal :: IO (InputStream Value)
extractJsonLocal = docLinksLocal "docs" >>= S.fromList >>= getJson C8.readFile

extractByModule :: PageFetch a C8.ByteString
                -> [a]
                -> IO [(a, IO (InputStream Value))]
extractByModule fetch links = do
  forM links $ \l -> do
    inStream <- S.fromList [l]
    return (l, getJson fetch inStream)


extractLocalState :: IO (InputStream Value) -> IO (ApiParseState Value)
extractLocalState vals = do
  lst <- vals >>= S.toList
  let st =  mapM parseTopLevel lst
  execStateT st initParseState


-- allJsonBlocks :: IO [BL.ByteString]
-- allJsonBlocks = getLinks >>= mapM getJsonBlocks >>= return . concat

-- | Mirror all of the FreeAgent docs locally
crawl :: IO ()
crawl = do
  links <- getLinks =<< getUrl "/docs"
  forM_ links $ \lnk -> do
     let fname = C8.unpack $ C8.tail lnk
     putStrLn $ "Fetching " ++ fname
     withFileAsOutputExt fname WriteMode NoBuffering $ \outStream -> do
        inStream <- getUrl lnk >>= S.fromByteString
        S.connect inStream outStream

-- MODULE WRITING
faConfig :: (MonadIO m) => MuConfig m
faConfig = defaultConfig {muEscapeFunc = emptyEscape}

moduleTemplate :: ModuleCtx -> IO BLI.ByteString
moduleTemplate modCtx = hastacheFile faConfig "templates/moduleTemplate" $ mkGenericContext modCtx

writeModule :: ModuleName -> [DataDecl T.Text] -> IO BLI.ByteString
writeModule modName decls = moduleTemplate $ dataDeclsToContext modName decls

main :: IO ()
main = do
  links <- docLinksLocal "docs"
  byModule <- extractByModule C8.readFile links
  generatedModules <- return . fmap catMaybes =<< forM byModule $ \(link, valStream) -> do
    let moduleName = docLinkToModuleName parentModule link
    let fname = "src" </> moduleToFileName moduleName
    let dir = takeDirectory fname
    createDirectoryIfMissing True dir
    st <- extractLocalState valStream
    dataDecls <- return . filter filterDisabled =<< evalStateT (resolveDependencies st) initParseState
    case dataDecls of
      [] -> return Nothing
      _  -> writeModule moduleName dataDecls >>= BL.writeFile fname >> (return . Just $ moduleName)

  parent <- writeParent parentModule generatedModules
  BL.writeFile ("src" </> moduleToFileName parentModule) parent
  mapM_ putStrLn $ parentModule : generatedModules



  where
    -- getLinksFn = docLinks >>= debugInput id "LINKS" S.stdout
    -- getContentFn = getUrl
    filterDisabled dd = (getName dd) `notElem` ["TrialBalanceSummary", "RecurringInvoice"]
    parentModule = "Data.FreeAgent.Types"
    writeParent parentMod generatedMods = hastacheFile faConfig tmpl $ mkStrContext context
      where
        context "modName" = MuVariable parentMod
        context "modules" = MuList $ map (mkStrContext . const . MuVariable)  generatedMods
        context _ = MuVariable ("" :: String)
        tmpl = "templates/parentModuleTemplate"

    getLinksFn = docLinksLocal "docs" >>= S.fromList >>= debugInput C8.pack "LINKS" S.stdout
    getContentFn = C8.readFile


-- UTILITIES

-- | Represent a list of @'Maybe' a@ as a string of @(#Just a's / # total)
lenMaybes :: [Maybe a] -> C8.ByteString
lenMaybes ml = C8.pack $ (lenStr $ catMaybes ml) </> (lenStr ml)
  where lenStr = show . length

-- | Convert from "docs/some_stuff" to Prefix.SomeStuff
convertToModule :: FilePath -> FilePath -> FilePath
convertToModule prefix docName = (prefix'++) . concat $ map camelize pathElems
  where
    pathElems = tail $ splitPath docName
    camelize = T.unpack . toCamelCase . T.pack
    prefix' = prefix++"."

-- TESTING
-- testEstimates inVals = do
--   st <- extractLocalState inVals
--   evalStateT (resolveDependencies st) initParseState

-- justEstimates = (S.fromList ["docs/estimates"] >>= getJson C8.readFile)
