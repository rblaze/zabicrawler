{-# Language OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Sequence (Seq, (|>), ViewL(..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.URI
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import qualified Data.Set as S

data FetchContext = FetchContext
    { urlQueue :: TVar (Seq String)
    , activeFetchers :: TVar Int
    , visited :: TVar (S.Set String)
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [what, whereTo] -> fetch what whereTo
        _ -> putStrLn "usage: zabicrawler http://what.to.download where_to_put"

fetch :: String -> FilePath -> IO ()
fetch urlRoot baseOutputDir = do
    manager <- newManager tlsManagerSettings
    context <- atomically $ do
                q <- newTVar $ Seq.singleton urlRoot
                n <- newTVar 0
                v <- newTVar $ S.singleton urlRoot
                return FetchContext { urlQueue = q, activeFetchers = n, visited = v }

    replicateM_ 20 $ forkIO $ worker manager context baseOutputDir

    atomically $ do
        queueEmpty <- Seq.null <$> readTVar (urlQueue context)
        nActive <- readTVar $ activeFetchers context
        unless (queueEmpty && nActive == 0) retry

worker :: Manager -> FetchContext -> FilePath -> IO ()
worker manager context baseOutputDir = do
    topUrl <- getFirstUrl
    whenJust topUrl $ \url -> do
        moreUrls <- catch (getAndParse url) $
            \e -> do
                putStrLn $ url ++ " " ++ show (e :: SomeException)
                return []

        atomically $ do
            v <- readTVar (visited context)
            vnext <- foldM (\vis nextUrl ->
                    if nextUrl `S.notMember` vis
                        then do
                            modifyTVar (urlQueue context) (|> nextUrl)
                            return $ nextUrl `S.insert` vis
                        else return vis
                ) v moreUrls
            writeTVar (visited context) vnext
            modifyTVar (activeFetchers context) (\n -> n - 1)

        worker manager context baseOutputDir
    where
    getFirstUrl = atomically $ do
        q <- readTVar $ urlQueue context
        let qview = Seq.viewl q
        case qview of
            EmptyL -> do
                nActive <- readTVar $ activeFetchers context
                if nActive /= 0 then retry else return Nothing
            topUrl :< qnext -> do
                writeTVar (urlQueue context) qnext
                modifyTVar (activeFetchers context) (+ 1)
                return $ Just topUrl
    getAndParse url = do
        request <- parseRequest url
        withResponseHistory request manager $ \histResponse -> do
            let response = hrFinalResponse histResponse
            let finalUri = getUri $ hrFinalRequest histResponse
            let baseUri = getUri request
            let status = responseStatus response
            let contentType = responseContentType response
            putStrLn $ show baseUri ++ " -> " ++ show finalUri ++ " " ++ show status ++ " " ++ show contentType

            if statusIsSuccessful status && fromSameHost baseUri finalUri && isText response
                then do
                    let relativePath = uriToPath finalUri
                    let outputPath = baseOutputDir </> relativePath
                    let tempPath = outputPath ++ "#part"

                    createDirectoryIfMissing True $ takeDirectory outputPath
                    withFile tempPath WriteMode $ \h -> do
                        let loop = do
                                part <- brRead $ responseBody response
                                unless (BS8.null part) $ do
                                    BS8.hPut h part
                                    loop
                        loop
                    renameFile tempPath outputPath

                    if isHtml response
                        then mapMaybe (fixLink finalUri) . getLinks <$> BL.readFile outputPath
                        else return []
                else return []
    whenJust Nothing _ = return ()
    whenJust (Just a) f = f a
    responseContentType = lookup hContentType . responseHeaders
    getLinks body =
        let tags = parseTagsOptions parseOptionsFast body
            anchors = filter (isTagOpenName "a") tags
         in mapMaybe getHref anchors
    getHref (TagOpen "a" attrs) = lookup "href" attrs
    getHref t = error $ "Internal error: expected TagOpen 'a', got " ++ show t
    fixLink base url = do
        uri <- parseURIReference $ BS8.unpack $ BL.toStrict url
        let absoluteUri = if uriIsAbsolute uri then uri else relativeTo uri base
        if fromSameHost base absoluteUri
            then return $ normalizePathSegments $ show $ absoluteUri { uriFragment = "" }
            else Nothing
    fromSameHost uri1 uri2 = uriScheme uri1 == uriScheme uri2 && uriAuthority uri1 == uriAuthority uri2
    isText response = (BS8.isPrefixOf "text/" <$> responseContentType response) == Just True
    isHtml response = (BS8.isPrefixOf "text/html" <$> responseContentType response) == Just True
    uriToPath uri =
        let base = uriPath uri </> uriQuery uri
            expanded = if last base == '/' then base ++ "index.html" else base
         in tail expanded
