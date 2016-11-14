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
    , inProgress :: TVar (S.Set String)
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
    let contextFile = baseOutputDir </> "#status" 
    manager <- newManager tlsManagerSettings
    haveSavedContext <- doesFileExist contextFile
    context <- if haveSavedContext
                then do
                    putStrLn "recovering status"
                    (queue, _ : vis) <- span (/= "") . lines <$> readFile contextFile
                    atomically $ do
                        q <- newTVar $ Seq.fromList queue
                        n <- newTVar $ S.empty
                        v <- newTVar $ S.fromList vis
                        return FetchContext { urlQueue = q, inProgress = n, visited = v }
                else do
                    putStrLn "no saved status"
                    atomically $ do
                        q <- newTVar $ Seq.singleton urlRoot
                        n <- newTVar $ S.empty
                        v <- newTVar $ S.singleton urlRoot
                        return FetchContext { urlQueue = q, inProgress = n, visited = v }

    replicateM_ 20 $ forkIO $ worker manager context baseOutputDir
    writer <- forkIO $
        let loop = do
                threadDelay 1000000

                (q, a, v) <- atomically $ do
                    q <- readTVar (urlQueue context)
                    a <- readTVar (inProgress context)
                    v <- readTVar (visited context)
                    return (q, a, v)

                createDirectoryIfMissing True baseOutputDir
                withFile (baseOutputDir </> "#new-status") WriteMode $ \h -> do
                    mapM_ (hPutStrLn h) a
                    mapM_ (hPutStrLn h) q
                    hPutStrLn h ""
                    mapM_ (hPutStrLn h) v
                renameFile (baseOutputDir </> "#new-status") contextFile
                loop
         in loop

    atomically $ do
        queueEmpty <- Seq.null <$> readTVar (urlQueue context)
        workersInactive <- S.null <$> readTVar (inProgress context)
        unless (queueEmpty && workersInactive) retry

    killThread writer

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
            modifyTVar (inProgress context) (url `S.delete`)

        worker manager context baseOutputDir
    where
    getFirstUrl = atomically $ do
        q <- readTVar $ urlQueue context
        let qview = Seq.viewl q
        case qview of
            EmptyL -> do
                allInactive <- S.null <$> readTVar (inProgress context)
                if allInactive then return Nothing else retry
            topUrl :< qnext -> do
                writeTVar (urlQueue context) qnext
                modifyTVar (inProgress context) (topUrl `S.insert`)
                return $ Just topUrl

    getAndParse url = do
        request <- parseRequest url
        withResponseHistory request manager $ \histResponse -> do
            let response = hrFinalResponse histResponse
            let finalUri = fixPort $ getUri $ hrFinalRequest histResponse
            let baseUri = fixPort $ getUri request
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
            anchors = filter (\t -> isTagOpenName "a" t || isTagOpenName "link" t) tags
         in mapMaybe getHref anchors

    getHref (TagOpen "a" attrs) = lookup "href" attrs
    getHref (TagOpen "link" attrs) = lookup "href" attrs
    getHref t = error $ "Internal error: expected TagOpen 'a' or 'link', got " ++ show t

    fixLink base url = do
        uri <- parseURIReference $ BS8.unpack $ BL.toStrict url
        let absoluteUri = if uriIsAbsolute uri then uri else relativeTo uri base
        let fixedScheme = if null (uriScheme absoluteUri)
                            then absoluteUri { uriScheme = uriScheme base }
                            else absoluteUri
        let fixedPortUri = fixPort fixedScheme
        if fromSameHost base fixedPortUri
            then return $ normalizePathSegments $ show $ fixedPortUri { uriFragment = "" }
            else Nothing

    fixPort uri = let urischeme = uriScheme uri
                      uriport = uriPort <$> uriAuthority uri
                   in if (urischeme == "https:" && uriport == Just ":443") ||
                            (urischeme == "http:" && uriport == Just ":80")
                        then uri { uriAuthority = (\a -> a { uriPort = "" }) <$> uriAuthority uri }
                        else uri
    fromSameHost uri1 uri2 = uriScheme uri1 == uriScheme uri2 && uriAuthority uri1 == uriAuthority uri2
    isText response = (BS8.isPrefixOf "text/" <$> responseContentType response) == Just True
    isHtml response = (BS8.isPrefixOf "text/html" <$> responseContentType response) == Just True

    uriToPath uri =
        let base = uriPath uri </> uriQuery uri
            expanded = if last base == '/' then base ++ "index.html" else base
         in tail expanded
