{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified System.Locale as Locale
import qualified Data.ConfigFile as CF
import qualified Control.Monad.Error as Error

import qualified System.IO as IO
import qualified Network as N
{-import qualified GHC.IO.Handle-}

import qualified Data.ByteString as BS hiding (putStrLn, hPutStrLn) -- for hGetContents
import qualified Data.ByteString.Char8 as C -- for pack
-- import qualified Data.Word as W -- for pack

import Data.Attoparsec.ByteString.Char8 as AP
import Control.Applicative -- for <|>
import Control.Monad -- for mzero

import qualified System.Directory as Directory -- for doesFileExist
import qualified System.FilePath as FilePath -- for doesFileExist
import System.FilePath ((</>))

import qualified Control.Exception as E
import qualified Control.Concurrent as Con
import qualified System.Posix.Signals
import qualified Debug.Trace as Debug

data Settings = Settings {
  port :: N.PortNumber,
  domainRoot :: FilePath.FilePath,
  mediateSwitch :: Bool,
  mediateHost :: String,
  mediatePort :: N.PortNumber
}

testString :: C.ByteString
testString = "Let the sky fall"

-- debugGetLine handle = do
                        -- line <- C.hGetLine handle
                        -- Debug.trace (C.unpack "Grabbing line: " ++ show line) (return line)
debugGetLine handle = C.hGetLine handle
{-- INLINE --}

-- testChar :: W.Word8
-- testChar = ':'

-- http://en.wikipedia.org/wiki/HTTP_persistent_connection
-- "Connection: Keep-Alive" is the default so we cannot use `hGetContents` as 
-- it will block.

-- http://stackoverflow.com/questions/8042286/haskell-how-to-use-forkio-to-enable-multiple-clients-to-connect-to-a-server

-- `withSocketsDo` is for portability with Windows (not really required)
main = N.withSocketsDo $ do
  tid <- Con.myThreadId

  -- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigINT (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigTERM (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigINT Ignore Nothing -- ignore Ctrl-C
  --
  -- val <- readfile emptyCP "conf.ini"
  -- let cp = forceEither val
  -- putStrLn "Your setting is:"
  -- putStrLn $ forceEither $ get cp "Server" "port"
  eitherSettings <- Error.runErrorT $
    do
      cp <- join $ Error.liftIO $ CF.readfile CF.emptyCP "conf.ini"
      -- Server settings
      p <- (CF.get cp "Server" "port") :: Error.MonadError CF.CPError m => m Int
      d <- (CF.get cp "Server" "domain-root")

      -- Mediate settings
      m_o <- (CF.get cp "Mediate" "mediate")
      m_h <- (CF.get cp "Mediate" "hostname")
      m_p <- (CF.get cp "Mediate" "port") :: Error.MonadError CF.CPError m => m Int
      -- if m_h == "127.0.0.1" && (p == m_p) then (CF.OtherProblem "Cannot mediate to self", "Cannot mediate to self" ) else (CF.OtherProblem "Cannot mediate to self", "Cannot mediate to self")
      if m_h == "127.0.0.1" && (p == m_p) then Error.throwError (CF.OtherProblem "Cannot mediate to self", "Cannot mediate to self") else return "LOL"
      Error.liftIO $ putStrLn (show p)
      Error.liftIO $ putStrLn d
      d2 <- Error.liftIO $ Directory.canonicalizePath d
      return Settings { port = (fromIntegral p),
                        domainRoot = d2,
                        mediateSwitch = m_o,
                        mediateHost = m_h,
                        mediatePort = (fromIntegral m_p) }

  case eitherSettings of
    Either.Left err -> error $ "Failed parsing conf.ini: " ++ show err
    Either.Right settings -> do
      putStrLn $ "Started listening on port " ++ show (port settings)
      sock <- N.listenOn (N.PortNumber (port settings))
      acceptConnections sock settings
  -- forever $ do 
  --   (handle, hostname, portnumber) <- N.accept sock
  --   putStrLn $ "Got connection from " ++ hostname ++ ":" ++ show portnumber
  --   IO.withFile "http.log" IO.AppendMode (\file -> do
  --     IO.hPutStrLn file $ "Got connection from " ++ hostname ++ ":" ++ show portnumber
  --     )
  --   Con.forkIO $ acceptConn handle `E.catch` interrupt sock handle
  -- where interrupt sock handle exception =
  --         let err = show (exception :: E.SomeException) in do
  --           IO.hPutStrLn IO.stderr $ "Some exception caught: " ++ err
  --           putStrLn "Closing socket."
  --           N.sClose sock


acceptConnections sock settings = do
  putStrLn "trying to accept" -- debug msg
  conn@(handle,host,port) <- N.accept sock
  print conn -- debug msg
  Con.forkIO $ catch (acceptConn handle settings `E.finally` IO.hClose handle) (\e -> print e)
  acceptConnections sock settings



takeWhile' :: (IO Bool) -> [IO t] -> IO [t]
takeWhile' test (a:as) = do
  x <- a
  continue <- test
  if continue
    then do xs <- (takeWhile' test as)
            return (x:xs)
    else do return []

mediate' = mediate IO.stdin
mediate handle host port request = do
    -- Establish a socket for communication
    mediate_handle <- N.connectTo host port
    IO.hSetBuffering mediate_handle IO.NoBuffering
    -- IO.hSetBuffering mediate_handle IO.LineBuffering

    -- Send request
    print "Sending browser request"
    C.hPutStr mediate_handle request

    -- Get answer
    print "Fetching server answer"
    -- These methods wait forever for more input
    -- answer <- C.hGetContents mediate_handle
    -- print answer
    -- ans_line_actions <- return $ repeat $ debugGetLine mediate_handle
    -- ans_lines <- takeWhile' (/="\r") $ ans_line_actions
    -- C.hPutStrLn handle $ C.unlines ans_lines
    -- C.hGetContents mediate_handle >>= C.hPutStrLn handle

    -- Make a list of actions that fetch one line each
    request_line_actions <- return $ repeat $ debugGetLine mediate_handle
    -- Take lines until we wait more than 1 millisecond for more input
    request_lines <- takeWhile' (IO.hWaitForInput mediate_handle 1) $ request_line_actions
    -- Set line delimiter
    let end = if last (C.unpack$head request_lines) == '\r' then "\r" else ""
    -- Reconstruct request
    let request = C.concat $ map (\l -> C.concat [l, "\n"]) $ request_lines ++ [end]
    C.hPutStrLn handle request

    IO.hClose mediate_handle


acceptConn handle settings = do
  -- Make a list of actions that fetch one line each
  request_line_actions <- return $ repeat $ debugGetLine handle
  -- Take lines until we wait more than 1 millisecond for more input
  request_lines <- takeWhile' (IO.hWaitForInput handle 1) $ request_line_actions
  -- Set line delimiter
  let end = if last (C.unpack$head request_lines) == '\r' then "\r" else ""
  -- Reconstruct request
  let request = C.concat $ map (\l -> C.concat [l, "\n"]) $ request_lines ++ [end]

  if mediateSwitch settings
    then mediate handle (mediateHost settings) (N.PortNumber (mediatePort settings)) request
    else do
      -- initiate the parser with at least one line of input
      -- text <- debugGetLine handle
      -- parse and let the parser get more input with `debugGetLine`
      -- Done unparsed_text (action, url) <- parseWith (debugGetLine handle) httpRequestParser text
      -- IO.withFile "http.log" IO.AppendMode (\file -> do
      --     C.hPutStr file unparsed_text
      --     )
      -- Write request to log
      IO.withFile "http.log" IO.AppendMode (\file -> do
          C.hPutStrLn file request
          )
      -- Parse request
      _ <- case parse httpRequestParser request of
             Fail unparsed_text l_str str     -> badRequest handle
             Partial f                        -> badRequest handle
             Done unparsed_text (action, url, headers) -> if url == "/json"
               then jsonRequest handle (action, url, headers) settings request
               else goodRequest handle (action, url, headers) settings
      return ()

removePortPart host = C.takeWhile (/= ':') host
removeWWWPart host = if C.length host > 4 && C.take 4 host == "www." then C.drop 4 host
                     else host

jsonRequest handle (action, url, headers) settings request = do
  _ <- case Map.lookup "JSON-URL" (Map.fromList headers) of
                   Just some_url -> mediate handle (C.unpack some_url) (N.PortNumber 80) request
                   Nothing -> return ()
  return ()

goodRequest handle (action, url, headers) settings = do
  let base_filepath = if url == "/" then "." else C.dropWhile (== '/') url
  -- Get "Host" header
  let host = case Map.lookup "Host" (Map.fromList headers) of
               -- Remove any port part
               Just hostWithPort -> (removeWWWPart.removePortPart) hostWithPort
               Nothing -> ""
  -- Print "Host: ..."
  C.putStrLn $ C.append "Host: " host

  let filepath_req = (domainRoot settings) </> C.unpack host </> C.unpack base_filepath
  --
  -- Check if legal path (no '..')
  let isValidPath = not $ ".." `elem` (FilePath.splitDirectories filepath_req)

  if not isValidPath then notFound handle else do

    -- Check if legal path (inside domainRoot)
    -- filepath_req <- Directory.canonicalizePath $ (domainRoot settings) </> C.unpack host </> C.unpack base_filepath
    -- let isValidPath = (domainRoot settings) `List.isPrefixOf` filepath_req

    C.putStrLn $ C.append "Grabbing file: " (C.pack filepath_req)

    -- check if directory
    isDir <- Directory.doesDirectoryExist filepath_req
    let filepath = if isDir
                      then filepath_req </> "index.html"
                      else filepath_req

    -- check if file exists
    fExists <- Directory.doesFileExist filepath
    if fExists
      then IO.withFile filepath IO.ReadMode (\file -> do
        filesize <- IO.hFileSize file
        -- hFileSize is slow?
        -- http://stackoverflow.com/questions/5620332/what-is-the-best-way-to-retrieve-the-size-of-a-file-in-haskell

        -- check extension and add Content-Type header
        -- let ext = reverse $ List.takeWhile (/= '.') $ reverse filepath
        let ext = dropWhile (=='.') $ FilePath.takeExtension filepath
        let headers = case Map.lookup ext mimeTypes of
                        Just mimeType -> [("Content-Type", mimeType)]
                        Nothing -> []

        -- header <- httpResponse 200 [("Content-Type", "text/html"), ("Content-Length", (C.pack$show$filesize))] ""
        header <- httpResponse 200 (headers++[("Content-Length", (C.pack$show$filesize))]) ""
        C.hPutStr handle header
        (C.hPutStr handle =<< C.hGetContents file)
        -- C.hPutStr handle $ readFile $ tail url
        )
       else notFound handle

badRequest handle = do
  print "worked2"
  httpResponse 400 [] "400 Bad Request" >>= C.hPutStr handle

notFound handle = do
  print "worked3"
  httpResponse 404 [] "404 Not Found" >>= C.hPutStr handle
                                -- "Content-Type: text/html",
                                -- C.append "Content-Length: " (C.pack$show$filesize),
                                -- ""]

  -- N.sClose sock
statusMsgs = Map.fromList [ (200, "OK"), (400, "Bad Request"), (404, "Not Found") ]

-- http://www.iana.org/assignments/media-types
mimeTypes = Map.fromList [
              ("html", "text/html"),
              ("css", "text/css"),
              ("js", "application/javascript"),
              ("jpg", "image/jpeg"),
              ("png", "image/png"),
              ("svg", "image/svg+xml")
            ]

httpResponse :: Int -> [(C.ByteString, C.ByteString)] -> C.ByteString -> IO C.ByteString
httpResponse statusCode headers content = do
  cTime <- Time.getCurrentTime
  let Just msg = Map.lookup statusCode statusMsgs
  -- let date_header = ("Date", C.pack$Time.formatTime Locale.defaultTimeLocale "%a, %e %b %Y %T %Z" cTime)
  let date_header = ("Date", C.pack$Time.formatTime Locale.defaultTimeLocale "%a, %e %b %Y %T GMT" cTime)
  let header_lines = map (\(a,b) -> a `C.append` ": " `C.append` b `C.append` "\r") (date_header:headers)
  let headers_str = C.unlines header_lines
  -- "Date: Fri, 31 Dec 1999 23:59:59 GMT",
  return $ C.concat [
      "HTTP/1.0 ",
      (C.pack.show) statusCode,
      " ",
      msg,
      "\r\n",
      headers_str,
      "\r\n",
      content
    ]

-- httpRequestParser :: Parser BS.ByteString
httpRequestParser = do
  action <- string "GET" <|> string "POST"
  space
  url <- AP.takeWhile (not.isSpace)
  space
  string "HTTP/"
  httpVersion <- rational
  --Debug.traceShow httpVersion (return "")
  string "\n" <|> string "\r\n"
  options <- manyTill (do
                     optionName <- takeWhile1 (/= ':')
                     string ": "
                     optionValue <- takeWhile1 (\c -> c /= '\n' && c /= '\r')
                     string "\n" <|> string "\r\n"
                     return (optionName, optionValue))
                      (string "\n" <|> string "\r\n")
  --Debug.traceShow options (return "")
  return (action, url, options)




-- End of line
-- eol
