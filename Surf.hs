{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import qualified Data.Either as Either
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

import qualified System.Directory as Folder -- for doesFileExist

import qualified Control.Exception as E
import qualified Control.Concurrent as Con
import qualified System.Posix.Signals
import qualified Debug.Trace as Debug

port = 80

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
  eitherPort <- Error.runErrorT $
    do
      cp <- join $ Error.liftIO $ CF.readfile CF.emptyCP "conf.ini"
      p <- (CF.get cp "Server" "port") :: Error.MonadError CF.CPError m => m Int
      Error.liftIO $ putStrLn (show p)
      return p

  case eitherPort of
    Either.Left err -> error "Failed parsing conf.ini"
    Either.Right port -> do
      putStrLn $ "Started listening on port " ++ show port
      sock <- N.listenOn (N.PortNumber (fromIntegral port))
      acceptConnections sock
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


acceptConnections sock = do
  putStrLn "trying to accept" -- debug msg
  conn@(handle,host,port) <- N.accept sock
  print conn -- debug msg
  Con.forkIO $ catch (acceptConn handle `E.finally` IO.hClose handle) (\e -> print e)
  acceptConnections sock


mediateSwitch = False
local_hostname = "127.0.0.1"
local_port = 9000

takeWhile' :: (t -> Bool) -> [IO t] -> IO [t]
takeWhile' test (a:as) = do
  x <- a
  if test x 
    then do xs <- (takeWhile' test as)
            return (x:xs)
    else do return []

mediate' = mediate IO.stdin
mediate handle = do


    -- Establish a socket for communication
    mediate_handle <- N.connectTo local_hostname (N.PortNumber local_port)
    IO.hSetBuffering mediate_handle IO.NoBuffering
    -- IO.hSetBuffering mediate_handle IO.LineBuffering

    -- Send request
    print "Sending browser request"
    req_line_actions <- return $ repeat $ debugGetLine handle
    req_lines <- takeWhile' (/="\r") $ req_line_actions
    C.hPutStr mediate_handle $ C.unlines $ req_lines ++ ["\r"]

    -- Get answer
    print "Fetching server answer"
    -- answer <- C.hGetContents mediate_handle
    -- print answer

    -- C.hPutStrLn handle
    -- ans_line_actions <- return $ repeat $ debugGetLine mediate_handle
    -- ans_lines <- takeWhile' (/="\r") $ ans_line_actions
    -- C.hPutStrLn handle $ C.unlines ans_lines
    C.hGetContents mediate_handle >>= C.hPutStrLn handle

    IO.hClose mediate_handle


acceptConn handle = do

  if mediateSwitch
    then mediate handle
    else do
      -- initiate the parser with at least one line of input
      -- text <- debugGetLine handle
      -- parse and let the parser get more input with `debugGetLine`
      -- Done unparsed_text (action, url) <- parseWith (debugGetLine handle) httpRequestParser text
      -- IO.withFile "http.log" IO.AppendMode (\file -> do
      --     C.hPutStr file unparsed_text
      --     )
      request_line_actions <- return $ repeat $ debugGetLine handle
      request_lines <- takeWhile' (\c-> c/="\r" && c/="") $ request_line_actions
      let end = if last (C.unpack$head request_lines) == '\r' then "\r" else ""
      let request = C.concat $ map (\l -> C.concat [l, "\n"]) $ request_lines ++ [end]
      IO.withFile "http.log" IO.AppendMode (\file -> do
          C.hPutStrLn file request
          )
      _ <- case parse httpRequestParser request of
             Fail unparsed_text l_str str     -> badRequest handle
             Partial f                        -> badRequest handle
             Done unparsed_text (action, url) -> goodRequest handle (action, url)
      IO.hClose handle

goodRequest handle (action, url) = do
  filepath <- if url == "/" then return "index.html" else return $ C.tail url

  fExists <- Folder.doesFileExist (C.unpack filepath)
  if fExists
    then IO.withFile (C.unpack filepath) IO.ReadMode (\file -> do
      filesize <- IO.hFileSize file
      -- hFileSize is slow?
      -- http://stackoverflow.com/questions/5620332/what-is-the-best-way-to-retrieve-the-size-of-a-file-in-haskell

      header <- httpResponse 200 $ C.unlines ["Content-Type: text/html", C.append "Content-Length: " (C.pack$show$filesize), ""]
      C.hPutStr handle header
      (C.hPutStr handle =<< C.hGetContents file)
      -- C.hPutStr handle $ readFile $ tail url
      )
     else notFound handle

badRequest handle = do
  httpResponse 400 "400 Bad Request" >>= C.hPutStr handle

notFound handle = do
  httpResponse 404 "404 Not Found" >>= C.hPutStr handle
                                -- "Content-Type: text/html",
                                -- C.append "Content-Length: " (C.pack$show$filesize),
                                -- ""]

  -- N.sClose sock

statusMsgs = Map.fromList [ (200, "OK"), (400, "Bad Request"), (404, "Not Found") ]

httpResponse :: Int -> C.ByteString -> IO C.ByteString
httpResponse statusCode content = do
  cTime <- Time.getCurrentTime
  let Just msg = Map.lookup statusCode statusMsgs
  let time = Time.formatTime Locale.defaultTimeLocale "Date: %a, %e %b %Y %T %Z" cTime
  -- "Date: Fri, 31 Dec 1999 23:59:59 GMT",
  return $ C.concat [
      "HTTP/1.0 ",
      (C.pack.show) statusCode,
      " ",
      msg,
      "\n",
      C.pack time,
      "\n",
      "\n",
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
  return (action, url)




-- End of line
-- eol
