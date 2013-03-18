{-# LANGUAGE OverloadedStrings #-}
import qualified System.IO as IO
import qualified Network as N
{-import qualified GHC.IO.Handle-}

import qualified Data.ByteString as BS hiding (putStrLn, hPutStrLn) -- for hGetContents
import qualified Data.ByteString.Char8 as C -- for pack
-- import qualified Data.Word as W -- for pack

import Data.Attoparsec.ByteString.Char8 as AP
import Control.Applicative -- for <|>
import Control.Monad -- for mzero

import qualified Control.Exception as E
import qualified Control.Concurrent as Con
import qualified System.Posix.Signals
import qualified Debug.Trace as Debug

port = 8000

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

  putStrLn $ "Started listening on port " ++ show port
  sock <- N.listenOn (N.PortNumber port)
  forever $ do 
    (handle, hostname, portnumber) <- N.accept sock
    putStrLn $ "Got connection from " ++ hostname ++ ":" ++ show portnumber
    Con.forkIO $ acceptConn handle `E.catch` interrupt sock
  where interrupt sock exception =
          let err = show (exception :: E.SomeException) in do
            IO.hPutStrLn IO.stderr $ "Some exception caught: " ++ err
            putStrLn "Closing socket."
            N.sClose sock

mediateSwitch = False
local_hostname = "127.0.0.1"
local_port = 9000

takeWhile' :: (t -> Bool) -> [IO t] -> IO [t]
takeWhile' test (a:as) = do
  x <- a
  -- return [x]
  if test x then do
    xs <- (takeWhile' test as)
    return (x:xs)
  else
    return []

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

  if mediateSwitch then
    mediate handle
  else do
    -- initiate the parser with at least one line of input
    text <- debugGetLine handle
    -- parse and let the parser get more input with `debugGetLine`
    Done unparsed_text (action, url) <- parseWith (debugGetLine handle) httpRequestParser text

    filepath <- if url == "/" then return "index.html" else return $ C.tail url

    IO.withFile (C.unpack filepath) IO.ReadMode (\file -> do
      filesize <- IO.hFileSize file
      -- hFileSize is slow?
      -- http://stackoverflow.com/questions/5620332/what-is-the-best-way-to-retrieve-the-size-of-a-file-in-haskell

      C.hPutStr handle $ C.unlines ["HTTP/1.0 200 OK",
                                  "Date: Fri, 31 Dec 1999 23:59:59 GMT",
                                  "Content-Type: text/html",
                                  C.append "Content-Length: " (C.pack$show$filesize),
                                  ""]
      (C.hPutStr handle =<< C.hGetContents file)
      -- C.hPutStr handle $ readFile $ tail url
      )
    IO.hClose handle

  -- N.sClose sock


-- httpRequestParser :: Parser BS.ByteString
httpRequestParser = do
  action <- string "GET" <|> string "POST"
  space
  url <- AP.takeWhile (not.isSpace)
  space
  string "HTTP/"
  httpVersion <- rational
  Debug.traceShow httpVersion (return "")
  char '\r'
  options <- manyTill (do
                     optionName <- takeWhile1 (/= ':')
                     string ": "
                     optionValue <- takeWhile1 (/= '\r')
                     char '\r'
                     return (optionName, optionValue))
                      (char '\r')
  Debug.traceShow options (return "")
  return (action, url)




-- End of line
-- eol
