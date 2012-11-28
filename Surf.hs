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

-- `withSocketsDo` is for portability with Windows (not really required)
main = N.withSocketsDo $ do
  tid <- Con.myThreadId
  -- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigINT (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigTERM (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigINT Ignore Nothing -- ignore Ctrl-C

  putStrLn $ "Started listening on port " ++ show port
  sock <- N.listenOn (N.PortNumber port)
  acceptConn sock `E.catch` interrupt sock
  where interrupt sock exception =
          let err = show (exception :: E.SomeException) in do
            putStrLn "Closing socket."
            IO.hPutStrLn IO.stderr $ "Some exception caught: " ++ err
            N.sClose sock

acceptConn sock = do
  (handle, hostname, portnumber) <- N.accept sock
  putStrLn $ "Got connection from " ++ hostname ++ ":" ++ show portnumber
  text <- debugGetLine handle
  Done unparsed_text (action, url) <- parseWith (debugGetLine handle) httpRequestParser text

  filepath <- if url == "/" then return "index.html" else return $ C.tail url

  IO.withFile (C.unpack filepath) IO.ReadMode (\file -> do
    filesize <- IO.hFileSize file

    C.hPutStr handle $ C.unlines ["HTTP/1.0 200 OK",
                                "Date: Fri, 31 Dec 1999 23:59:59 GMT",
                                "Content-Type: text/html",
    -- hFileSize is slow?
    -- http://stackoverflow.com/questions/5620332/what-is-the-best-way-to-retrieve-the-size-of-a-file-in-haskell
                                C.append "Content-Length: " (C.pack$show$filesize),
                                ""]
    (C.hPutStr handle =<< C.hGetContents file)
    -- C.hPutStr handle $ readFile $ tail url
    )

  N.sClose sock


peekToFail :: Char -> Parser (Result [a])
peekToFail cExpected = do
  c <- peekChar
  if c == (Just cExpected) || c == Nothing then fail "hehe" -- `mzero` also works
  else return $ Done "" []

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
  options <- many (do
                     peekToFail '\r' -- Die if line starts with \r
                     optionName <- takeWhile1 (/= ':')
                     string ": "
                     optionValue <- takeWhile1 (/= '\r')
                     char '\r'
                     return (optionName, optionValue))
  Debug.traceShow options (return "")
  return (action, url)




-- End of line
-- eol
