import qualified System.IO as IO
import qualified Network as N
{-import qualified GHC.IO.Handle-}

import qualified Data.ByteString as BS hiding (putStrLn, hPutStrLn) -- for hGetContents
import qualified Data.ByteString.Char8 as C -- for pack

import Data.Attoparsec.ByteString
import Control.Applicative -- for <|>

import qualified Control.Exception as E
import qualified Control.Concurrent as Con
import qualified System.Posix.Signals

port = 8000


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
  text <- BS.hGetLine handle
  print text
  C.hPutStr handle $ C.pack $ unlines ["HTTP/1.0 200 OK",
                             "Date: Fri, 31 Dec 1999 23:59:59 GMT",
                             "Content-Type: text/html",
                             "Content-Length: 1354",
                             "",
                             "<html>",
                             "<body>",
                             "<h1>Happy New Millennium!</h1>",
                             "(more file contents)",
                             "  .",
                             "  .",
                             "  .",
                             "</body>",
                             "</html>"]
  N.sClose sock



httpRequestParser str = parseOnly (httpRequest <?> "HTTP parse failed") str

httpRequest = (string (C.pack "GET") <?> "uo") <|> (string (C.pack "POST") <?> "erro")


-- End of line
-- eol
