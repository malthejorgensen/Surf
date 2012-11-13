import System.IO as IO
import Network as N
{-import GHC.IO.Handle-}
import Data.ByteString as BS hiding (putStrLn, hPutStrLn) -- for hGetContents
import qualified Data.ByteString.Char8 as C-- for pack
import Data.Attoparsec.ByteString
import Control.Applicative -- for <|>
{-import qualified Data.ByteString as B-}
import Control.Exception as E
import Control.Concurrent
import System.Posix.Signals

port = 8000


-- http://en.wikipedia.org/wiki/HTTP_persistent_connection
-- "Connection: Keep-Alive" is the default so we cannot use `hGetContents` as 
-- it will block.


main = do
  tid <- myThreadId
  -- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigINT (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigTERM (Catch (throwTo tid UserInterrupt)) Nothing
  -- installHandler sigINT Ignore Nothing -- ignore Ctrl-C

  putStrLn $ "Started listening on port " ++ show port
  sock <- N.listenOn (PortNumber port)
  acceptConn sock `E.catch` interrupt sock
  where interrupt sock exception =
          let err = show (exception :: SomeException) in do
            putStrLn "Closing socket."
            hPutStrLn stderr $ "Some exception caught: " ++ err
            sClose sock

acceptConn sock = do
  (handle, hostname, portnumber) <- accept sock
  putStrLn $ "Got connection from " ++ hostname ++ ":" ++ show portnumber
  text <- BS.hGetLine handle
  print text
  sClose sock



httpRequestParser str = parseOnly (httpRequest <?> "HTTP parse failed") str

httpRequest = (string (C.pack "GET") <?> "uo") <|> (string (C.pack "POST") <?> "erro")


-- End of line
-- eol
