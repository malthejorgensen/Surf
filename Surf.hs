import Network
{-import GHC.IO.Handle-}
import Data.ByteString -- for hGetContents
import qualified Data.ByteString.Char8 as C -- for pack
import Data.Attoparsec.ByteString
import Control.Applicative -- for <|>
{-import qualified Data.ByteString as B-}

port = 8000

main = do
  print $ "Started listening on port " ++ show port
  sock <- listenOn (PortNumber port)
  (handle, hostname, portnumber) <- accept sock
  text <- hGetContents handle
  print text
  sClose sock



httpRequestParser str = parseOnly (httpRequest <?> "HTTP parse failed") str

httpRequest = (string (C.pack "GET") <?> "uo") <|> (string (C.pack "POST") <?> "erro")


-- End of line
-- eol
