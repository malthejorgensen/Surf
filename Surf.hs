import Network
{-import GHC.IO.Handle-}
import Data.ByteString -- for hGetContents
import qualified Data.ByteString.Char8 as C -- for pack
import Data.Attoparsec.ByteString
import Control.Applicative -- for <|>
{-import qualified Data.ByteString as B-}

port = 8000

main = (print $ "Started listening on port " ++ show port)
     >> listenOn (PortNumber port)
     >>= accept
     >>= \(handle, hostname, portnumber) -> hGetContents handle
     >>= print
     -- >> sClose (PortNumber port)



httpRequestParser str = parseOnly (httpRequest <?> "HTTP parse failed") str

httpRequest = (string (C.pack "GET") <?> "uo") <|> (string (C.pack "POST") <?> "erro")


-- End of line
-- eol
