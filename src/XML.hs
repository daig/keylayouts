{-# language MagicHash #-}
module XML (module XML, module X) where
import Prelude
import Xeno.DOM as X
import Xeno.Types as X
import Data.List as X (find)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

class XML a where
  fromXML :: Node -> a
  toXML :: a -> String

find# n x = fromXML . just# $ (`find` x) \ a -> name a == n 
  where just# = \case Just a -> a; _ -> error "find# Nothing"
find' n x = fmap fromXML $ (`filter` x) \ a -> name a == n 

pattern XML :: ByteString ->  Map ByteString ByteString ->  [Node] -> Node
pattern XML {xml_name, xml_attributes , xml_children}
  <- ((\n -> (name n
             , Map.fromList $ attributes n
             , children n)) -> (xml_name,xml_attributes,xml_children))

readBS :: Read a => ByteString -> a
readBS = read . BS.unpack
