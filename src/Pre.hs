module Pre (module X) where
import Prelude as X hiding (lookup,words,stripPrefix,map)
import XML as X
import Text.Printf as X
import Data.List.NonEmpty as X (NonEmpty)
import GHC.Exts as X (IsList(fromList,toList))
import Data.ByteString.Char8 as X (ByteString,pack,unpack,words,stripPrefix,map)
import Data.Map as X ((!),lookup,Map,updateLookupWithKey,foldrWithKey,insert,delete,empty,findWithDefault)
import Data.Word as X (Word8)
import Data.Function as X ((&))
import Keyboard.Id as X
import Keyboard.Ix as X
