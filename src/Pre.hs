module Pre (module Pre, module X) where
import Prelude as X hiding (lookup,words,stripPrefix,map,id)
import XML as X hiding (name)
import Text.Printf as X
import Data.List.NonEmpty as X (NonEmpty)
import GHC.Exts as X (IsList(fromList,toList))
import Data.ByteString.Char8 as X (ByteString,pack,unpack,words,stripPrefix,map)
import Data.Map as X ((!),lookup,Map,updateLookupWithKey,foldrWithKey,insert,delete,empty,findWithDefault)
import Data.Word as X (Word8)
import Data.Function as X ((&))
import Keyboard.Id as X
import Keyboard.Ix as X

readBS :: Read a => ByteString -> a
readBS = read . unpack

null' :: b -> ([a] -> b) -> [a] -> b
null' z s = \case {[] -> z; as -> s as}
