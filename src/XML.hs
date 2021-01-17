{-# language MagicHash #-}
module XML (module XML, module X) where
import Prelude
import Xeno.DOM as X (Node)
import qualified Xeno.DOM as Xeno (parse,name,attributes,children)
import Xeno.Types as X
import Data.List as X (find)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

class XML a where
  fromXML :: Node -> a
  toXML :: a -> String

instance XML a => XML [a] where
  fromXML = error "fromXML @[a]"
  toXML = unlines . fmap toXML

find# :: (XML a, Foldable t) => ByteString -> t Node -> a
find# n x = fromXML . unjust# $ (`find` x) \ a -> Xeno.name a == n 
  where unjust# = maybe (error "find# Nothing") id
filterName :: XML a => ByteString -> [Node] -> [a]
filterName n x = fmap fromXML $ (`filter` x) \ a -> Xeno.name a == n 

pattern XML :: ByteString ->  Map ByteString ByteString ->  [Node] -> Node
pattern XML {name, attributes , children}
  <- ((\n -> (Xeno.name n
             , Map.fromList $ Xeno.attributes n
             , Xeno.children n)) -> (name,attributes,children))

parse :: XML a => ByteString -> Either XenoException a
parse = fmap fromXML . Xeno.parse

findName :: (XML a, Foldable t) => ByteString -> t Node -> [a]
findName n as = maybe [] (fmap fromXML . children) $ find (\a -> Xeno.name a == n) as

                    --,maxout      = (maybe @String "" (printf "maxout = \"%s\"" . show)                        -> mo)
                    --,actions     = (null' @String "" (printf "\n\t<actions>\n%s\t</actions>" . toXML)         -> as)
                    --,terminators = (null' @String "" (printf "\n\t<terminators>\n%s\t</terminators>" . toXML) -> ts)
