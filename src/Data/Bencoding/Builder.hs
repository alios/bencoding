module Data.Bencoding.Builder (buildBValue) where

import           Control.Lens.Indexed
import           Control.Lens.Operators
import           Data.Bencoding.Types
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder
import qualified Data.Map                as Map
import qualified Data.Text               as T
import           Data.Text.Encoding      as T
import qualified Data.Vector             as V


buildBValue :: BValue -> Builder
buildBValue (BString s) =
  let s' = T.encodeUtf8 s
  in mconcat
  [ byteString . T.encodeUtf8 . T.pack . show . BS.length $ s'
  , char7 ':'
  , byteString s'
  ]
buildBValue (BInt i) =
  let i' = byteString . T.encodeUtf8 . T.pack . show $ i
  in mconcat [ char7 'i', i', char7 'e' ]
buildBValue (BList l) =
  let l' = V.toList $ buildBValue <$> l
  in mconcat [char7 'l', mconcat l' ,char7 'e' ]
buildBValue (BDict d) =
  let t = toLazyByteString . buildBValue
      ins k v = Map.insert (t $ bvalue # k) (t v)
  in mconcat [ char7 'd'
             , lazyByteString . mconcat .
               fmap (uncurry mappend) . Map.toList .
               ifoldr ins mempty $ d
             , char7 'e'
             ]
