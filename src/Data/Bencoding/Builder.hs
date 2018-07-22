{-# LANGUAGE OverloadedStrings #-}
module Data.Bencoding.Builder (buildBValue) where


import           Control.Lens.Operators
import           Data.Bencoding.Types
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Map                as Map
import qualified Data.Text               as T
import           Data.Text.Encoding      as T
import qualified Data.Vector             as V


buildBValue :: BValue -> Builder
buildBValue (BByteString bs) =
  mconcat
  [ byteString . T.encodeUtf8 . T.pack . show . BS.length $ bs
  , char7 ':'
  , byteString bs
  ]
buildBValue (BInt i) =
  let i' = byteString . T.encodeUtf8 . T.pack . show $ i
  in mconcat [ char7 'i', i', char7 'e' ]
buildBValue (BList l) =
  let l' = V.toList $ buildBValue <$> l
  in mconcat [char7 'l', mconcat l' ,char7 'e' ]
buildBValue (BDict d) =
  let t = toLazyByteString . buildBValue
      vs = mconcat . fmap (\(k, v) -> (t $ bvalue # k) `mappend` (t v)) .
           Map.toList $ d
  in mconcat [ char7 'd'
             , lazyByteString vs
             , char7 'e'
             ]
