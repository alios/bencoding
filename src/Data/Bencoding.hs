module Data.Bencoding ( BValue, HasBValue(..), _BValue, _BValueStrict ) where

import           Control.Lens.Fold
import           Control.Lens.Iso
import           Control.Lens.Operators
import           Control.Lens.Prism
import           Data.Attoparsec.ByteString.Lazy
import           Data.Bencoding.Builder
import           Data.Bencoding.Parser
import           Data.Bencoding.Types
import qualified Data.ByteString                 as BS
import           Data.ByteString.Builder
import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy            as BL

_BValue :: HasBValue a => Prism' ByteString a
_BValue = prism' f t
  where f = toLazyByteString . buildBValue . (bvalue #)
        t = (preview bvalue =<<) . maybeResult . parse parseBValue

_BValueStrict :: HasBValue a => Prism' BS.ByteString a
_BValueStrict = iso BL.fromStrict BL.toStrict . _BValue
