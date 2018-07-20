{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Bencoding.Types
  ( BValue(..), HasBValue(..), _BInt, _BList, getBDict
  ) where

import           Control.Lens.At
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Operators
import           Control.Lens.Prism
import           Data.Map               (Map)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Vector            (Vector)

-- | represents a value in bittorrents bencoding
data BValue
  = BString !Text
  | BInt !Integer
  | BList !(Vector BValue)
  | BDict !(Map Text BValue)
  deriving (Eq, Show)

-- | a type which can be converted to/from a 'BValue'
class HasBValue a where
  -- | a 'Prism' into a value
  bvalue :: Prism' BValue a

instance HasBValue BValue where
  bvalue = iso id id

_BString :: Prism' BValue Text
_BString = prism' BString $ \case
  (BString a) -> pure a
  _           -> Nothing

instance HasBValue Text where
  bvalue = _BString

instance HasBValue String where
  bvalue = _BString . iso T.unpack T.pack

-- | 'Prism' into a 'Integral' 'BValue'
_BInt :: (Integral a) => Prism' BValue a
_BInt = prism' (BInt . toInteger) $ \case
  (BInt a) -> pure (fromInteger a)
  _        -> Nothing

instance HasBValue Int where
  bvalue = _BInt

instance HasBValue Integer where
  bvalue = _BInt

-- | 'Prism' into a list like 'BValue'
_BList :: (Applicative f, Foldable f, Monoid (f BValue)) => Prism' BValue (f BValue)
_BList = prism' (BList . tfV) $ \case
  (BList a) -> pure $ tfV a
  _         -> Nothing
  where tfV :: (Foldable t, Monoid (f a), Applicative f) => t a -> f a
        tfV = foldr (\a b -> pure a `mappend` b ) mempty

_BList' :: (Functor f, Traversable f, HasBValue a) => Prism' (f BValue) (f a)
_BList' = prism' (fmap (bvalue #)) (traverse (preview bvalue))

instance HasBValue a => HasBValue [a] where
  bvalue = _BList . _BList'

instance HasBValue a => HasBValue (Vector a) where
  bvalue = _BList . _BList'


_BDict :: Prism' BValue (Map Text BValue)
_BDict = prism' BDict $ \case
  (BDict m) -> pure m
  _         -> Nothing

_BDict' :: HasBValue a => Prism' BValue (Map Text a)
_BDict' = _BDict . _BList'

instance HasBValue a => HasBValue (Map Text a) where
  bvalue = _BDict'

-- | lookup a value in a bencoded dict
getBDict :: (HasBValue a) => Text -> Getter BValue (Maybe a)
getBDict k = to $ \a -> do
  d <- preview _BDict a
  d ^. at k >>= preview bvalue
