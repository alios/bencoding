{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Lens.Fold
import           Control.Lens.Operators

import           Data.Bencoding
import           Data.Bencoding.Types
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary T.Text where
  arbitrary = T.pack . getPrintableString <$> arbitrary

genBString :: Gen BValue
genBString = BByteString . BS.pack <$> arbitrary

genBInt :: Gen BValue
genBInt = BInt <$> arbitrary

genBList :: Gen BValue
genBList = do
  n <- getSize
  BList . V.fromList <$> resize n arbitrary

genDict :: Gen BValue
genDict = do
  n <- getSize
  BDict <$> resize n arbitrary


instance Arbitrary BValue where
  arbitrary =
    resize 4 $ oneof [genBString, genBInt  , genBList, genDict ]

prop_bvalue_iso :: BValue -> Bool
prop_bvalue_iso a = pure a == preview bvalue (bvalue # a)

prop_bvalue_iso_bin :: BValue -> Bool
prop_bvalue_iso_bin a = pure a == preview _BValue (_BValue # a)

prop_bvalue_iso_bin_strict :: BValue -> Bool
prop_bvalue_iso_bin_strict a = pure a == preview _BValueStrict (_BValueStrict # a)

iso_file :: String -> SpecWith ()
iso_file fp = describe ("test file " ++ fp) $ do
  bs <- runIO $ BL.readFile fp
  it "should de-/encode isometric" $
    let v :: BValue
        v = fromMaybe (error "error") . preview _BValue $ bs
        bs' = _BValue # v
    in bs `shouldBe` bs'


main :: IO ()
main = hspec $ parallel $ do
  describe "BValue" $ parallel $ do
    it "bvalue Prism should be isometric on same value" $
      property prop_bvalue_iso
    it "_BValue Prism should be isometric on same value" $
      property prop_bvalue_iso_bin
    it "_BValueStrict Prism should be isometric on same value" $
      property prop_bvalue_iso_bin_strict
  describe "testing files" $ parallel $ do
      iso_file "test/test.dict"
