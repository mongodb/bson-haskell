-- import Test.HUnit hiding (Test)
import Test.Hspec.Monadic (Specs, describe, it, hspecX)
import Test.Hspec.HUnit()
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck
import Data.Bson
import FileLocation (debug)

main :: IO ()
main = hspecX specs

instance Arbitrary ObjectId where
  arbitrary = do
      t <- arbitrary
      p <- arbitrary
      m <- arbitrary
      i <- arbitrary
      return $ Oid t $ composite m p i

specs :: Specs
specs = do
  describe "ObjectId" $ do
    prop "read <-> show" $ \objId ->
      (debug . read . show . debug) objId == (objId :: ObjectId)

  describe "roundTo" $ do
    prop "round" $ \d ->
      let r =  roundTo (1 / 10) (d :: Double)
      in  r == roundTo (1 / 10) r
