module ConfigSpec (spec) where

import           Config
import           Test.Hspec

main :: IO ()
main = hspec $ spec

completeAppDefault :: CompleteAppConfig
completeAppDefault = (mempty :: CompleteAppConfig)

partialAppDefault :: PartialAppConfig
partialAppDefault = (mempty :: PartialAppConfig)

spec :: Spec
spec = do
  describe "defaults" $ do
    it "has localhost as the default host" $
      defaultHost `shouldBe` "localhost"

    it "has 5000 as default port" $
      defaultPort `shouldBe` 5000
