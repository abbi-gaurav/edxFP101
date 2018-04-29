module ArbitraryDefs where
{-
import           System.Random
import           Test.QuickCheck (Gen)

class Arbitrary a where
  arbitrary :: Gen a

elements :: [a] -> Gen a
elements = undefined

choose :: Random a => (a, a) -> Gen a
choose = undefined

oneOf :: [Gen a] -> Gen a
oneOf = undefined

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (x,y)

instance Arbitrary Char where
  arbitrary = elements(['A'..'Z'] ++  ['a'..'z'] ++ "~!@#$%^&*()")

-}
