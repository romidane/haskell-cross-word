import           Data
import           Lib
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "formatGrid" $ do
      it "concatinates every line with a new line" $ do
        formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"
    describe "findWord" $ do
      it "finds words that exist on a Grid" $ do
        findWord grid "HASKELL" `shouldBe` Just "HASKELL"
        findWord grid "PERL" `shouldBe` Just "PERL"
      it "doesn't find a word that's not on a Grid" $ do
        findWord grid "UKNOWN" `shouldBe` Nothing
    describe "findWords" $ do
      it "finds all the words that exist on a grid" $ do
        findWords grid languages `shouldBe` languages

      it "doesn't find words that don't exist on a grid" $ do
        findWords grid ["UNKNOWN", "COBALER"] `shouldBe` []
