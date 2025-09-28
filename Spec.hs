import Test.Hspec
import Test.QuickCheck
import VehicleRegister

main :: IO ()
main = hspec $ do
  describe "trim" $ do
    it "removes leading/trailing spaces" $ do
      trim "  hello  " `shouldBe` "hello"
    it "keeps internal spaces" $ do
      trim "  hello  world " `shouldBe` "hello  world"

  describe "lower" $ do
    it "lowercases ASCII letters" $ lower "Hi There!" `shouldBe` "hi there!"

  describe "parseStatus" $ do
    it "parses Registered (case-insensitive)" $ do
      parseStatus "registered" `shouldBe` Just Registered
      parseStatus "REGISTERED" `shouldBe` Just Registered
    it "parses Unregistered (case-insensitive)" $ do
      parseStatus "unregistered" `shouldBe` Just Unregistered
      parseStatus "UnReGiStErEd" `shouldBe` Just Unregistered
    it "rejects unknown terms" $ parseStatus "active" `shouldBe` Nothing

  describe "findVehicle" $ do
    let vs = [ Vehicle "A" 2000 "ABC" Registered
             , Vehicle "B" 2001 "DEF" Unregistered
             , Vehicle "C" 2002 "GHI" Registered
             ]
    it "finds exact reg number" $ do
      vRegNo <$> findVehicle "DEF" vs `shouldBe` Just "DEF"
    it "returns Nothing when absent" $ do
      findVehicle "NOPE" vs `shouldBe` Nothing

  -- QuickCheck-style property: trimming spaces then lowercasing equals lowercasing then trimming
  describe "trim/lower composition" $ do
    it "commutes on spaces and letters" $ property $ \s ->
      let onlyAscii = filter (`elem` (['a'..'z']++['A'..'Z']++" ")) s
      in lower (trim onlyAscii) == trim (lower onlyAscii)
