-- module Spec (main)  where
-- It is an error to define a module here!! 

import Test.Hspec
import Lib.Lang1  

main :: IO ()
main = hspec $ do
  describe "show" $ do
    it "shows a sample value" $ do
      show sample `shouldBe` "a := 1;\nb := 2 * a;"

    it "shows a multiplication" $ do
      show (Mul  (IntExp 2) (VarExp (Var "a"))) `shouldBe` "2 * a"
