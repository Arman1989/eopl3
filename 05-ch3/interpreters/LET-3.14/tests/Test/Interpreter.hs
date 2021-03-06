module Test.Interpreter (spec) where

import Test.Hspec

import qualified Let.Interpreter as I

spec :: Spec
spec = do
  describe "example 1" $ do
    it "returns 5" $ do
      let input = "5"

      run input `shouldBe` "5"

  describe "example 2" $ do
    it "returns 10" $ do
      let input = "x"

      run input `shouldBe` "10"

  describe "example 3" $ do
    it "returns 10" $ do
      let input = "if zero?(i) then v else x"

      run input `shouldBe` "10"

  describe "example 4" $ do
    it "returns 5" $ do
      let input = "if zero?(-(i, 1)) then v else x"

      run input `shouldBe` "5"

  describe "example 5" $ do
    it "returns 56" $ do
      let input = "-(55, -(x, 11))"

      run input `shouldBe` "56"

  describe "example 6" $ do
    it "returns 3" $ do
      let input = "-(-(x, 3), -(v, i))"

      run input `shouldBe` "3"

  describe "example 7" $ do
    it "returns 18" $ do
      let input = "                                          \
        \ let x = 33                                         \
        \ in let y = 22                                      \
        \    in if zero?(-(x, 11)) then -(y, 2) else -(y, 4) "

      run input `shouldBe` "18"

  describe "example 8" $ do
    it "returns 2" $ do
      let input = "let x = 5 in -(x, 3)"

      run input `shouldBe` "2"

  describe "example 9" $ do
    it "returns 3" $ do
      let input = "                       \
        \ let z = 5 in                    \
        \   let x = 3 in                  \
        \     let y = -(x, 1) in          \
        \       let x = 4 in -(z, -(x, y))"

      run input `shouldBe` "3"

  describe "example 10" $ do
    it "returns -5" $ do
      let input = "                                 \
        \ let x = 7 in                              \
        \   let y = 2 in                            \
        \     let y = let x = -(x, 1) in -(x, y) in \
        \       -(-(x, 8), y)                       "

      run input `shouldBe` "-5"

  describe "example 11" $ do
    it "returns 14" $ do
      let input = "minus(-(minus(5), 9))"

      run input `shouldBe` "14"

  describe "example 12" $ do
    it "returns 6" $ do
      let input = "add(1, add(2, 3))"

      run input `shouldBe` "6"

  describe "example 13" $ do
    it "returns 60" $ do
      let input = "mul(mul(2, 3), 10)"

      run input `shouldBe` "60"

  describe "example 14" $ do
    it "returns 4" $ do
      let input = "div(div(24, 3), div(4, 2))"

      run input `shouldBe` "4"

  describe "example 15" $ do
    it "returns 6" $ do
      let input = "if equal?(add(1, add(2, 3)), add(add(1, 2), 3)) then 6 else 0"

      run input `shouldBe` "6"

  describe "example 16" $ do
    it "returns 10" $ do
      let input = "if greater?(mul(2, 5), add(2, 5)) then 10 else 7"

      run input `shouldBe` "10"

  describe "example 17" $ do
    it "returns 2" $ do
      let input = "if less?(div(6, 3), -(6, 3)) then 2 else 3"

      run input `shouldBe` "2"

run :: String -> String
run = show . I.run
