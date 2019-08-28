module Test.Interpreter (spec) where

import Test.Hspec

import qualified XLet.Interpreter as I

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
    it "returns False" $ do
      let input = "zero?(i)"

      run input `shouldBe` "False"

  describe "example 4" $ do
    it "returns True" $ do
      let input = "zero?(-(i, 1))"

      run input `shouldBe` "True"

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
    it "returns 15" $ do
      let input = "add(add(1, 2), add(add(3, 4), 5))"

      run input `shouldBe` "15"

  describe "example 13" $ do
    it "returns 6" $ do
      let input = "mul(2, mul(mul(1, -(1, 0)), 3))"

      run input `shouldBe` "6"

  describe "example 14" $ do
    it "returns 10" $ do
      let input = "div(div(100, 5), 2)"

      run input `shouldBe` "10"

  describe "example 15" $ do
    it "returns True" $ do
      let input = "equal?(-(10, 2), mul(2, 4))"

      run input `shouldBe` "True"

  describe "example 16" $ do
    it "returns False" $ do
      let input = "greater?(add(10, 2), mul(5, 4))"

      run input `shouldBe` "False"

  describe "example 17" $ do
    it "returns True" $ do
      let input = "less?(mul(1, mul(2, 0)), -(2, 1))"

      run input `shouldBe` "True"

  describe "example 18" $ do
    it "returns (4 (3))" $ do
      let input = "                     \
        \ let x = 4                     \
        \ in cons(x,                    \
        \         cons(cons(-(x, 1),    \
        \                   emptylist), \
        \              emptylist))      "

      run input `shouldBe` "(4 (3))"

  describe "example 19" $ do
    it "returns (1 . 2)" $ do
      let input = "cons(1, 2)"

      run input `shouldBe` "(1 . 2)"

  describe "example 20" $ do
    it "returns (1 2 . 3)" $ do
      let input = "cons(1, cons(2, 3))"

      run input `shouldBe` "(1 2 . 3)"

  describe "example 21" $ do
    it "returns (1 (2 . 3) 4 . 5)" $ do
      let input = "cons(1, cons(cons(2, 3), cons(4, 5)))"

      run input `shouldBe` "(1 (2 . 3) 4 . 5)"

  describe "example 22" $ do
    it "returns ()" $ do
      let input = "emptylist"

      run input `shouldBe` "()"

  describe "example 23" $ do
    it "returns True" $ do
      let input = "null?(emptylist)"

      run input `shouldBe` "True"

  describe "example 24" $ do
    it "returns False" $ do
      let input = "null?(cons(1, emptylist))"

      run input `shouldBe` "False"

  describe "example 25" $ do
    it "returns 3" $ do
      let input = "             \
        \ let x = cons(1, 2)    \
        \ in add(car(x), cdr(x))"

      run input `shouldBe` "3"

run :: String -> String
run = show . I.run
