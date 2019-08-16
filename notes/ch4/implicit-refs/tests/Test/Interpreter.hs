module Test.Interpreter (spec) where

import Test.Hspec

import qualified Interpreter

spec :: Spec
spec = do
  describe "example 1" $ do
    it "returns 5" $ do
      run example1 `shouldBe` "5"

  describe "example 2" $ do
    it "returns 10" $ do
      run example2 `shouldBe` "10"

  describe "example 3" $ do
    it "returns 56" $ do
      run example3 `shouldBe` "56"

  describe "example 4" $ do
    it "returns #f" $ do
      run example4 `shouldBe` "#f"

  describe "example 5" $ do
    it "returns #t" $ do
      run example5 `shouldBe` "#t"

  describe "example 6" $ do
    it "returns 2" $ do
      run example6 `shouldBe` "2"

  describe "example 7" $ do
    it "returns 3" $ do
      run example7 `shouldBe` "3"

  describe "example 8" $ do
    it "returns -5" $ do
      run example8 `shouldBe` "-5"

  describe "example 9" $ do
    it "returns 55" $ do
      run example9 `shouldBe` "55"

  describe "example 10" $ do
    it "returns 55" $ do
      run example10 `shouldBe` "55"

  describe "example 11" $ do
    it "returns 12" $ do
      run example11 `shouldBe` "12"

  describe "example 12" $ do
    it "returns 5050" $ do
      run example12 `shouldBe` "5050"

  describe "example 13" $ do
    it "returns 1" $ do
      run example13 `shouldBe` "1"

  describe "example 14" $ do
    it "returns 3" $ do
      run example14 `shouldBe` "3"

  describe "example 15" $ do
    it "returns 1" $ do
      run example15 `shouldBe` "1"

  describe "example 16" $ do
    it "returns -1" $ do
      run example16 `shouldBe` "-1"

  describe "example 17" $ do
    it "returns 0" $ do
      run example17 `shouldBe` "0"

run :: String -> String
run = show . Interpreter.run

-- Examples

example1 :: String
example1 = "5"

example2 :: String
example2 = "x"

example3 :: String
example3 = "-(55, -(x, 11))"

example4 :: String
example4 = "zero?(i)"

example5 :: String
example5 = "zero?(-(i, 1))"

example6 :: String
example6 = "let x = 5 in -(x, 3)"

example7 :: String
example7 = "                        \
  \ let z = 5 in                    \
  \   let x = 3 in                  \
  \     let y = -(x, 1) in          \
  \       let x = 4 in -(z, -(x, y))"

example8 :: String
example8 = "                                  \
  \ let x = 7 in                              \
  \   let y = 2 in                            \
  \     let y = let x = -(x, 1) in -(x, y) in \
  \       -(-(x, 8), y)                       "

example9 :: String
example9 = "let f = proc (x) -(x, 11) in (f (f 77))"

example10 :: String
example10 = "(proc (f) (f (f 77)) proc (x) -(x, 11))"

example11 :: String
example11 = "\
  \ letrec double(x) = if zero?(x) then 0 else -((double -(x, 1)), -2) \
  \ in (double 6)                                                      "

example12 :: String
example12 = "\
  \ letrec sum (n) = if zero?(n) then 0 else -(n, -(0, (sum -(n, 1)))) \
  \ in (sum 100)                                                       "

example13 :: String
example13 = "\
  \ letrec                                             \
  \   even(x) = if zero?(x) then 1 else (odd -(x, 1))  \
  \   odd(x)  = if zero?(x) then 0 else (even -(x, 1)) \
  \ in (odd 13)                                        "

example14 :: String
example14 = "\
  \ begin                   \
  \   1;                    \
  \   i;                    \
  \   let a = 8 in -(a, v)  \
  \ end                     "

example15 :: String
example15 = "\
  \ let x = 0                             \
  \ in letrec                             \
  \      even(dummy) =                    \
  \        if zero?(x)                    \
  \        then 1                         \
  \        else begin                     \
  \               set x = -(x, 1);        \
  \               (odd 888)               \
  \             end                       \
  \      odd(dummy) =                     \
  \        if zero?(x)                    \
  \        then 0                         \
  \        else begin                     \
  \               set x = -(x, 1);        \
  \               (even 888)              \
  \             end                       \
  \    in begin set x = 13; (odd 888) end "

example16 :: String
example16 = "\
  \ let g = let counter = 0                      \
  \         in proc (dummy)                      \
  \              begin                           \
  \                set counter = -(counter, -1); \
  \                counter                       \
  \              end                             \
  \ in let a = (g 11)                            \
  \    in let b = (g 11)                         \
  \       in -(a, b)                             "

example17 :: String
example17 = "\
  \ let g = proc (dummy)                         \
  \           let counter = 0                    \
  \           in begin                           \
  \                set counter = -(counter, -1); \
  \                counter                       \
  \              end                             \
  \ in let a = (g 11)                            \
  \    in let b = (g 11)                         \
  \       in -(a, b)                             "
