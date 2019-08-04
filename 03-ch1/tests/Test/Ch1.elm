module Test.Ch1 exposing
  ( inS
  , listLength
  , nthElement
  , removeFirst
  , occursFree
  , subst

  , remove
  )


import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


import Ch1 exposing (LcExp(..), SList(..), SExp(..))


inS : Test
inS =
  describe "inS"
    [ fuzz Fuzz.int "is True for non-negative multiples of 3 and False otherwise" <|
        \n ->
          Ch1.inS n
            |> Expect.equal (n >= 0 && modBy 3 n == 0)
    ]


listLength : Test
listLength =
  describe "listLength"
    [ fuzz (Fuzz.list Fuzz.unit) "computes the length of any list" <|
        \list ->
          Ch1.listLength list
            |> Expect.equal (List.length list)
    ]


nthElement : Test
nthElement =
  describe "nthElement"
    [ describe "empty list"
        [ fuzz Fuzz.int "can never find the nth element of the empty list" <|
            \n ->
              Ch1.nthElement [] n
                |> Expect.err
        ]
    , describe "non-empty list"
        [ test "it returns the 1st element" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 0
                |> Expect.equal (Ok 1)
        , test "it returns the 2nd element" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 1
                |> Expect.equal (Ok 2)
        , test "it returns the 5th element" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 4
                |> Expect.equal (Ok 5)
        , test "it returns an error given a negative index" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] -1
                |> Expect.equal (Err "The index must be non-negative: -1")
        , test "it returns an error given an out of bounds index" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 5
                |> Expect.equal (Err "List too short by 1 element")
        ]
    ]


removeFirst : Test
removeFirst =
  describe "removeFirst"
    [ test "it removes the only a" <|
        \_ ->
          Ch1.removeFirst "a" ["a", "b", "c"]
            |> Expect.equal ["b", "c"]
    , test "it returns the original list" <|
        \_ ->
          Ch1.removeFirst "b" ["e", "f", "g"]
            |> Expect.equal ["e", "f", "g"]
    , test "it removes the first a4" <|
        \_ ->
          Ch1.removeFirst "a4" ["c1", "a4", "c1", "a4"]
            |> Expect.equal ["c1", "c1", "a4"]
    , test "it returns the empty list" <|
        \_ ->
          Ch1.removeFirst "x" []
            |> Expect.equal []
    ]


occursFree : Test
occursFree =
  describe "occursFree"
    [ test "it returns True for x in x" <|
        \_ ->
          Ch1.occursFree "x" (Id "x")
            |> Expect.equal True
    , test "it returns False for x in y" <|
        \_ ->
          Ch1.occursFree "x" (Id "y")
            |> Expect.equal False
    , test "it returns False for x in λx.(x y)" <|
        \_ ->
          Ch1.occursFree "x" (Lambda "x" (App (Id "x") (Id "y")))
            |> Expect.equal False
    , test "it returns True for x in λy.(x y)" <|
        \_ ->
          Ch1.occursFree "x" (Lambda "y" (App (Id "x") (Id "y")))
            |> Expect.equal True
    , test "it returns True for x in (λx.x (x y))" <|
        \_ ->
          Ch1.occursFree "x" (App (Lambda "x" (Id "x")) (App (Id "x") (Id "y")))
            |> Expect.equal True
    , test "it returns True for x in λy.λz.(x (y z))" <|
        \_ ->
          Ch1.occursFree "x" (Lambda "y" (Lambda "z" (App (Id "x") (App (Id "y") (Id "z")))))
            |> Expect.equal True
    ]


subst : Test
subst =
  describe "subst"
    [ test "it replaces b's with a's" <|
        \_ ->
          let
            -- ((b c) (b () d))
            input =
              (Cons
                (SList (Cons (Symbol "b") (Cons (Symbol "c") Empty)))
                (Cons
                  (SList (Cons (Symbol "b") (Cons (SList Empty) (Cons (Symbol "d") Empty))))
                  Empty))

            -- ((a c) (a () d))
            output =
              (Cons
                (SList (Cons (Symbol "a") (Cons (Symbol "c") Empty)))
                (Cons
                  (SList (Cons (Symbol "a") (Cons (SList Empty) (Cons (Symbol "d") Empty))))
                  Empty))
          in
            Ch1.subst "a" "b" input
              |> Expect.equal output
    ]


-- TEST SOLUTIONS TO EXERCISES


remove : Test
remove =
  describe "remove"
    [ test "it removes the only a" <|
        \_ ->
          Ch1.remove "a" ["a", "b", "c"]
            |> Expect.equal ["b", "c"]
    , test "it returns the original list" <|
        \_ ->
          Ch1.remove "b" ["e", "f", "g"]
            |> Expect.equal ["e", "f", "g"]
    , test "it removes all a4" <|
        \_ ->
          Ch1.remove "a4" ["c1", "a4", "c1", "a4"]
            |> Expect.equal ["c1", "c1"]
    , test "it returns the empty list" <|
        \_ ->
          Ch1.remove "x" []
            |> Expect.equal []
    ]