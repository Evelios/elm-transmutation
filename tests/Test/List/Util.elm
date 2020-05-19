module Test.List.Util exposing (..)

import Expect
import List.Util
import Test exposing (Test, describe, test)


linspace : Test
linspace =
    describe "Linearly space"
        [ test "one number from 1 to 1" <|
            \_ ->
                List.Util.linspace 1 1 1
                    |> Expect.equal [ 1.0 ]
        , test "two numbers from 1 to 2" <|
            \_ ->
                List.Util.linspace 2 1 2
                    |> Expect.equal [ 1.0, 2.0 ]
        , test "three numbers from 0 to 2" <|
            \_ ->
                List.Util.linspace 3 0 2
                    |> Expect.equal [ 0.0, 1.0, 2.0 ]
        , test "three numbers from 2 to 0" <|
            \_ ->
                List.Util.linspace 3 2 0
                    |> Expect.equal [ 0.0, 1.0, 2.0 ]
        , test "five numbers from 0 to 1" <|
            \_ ->
                List.Util.linspace 6 0 1
                    |> Expect.equal [ 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 ]
        ]
