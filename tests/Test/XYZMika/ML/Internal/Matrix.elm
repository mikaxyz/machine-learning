module Test.XYZMika.ML.Internal.Matrix exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import XYZMika.ML.Internal.Matrix as Matrix


suite : Test
suite =
    Test.concat
        [ test "creates from list" <|
            \_ ->
                let
                    result =
                        expected
                            |> Matrix.fromList

                    expected =
                        [ [ 1, 2, 3 ]
                        , [ 4, 5, 6 ]
                        ]
                in
                Expect.all
                    [ \m -> Expect.equalLists (Matrix.toList m) expected
                    , \m -> Expect.equal (Matrix.dimensions m) { rows = 2, cols = 3 }
                    ]
                    result
        , test "can do addition" <|
            \_ ->
                let
                    ( m1, m2 ) =
                        ( Matrix.fromList [ [ 4, 3, 2, 1 ] ]
                        , Matrix.fromList [ [ 4, 3, 2, 1 ] ]
                        )

                    result =
                        Matrix.add m1 m2
                            |> Matrix.toList

                    expected =
                        [ [ 8, 6, 4, 2 ] ]
                in
                Expect.equal expected result
        , test "can do subtraction" <|
            \_ ->
                let
                    ( m1, m2 ) =
                        ( Matrix.fromList [ [ 8, 6, 4, 2 ] ]
                        , Matrix.fromList [ [ 4, 3, 2, 1 ] ]
                        )

                    result =
                        m1
                            |> Matrix.sub m2
                            |> Matrix.toList

                    expected =
                        [ [ 4, 3, 2, 1 ] ]
                in
                Expect.equal expected result
        , test "can be transposed" <|
            \_ ->
                let
                    result =
                        [ [ 1, 2, 3 ]
                        , [ 4, 5, 6 ]
                        ]
                            |> Matrix.fromList
                            |> Matrix.transpose
                            |> Matrix.toList

                    expected =
                        [ [ 1, 4 ]
                        , [ 2, 5 ]
                        , [ 3, 6 ]
                        ]
                in
                Expect.equal expected result
        , test "can be scaled" <|
            \_ ->
                let
                    result =
                        [ [ 1, 2, 3 ]
                        , [ 4, 5, 6 ]
                        ]
                            |> Matrix.fromList
                            |> Matrix.scale 10
                            |> Matrix.toList

                    expected =
                        [ [ 10, 20, 30 ]
                        , [ 40, 50, 60 ]
                        ]
                in
                Expect.equal expected result
        , test "can be multiplied" <|
            \_ ->
                let
                    m1 =
                        [ [ 1, 2, 3 ]
                        , [ 4, 5, 6 ]
                        ]
                            |> Matrix.fromList

                    m2 =
                        [ [ 7, 8 ]
                        , [ 9, 10 ]
                        , [ 11, 12 ]
                        ]
                            |> Matrix.fromList

                    result =
                        Matrix.mul m1 m2
                            |> Matrix.toList

                    expected =
                        [ [ 58, 64 ]
                        , [ 139, 154 ]
                        ]
                in
                Expect.equal expected result

        --, test "does not change shape" <|
        --    \_ ->
        --        let
        --            result =
        --                [ [ 1, 2, 3, 4 ]
        --                , [ 1, 2, 3, 4 ]
        --                ]
        --                    |> Matrix.fromList
        --                    |> Matrix.toList
        --
        --            expected =
        --                [ [ 1, 2, 3, 4, 0, 0 ]
        --                , [ 1, 2, 3, 4, 0, 0 ]
        --                ]
        --        in
        --        Expect.equal expected result
        ]
