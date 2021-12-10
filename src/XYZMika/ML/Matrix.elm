module XYZMika.ML.Matrix exposing (..)

import Array
import List.Extra


type Matrix
    = Matrix
        { rows : Int
        , cols : Int
        , data : List (List Float)
        }


create : ( Int, Int ) -> Matrix
create ( rows, columns ) =
    Matrix
        { rows = rows
        , cols = columns
        , data = List.repeat rows (List.repeat columns 0)
        }


dimensions : Matrix -> { rows : Int, cols : Int }
dimensions (Matrix m) =
    { rows = m.rows, cols = m.cols }


fromList1 : ( Int, Int ) -> List (List Float) -> Matrix
fromList1 ( rows, columns ) data =
    let
        data_ =
            data
                |> Array.fromList
                |> Array.map Array.fromList
    in
    create ( rows, columns )
        |> indexedMap
            (\rowIndex colIndex existing ->
                Array.get rowIndex data_
                    |> Maybe.andThen (Array.get colIndex)
                    |> Maybe.withDefault existing
            )


fromList : List (List Float) -> Matrix
fromList data =
    let
        data_ =
            data
                |> Array.fromList
                |> Array.map Array.fromList

        rows =
            List.length data

        cols =
            data |> List.foldl (\x acc -> max acc (List.length x)) 0
    in
    create ( rows, cols )
        |> indexedMap
            (\rowIndex colIndex existing ->
                Array.get rowIndex data_
                    |> Maybe.andThen (Array.get colIndex)
                    |> Maybe.withDefault existing
            )


indexedMap : (Int -> Int -> Float -> Float) -> Matrix -> Matrix
indexedMap f (Matrix m) =
    Matrix
        { m
            | data =
                List.indexedMap
                    (\rowIndex row ->
                        row
                            |> List.indexedMap
                                (\colIndex x ->
                                    f rowIndex colIndex x
                                )
                    )
                    m.data
        }


map : (Float -> Float) -> Matrix -> Matrix
map f (Matrix m) =
    Matrix { m | data = List.map (List.map f) m.data }


toList : Matrix -> List (List Float)
toList (Matrix m) =
    m.data


add : Matrix -> Matrix -> Matrix
add (Matrix m1) (Matrix m2) =
    let
        addRow : List Float -> List Float -> List Float
        addRow r1 r2 =
            List.map2
                (\v1 v2 -> v1 + v2)
                r1
                r2

        data =
            List.map2
                addRow
                m2.data
                m1.data
    in
    Matrix { m1 | data = data }


sub : Matrix -> Matrix -> Matrix
sub (Matrix m1) (Matrix m2) =
    let
        addRow : List Float -> List Float -> List Float
        addRow r1 r2 =
            List.map2
                (\v1 v2 -> v1 - v2)
                r1
                r2

        data =
            List.map2
                addRow
                m2.data
                m1.data
    in
    Matrix { m1 | data = data }


mul : Matrix -> Matrix -> Matrix
mul (Matrix m1) matrix2 =
    let
        dot : List Float -> List Float -> Float
        dot l1 l2 =
            List.map2 (*) l1 l2 |> List.sum

        (Matrix m2) =
            transpose matrix2

        data =
            List.map
                (\a -> m2.data |> List.map (dot a))
                m1.data

        rows =
            List.length data

        cols =
            List.head data
                |> Maybe.map List.length
                |> Maybe.withDefault 0
    in
    Matrix { cols = cols, rows = rows, data = data }


hadamard : Matrix -> Matrix -> Matrix
hadamard (Matrix m1) (Matrix m2) =
    let
        rowHelp row1 row2 =
            List.map2 (*) row1 row2

        data =
            List.map2
                rowHelp
                m1.data
                m2.data
    in
    Matrix { m1 | data = data }


scale : Float -> Matrix -> Matrix
scale x (Matrix m) =
    Matrix { m | data = m.data |> List.map (List.map (\v -> v * x)) }


transpose : Matrix -> Matrix
transpose (Matrix m) =
    Matrix
        { m
            | rows = m.cols
            , cols = m.rows
            , data = List.Extra.transpose m.data
        }
