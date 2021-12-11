module XYZMika.ML.Internal.Matrix exposing
    ( Matrix
    , add
    , create
    , dimensions
    , fromList
    , hadamard
    , indexedMap
    , map
    , mul
    , scale
    , sub
    , toList
    , transpose
    )

import Array


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
            , data = listTranspose m.data
        }



-- From List.Extra copy pasta


{-| Transpose rows and columns of the list of lists.

    transpose [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
    --> [ [ 1, 4 ], [ 2, 5 ], [ 3, 6 ] ]

    transpose [ [ 10, 11 ], [ 20, 40 ], [ 30, 31, 32, 400 ] ]
    --> [ [ 10, 20, 30 ], [ 11, 40, 31 ] ]

-}
listTranspose : List (List a) -> List (List a)
listTranspose listOfLists =
    List.foldr (List.map2 (::)) (List.repeat (listRowsLength listOfLists) []) listOfLists


listRowsLength : List (List a) -> Int
listRowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x
