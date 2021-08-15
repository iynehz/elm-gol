module GameOfLife.Grid exposing (..)

import Array
import Array2D
import List


type alias Grid =
    Array2D.Array2D Bool


type alias Size =
    { width : Int, height : Int }


type alias Position =
    { x : Int, y : Int }


initialize : Size -> Grid
initialize size =
    Array2D.repeat size.width size.height False



-- Evolve grid


evolve : Grid -> Grid
evolve grid =
    let
        calc x y cell =
            let
                positions =
                    [ ( x - 1, y - 1 )
                    , ( x - 1, y )
                    , ( x - 1, y + 1 )
                    , ( x, y - 1 )
                    , ( x, y + 1 )
                    , ( x + 1, y - 1 )
                    , ( x + 1, y )
                    , ( x + 1, y + 1 )
                    ]

                n =
                    positions
                        |> List.filter (\p -> Maybe.withDefault False (Array2D.get (Tuple.first p) (Tuple.second p) grid))
                        |> List.length
            in
            (cell && 2 <= n && n <= 3) || (not cell && n == 3)
    in
    Array2D.indexedMap calc grid


clear : Grid -> Grid
clear grid =
    initialize (Size (Array2D.rows grid) (Array2D.columns grid))


toIndexedList : Grid -> List ( Position, Bool )
toIndexedList grid =
    let
        cellsArray =
            Array2D.indexedMap (\x y cell -> ( Position x y, cell )) grid
    in
    Array.toList cellsArray.data
        |> List.foldl (\a b -> b ++ Array.toList a) []


set : Position -> Bool -> Grid -> Grid
set p =
    Array2D.set p.x p.y


get : Position -> Grid -> Maybe Bool
get p grid =
    Array2D.get p.x p.y grid


setPositions : List Position -> Bool -> Grid -> Grid
setPositions positions x grid =
    List.foldl (\p g -> set p x g) grid positions


offsetPosition : Int -> Int -> Position -> Position
offsetPosition ox oy pos =
    Position (pos.x + ox) (pos.y + oy)
