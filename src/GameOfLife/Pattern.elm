module GameOfLife.Pattern exposing (..)

import GameOfLife.Grid as Grid

type alias Pattern = { size: Grid.Size, cells: List Grid.Position }


addPattern : Pattern -> Grid.Grid -> Grid.Grid
addPattern pattern grid =
    List.foldl (\p g -> Grid.set (Grid.offsetPosition 10 10 p) True g) grid pattern.cells


gliderGun : Pattern
gliderGun = 
    { size = Grid.Size 38 11
    , cells = 
            List.map (\p -> Grid.Position (Tuple.first p) (Tuple.second p))
                [ (25, 1)
                , (23, 2), (25, 2)
                , (13, 3), (14, 3), (21, 3), (22, 3), (35, 3), (36, 3)
                , (12, 4), (16, 4), (21, 4), (22, 4), (35, 4), (36, 4)
                , (1, 5), (2, 5), (11, 5), (17, 5), (21, 5), (22, 5)
                , (1, 6), (2, 6), (11, 6), (15, 6), (17, 6), (18, 6), (23, 6), (25, 6)
                , (11, 7), (17, 7), (25, 7)
                , (12, 8), (16, 8)
                , (13, 9), (14, 9)
                ]
    }