module Grid exposing (..)

import Array

type alias Square = (Tile, Attr)
type Tile = Mine | NoMine Int
type Attr = Covered | Uncovered | Flagged
type alias Grid = Array.Array (Array.Array Square)

resetGrid : Grid
resetGrid =
    Debug.todo "TODO"

generateBlankGrid : Int -> Grid
generateBlankGrid size =
    Array.repeat size (generateBlankRow size)

generateBlankRow : Int -> Array.Array Square
generateBlankRow size =
    Array.repeat size ((NoMine -1), Covered)

populateMines : Int -> Grid -> Grid
populateMines numMines blankGrid =
    Debug.todo "TODO"

calculateNumbers : Grid -> Grid
calculateNumbers minedGrid =
    Debug.todo "TODO"

