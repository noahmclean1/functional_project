module Grid exposing (..)

import Array

type Square = Tile Attr
type Tile = Mine | NoMine Int
type Attr = Covered | Uncovered | Flagged
type alias Grid = Array.Array (Array.Array Square)

resetGrid : Grid
resetGrid =
    Debug.todo "TODO"

generateBlankGrid : Int -> Grid
generateBlankGrid size =
    Debug.todo "TODO"

populateMines : Int -> Grid -> Grid
populateMines numMines blankGrid =
    Debug.todo "TODO"

calculateNumbers : Grid -> Grid
calculateNumbers minedGrid =
    Debug.todo "TODO"

