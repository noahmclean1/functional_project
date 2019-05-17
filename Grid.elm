module Grid exposing (..)

import Array
import Browser
import Debug
import List
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Time

----------------------------------------------------------------------------------
-- Type Declarations

type alias Square = (Loc, Tile, Attr)

type Tile
    = Mine
    | NoMine Int

type Attr
    = Covered
    | Uncovered
    | Flagged

type Status
    = Won
    | Lost
    | Neither

type alias Loc = (Int, Int)

type alias Grid = Array.Array (Array.Array Square)

type Msg
    = NewMinePos Int -- Generate a random (valid) index for a mine to be placed in
    | NewMine Int Int -- Place a mine at the index, continue placing [num] mines
    | PlaceNumbers -- Finalize the grid by setting each unmined square with an adjacency number
    | Uncover Loc -- Uncover the clicked square

type alias Model =
    { grid : Grid
    , status : Status
    , minePossibilities : List Int
    }

----------------------------------------------------------------------------------
-- "Global" variables

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

initSize = 24

numMines = 50

----------------------------------------------------------------------------------
-- HTML/front-end functions

-- Create the initial grid, with numMines mines at the specified size
init : () -> ( Model, Cmd Msg )
init _ =
    update (NewMinePos numMines)
        { grid = generateBlankGrid initSize
        , status = Neither
        , minePossibilities = List.range 0 (initSize ^ 2 - 1)
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMinePos num ->
            (model, 
                Random.generate 
                (\n -> NewMine n num)
                (Random.int 0 (List.length model.minePossibilities - 1)))
        NewMine index num ->
            case num of
                0 -> update PlaceNumbers model
                _ ->
                    let
                        pos = get index model.minePossibilities
                    in
                        update (NewMinePos (num - 1))
                            { grid = addMine (pos + 1) model.grid
                            , status = Neither
                            , minePossibilities = List.filter (\n -> n /= pos) model.minePossibilities
                            }
        PlaceNumbers -> 
            let
                newGrid = calculateNumbers model.grid
            in
                ({grid = newGrid
                , status = Neither
                , minePossibilities = []}, Cmd.none)
        Uncover (row, col) ->
            let
                newGrid = uncoverGrid col row model.grid
            in
                ({grid = newGrid
                , status = Neither
                , minePossibilities = []}, Cmd.none)             
                    

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    gridToView model.grid 500

gridToView : Grid -> Int -> Html Msg
gridToView grid size =
    div
    [
    style "height" ((Debug.toString size) ++ "px"),
    style "width" ((Debug.toString size) ++ "px"),
    style "font-size" ((Debug.toString (round (0.8 * toFloat size / toFloat (Array.length grid)))) ++ "px")
    ]
    ((Array.map (rowToView size) grid) |> Array.toList)

rowToView : Int -> Array.Array Square -> Html Msg
rowToView size arr =
    let
        len = Array.length arr
        sqSize = round (toFloat size / toFloat len)
    in
        div
        [
        style "height" ((Debug.toString sqSize) ++ "px"),
        style "width" ((Debug.toString size) ++ "px")
        ]
        ((Array.map (squareToView sqSize) arr) |> Array.toList)

squareToView : Int -> Square -> Html Msg
squareToView size ((row,col), tile, attr) =
    let
         styles =
            [
            style "height" "0",
            style "width" "0",
            style "padding-bottom" ((Debug.toString size) ++ "px"),
            style "padding-right" ((Debug.toString size) ++ "px"),
            style "border" "1px solid black",
            style "display" "inline-block",
            style "left" ((Debug.toString ((col-1) * size + 7)) ++ "px"),
            style "position" "absolute",
            style "text-align" "center"
            ]
    in 
    case attr of
        Covered ->
            div
                (styles ++ 
                    [
                    style "background-color" "blue",
                    onClick (Uncover (row,col))
                    ])
                []
        Flagged ->
            div
                styles
                [img 
                    (styles ++ [style "src" "images/flag.png"])
                    []
                ]
        Uncovered ->
            div
                styles
                (case tile of
                    Mine -> 
                        [img 
                            (styles ++ [style "src" "images/mine.png", style "background-color" "red"])
                            []
                        ]
                    NoMine i ->
                        case i of
                            0 -> []
                            _ ->
                                [div 
                                    [
                                    style "left" "30%", 
                                    style "top" "10%",
                                    style "position" "absolute"
                                    ] 
                                    [text (Debug.toString i)]])

----------------------------------------------------------------------------------
-- Back-end functions

-- 1st step of grid generation: a grid with 0 mines
generateBlankGrid : Int -> Grid
generateBlankGrid size =
    Array.initialize size (\n -> generateBlankRow (n + 1) size)

generateBlankRow : Int -> Int -> Array.Array Square
generateBlankRow rowNum size =
    Array.initialize size (\n -> ((rowNum, n + 1), NoMine -1, Covered ))

intToLoc : Int -> Int -> Loc
intToLoc n size =
    let
        m = n - 1
        total = size ^ 2
        row = floor (toFloat m / toFloat size)
        col = m - (row * size)
    in
        if n > total then 
            Debug.todo "intToLoc: n out of range"
        else
            (row, col)

get : Int -> List a -> a
get n l =
    case List.take 1 (List.drop n l) of
        head::_ -> head
        _ -> Debug.todo "get: bad case"

extractMaybe : Maybe a -> a
extractMaybe maybe =
    case maybe of
        Just x -> x
        Nothing -> Debug.todo "extractMaybe: bad case"

-- 2nd step of initialization (called repeatedly in update): Adds a single mine at a given location
addMine : Int -> Grid -> Grid
addMine n g =
    let
        pos = intToLoc n initSize
        rowNum = Tuple.first pos
        colNum = Tuple.second pos
        row = extractMaybe (Array.get rowNum g)
        square = extractMaybe (Array.get colNum row)
        loc =
            case square of
                (location, tile, attr) -> location
    in
        Array.set rowNum (Array.set colNum ( loc, Mine, Covered ) row) g

-- 3rd and final step of initialization: populate a mined grid with adjacency numbers
calculateNumbers : Grid -> Grid
calculateNumbers minedGrid =
    Array.map (\grid -> calculateRowNum minedGrid grid) minedGrid

calculateRowNum : Grid -> Array.Array Square -> Array.Array Square
calculateRowNum minedGrid row =
    Array.map (\square -> calculateOneNumber minedGrid square) row

calculateOneNumber : Grid -> Square -> Square
calculateOneNumber grid square =
    case square of
        (_, Mine, _) -> square
        ((y,x), NoMine _, attr) ->
            let
                -- A bit messy but still constant number of calls per square (8)
                sum =
                    checkMine (x-1) (y-1) grid
                    + checkMine x (y-1) grid
                    + checkMine (x+1) (y-1) grid
                    + checkMine (x-1) y grid
                    + checkMine (x+1) y grid
                    + checkMine (x-1) (y+1) grid
                    + checkMine x (y+1) grid
                    + checkMine (x+1) (y+1) grid
            in
                ((y,x), NoMine sum, attr)

-- Gives a 1 if there's a mine at the location, 0 if out of bounds or no mine, don't forget the rows/cols are 1-indexed!
checkMine : Int -> Int -> Grid -> Int
checkMine x y grid =
    case (Array.get (y-1) grid) of
        Nothing -> 0
        Just row ->
            case (Array.get (x-1) row) of
                Nothing -> 0
                Just (_, Mine, _) -> 1
                _ -> 0

-- Recursive uncovering function that makes minesweeper actually play well
uncoverGrid : Int -> Int -> Grid -> Grid
uncoverGrid x y grid =
    case (Array.get (y-1) grid) of
        Nothing -> grid
        Just row -> 
            case (Array.get (x-1) row) of
                Nothing -> grid
                Just (_, _, Uncovered) -> grid
                Just (loc, NoMine number, attr) ->
                    let
                        newGrid = Array.set (y-1) (Array.set (x-1) (loc, NoMine number, Uncovered) row) grid
                    in
                        if number /= 0 then
                            newGrid
                        else
                            uncoverGrid (x-1) (y-1) newGrid 
                                |> uncoverGrid x (y-1) 
                                |> uncoverGrid (x+1) (y-1) 
                                |> uncoverGrid (x-1) y 
                                |> uncoverGrid (x+1) y 
                                |> uncoverGrid (x-1) (y+1) 
                                |> uncoverGrid x (y+1) 
                                |> uncoverGrid (x+1) (y+1)
                _ -> Debug.todo "Error in uncoverGrid: recursed onto a mine TODO FIX"

            