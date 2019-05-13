module Grid exposing (..)

import Array
import Browser
import Debug
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Time

type alias Square = (Loc, Tile, Attr)

type Tile
    = Mine
    | NoMine Int

type Attr
    = Covered
    | Uncovered
    | Flagged

type alias Loc = (Int, Int)

type alias Grid = Array.Array (Array.Array Square)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { grid : Grid
    , won : Bool
    , lost : Bool
    , minePossibilities : List Int
    }

initSize = 2

numMines = 2

init : () -> ( Model, Cmd Msg )
init _ =
    update (NewMinePos numMines)
        { grid = generateBlankGrid initSize
        , won = False
        , lost = False
        , minePossibilities = List.range 0 (initSize ^ 2 - 1)
        }

type Msg
    = NewMinePos Int
    | NewMine Int Int

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
                0 -> (model, Cmd.none)
                _ ->
                    let
                        pos = get index model.minePossibilities
                    in
                        update (NewMinePos (num - 1))
                            { grid = addMine (pos + 1) model.grid
                            , won = False
                            , lost = False
                            , minePossibilities = List.filter (\n -> n /= pos) model.minePossibilities
                            }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    div [] [ Html.text (Debug.toString model.grid) ]

resetGrid : Grid
resetGrid =
    Debug.todo "TODO"

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
        if n > total 
        then Debug.todo "intToLoc: n out of range"
        else (row, col)

get : Int -> List a -> a
get n l =
    case List.take 1 (List.drop n l) of
        head::_ -> head
        _ -> Debug.todo "get: bad case"

extractMaybe : Int -> Maybe a -> a
extractMaybe n maybe =
    case maybe of
        Just x -> x
        Nothing -> Debug.todo ("extractMaybe: bad case at " ++ Debug.toString n)

addMine : Int -> Grid -> Grid
addMine n g =
    let
        pos = intToLoc n initSize
        rowNum = Tuple.first pos
        colNum = Tuple.second pos
        row = extractMaybe n (Array.get rowNum g)
        square = extractMaybe n (Array.get colNum row)
        loc =
            case square of
                (loc, tile, attr) -> loc
    in
        Array.set rowNum (Array.set colNum ( loc, Mine, Covered ) row) g

calculateNumbers : Grid -> Grid
calculateNumbers minedGrid =
    Debug.todo "TODO"
