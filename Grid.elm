module Grid exposing (..)

import Array
import Browser
import Browser.Dom exposing (getViewport, Viewport)
import Browser.Events exposing (..)
import Debug
import List
import Json.Decode as Json
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Time
import Task

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
    | Playing
    | NotStarted

type alias Loc = (Int, Int)

type alias Grid = Array.Array (Array.Array Square)

type Msg
    = NewMinePos Int (Maybe Int) Loc -- Generate a random (valid) index for a mine to be placed in
    | NewMine Int Int Loc -- Place a mine at the index, continue placing [num] mines
    | PlaceNumbers Loc -- Finalize the grid by setting each unmined square with an adjacency number
    | Uncover Loc -- Uncover the clicked square
    | Flag Loc -- Flag the clicked square as a mine
    | SetDifficulty Int Int -- Set the difficulty (aka set the size of the board and number of mines)
    | Resize Int Int
    | Tick Int

type alias Model =
    { grid : Grid
    , status : Status
    , minePossibilities : List Int
    , size : Int
    , numMines : Int
    , numFlags : Int
    , pixelSize : Int
    , timeTick : Int
    }

----------------------------------------------------------------------------------
-- "Global" variables

initSize = 8

initNumMines = 10

pixelSize = 500

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

----------------------------------------------------------------------------------
-- HTML/front-end functions

-- Create the initial grid, with numMines mines at the specified size
init : () -> ( Model, Cmd Msg )
init () =
    initDifficulty initSize initNumMines pixelSize

initDifficulty : Int -> Int -> Int -> ( Model, Cmd Msg )
initDifficulty size numMines pSize =
        ({ grid = generateBlankGrid size
        , status = NotStarted
        , minePossibilities = List.range 0 (size ^ 2 - 1)
        , size = size
        , numMines = numMines
        , numFlags = 0
        , pixelSize = pSize
        , timeTick = 0
        }, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMinePos num toRemove loc  ->
            case toRemove of
                Nothing ->
                    (model, 
                        Random.generate 
                        (\n -> NewMine n num loc)
                        (Random.int 0 (List.length model.minePossibilities - 1)))
                Just clicked ->
                    let
                        newMinePossibilities = List.filter (\n -> n /= clicked) (model.minePossibilities)
                    in
                    ({grid = model.grid
                    , status = model.status
                    , minePossibilities = newMinePossibilities
                    , size = model.size
                    , numMines = model.numMines
                    , numFlags = model.numFlags
                    , pixelSize = model.pixelSize
                    , timeTick = model.timeTick},
                      Random.generate 
                        (\n -> NewMine n num loc)
                        (Random.int 0 (List.length newMinePossibilities - 1)))
        NewMine index num loc ->
            case num of
                0 -> update (PlaceNumbers loc) model
                _ ->
                    let
                        pos = get index model.minePossibilities
                    in
                        update (NewMinePos (num - 1) Nothing loc)
                            { grid = addMine (pos + 1) model.grid model.size
                            , status = NotStarted
                            , minePossibilities = List.filter (\n -> n /= pos) model.minePossibilities
                            , size = model.size
                            , numMines = model.numMines
                            , numFlags = model.numFlags
                            , pixelSize = model.pixelSize
                            , timeTick = model.timeTick
                            }
        PlaceNumbers loc -> 
            let
                newGrid = calculateNumbers model.grid
            in
                update 
                (Uncover loc) 
                {grid = newGrid
                , status = Playing
                , minePossibilities = []
                , size = model.size
                , numMines = model.numMines
                , numFlags = model.numFlags
                , pixelSize = model.pixelSize
                , timeTick = model.timeTick}
        Uncover (row, col) ->
            case model.status of
                Playing ->
                    let
                        (newGrid, newStatus) = clicking col row (model.grid, model.status)
                        newStatus_ = 
                            if 
                                checkWon ({grid = newGrid
                                        , status = newStatus
                                        , minePossibilities = []
                                        , size = model.size
                                        , numMines = model.numMines
                                        , numFlags = model.numFlags
                                        , pixelSize = model.pixelSize
                                        , timeTick = model.timeTick})
                                && newStatus /= Lost 
                            then Won 
                            else newStatus
                    in
                        ({grid = newGrid
                        , status = newStatus_
                        , minePossibilities = []
                        , size = model.size
                        , numMines = model.numMines
                        , numFlags = model.numFlags
                        , pixelSize = model.pixelSize
                        , timeTick = model.timeTick}, Cmd.none)
                NotStarted ->
                    update (NewMinePos model.numMines (Just ((locToInt (row-1,col-1) model.size) - 1)) (row,col)) model
                _ ->
                    (model, Cmd.none)
        Flag (row,col) ->
            if model.status == Playing
            then
                let
                    (newGrid, flagsAdded) = flagInGrid col row model.grid
                    newStatus = 
                            if 
                                checkWon ({grid = newGrid
                                        , status = model.status
                                        , minePossibilities = []
                                        , size = model.size
                                        , numMines = model.numMines
                                        , numFlags = model.numFlags + flagsAdded
                                        , pixelSize = model.pixelSize
                                        , timeTick = model.timeTick})
                            then Won 
                            else Playing
                in
                    ({grid= newGrid
                    , status = newStatus
                    , minePossibilities = []
                    , size = model.size
                    , numMines = model.numMines
                    , numFlags = model.numFlags + flagsAdded
                    , pixelSize = model.pixelSize
                    , timeTick = model.timeTick}, Cmd.none)                        
            else
                (model, Cmd.none)
        SetDifficulty size numMines ->
            initDifficulty size numMines model.pixelSize
        Resize width height ->
            ({grid = model.grid
            , status = model.status
            , minePossibilities = model.minePossibilities
            , size = model.size
            , numMines = model.numMines
            , numFlags = model.numFlags
            , pixelSize = 
                if toFloat width / toFloat height < (860/634)
                then round (0.55 * toFloat width)
                else Basics.max 100 (round (0.75 * toFloat (Basics.min width height)))
            , timeTick = model.timeTick}, 
            Cmd.none)
        Tick num ->
            let
                -- Don't start the clock unless the game has begun or isn't over
                newNum =
                    case model.status of
                        Playing -> num+1
                        NotStarted -> 0
                        _ -> num
            in
                ({grid = model.grid
                , status = model.status
                , minePossibilities = model.minePossibilities
                , size = model.size
                , numMines = model.numMines
                , numFlags = model.numFlags
                , pixelSize = model.pixelSize
                , timeTick = newNum}, Cmd.none)   
                    

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        onResize (\width height -> Resize width height)
        , Time.every 1000 (\_ -> Tick model.timeTick)
    ]

view : Model -> Html Msg
view model =
    div []
    [ statusText model model.pixelSize
    , buttons model.numMines
    , gridToView model.grid model.pixelSize
    , instructionText model.pixelSize]

statusText : Model -> Int -> Html Msg
statusText model size =
    div
    [ style "width" ((Debug.toString size) ++ "px")
    , style "position" "absolute"
    , style "left" ((Debug.toString (size + 20)) ++ "px")
    , style "top" ((Debug.toString (toFloat size / 3)) ++ "px")
    , style "font-size" ((Debug.toString (toFloat size / 10)) ++ "px")
    ]
    ([ Html.text ("Time Elapsed: " ++ (Debug.toString model.timeTick))
    , br [] []
    , Html.text ("Mines Present: " ++ (Debug.toString model.numMines))
    , br [] []
    , Html.text ("Flags Placed: " ++ (Debug.toString model.numFlags))]
    ++ case model.status of
        Lost -> [br [] []
                , div [style "color" "red"] [Html.text "You lost! Select a difficulty to play again!"]]
        Won -> [br [] []
                , div [style "color" "green"] [Html.text "You won! Select a difficulty to play again!"]]
        _ -> [])

buttons : Int -> Html Msg
buttons numMines =
    let
        selected =
            case numMines of
                10 -> 1
                40 -> 2
                99 -> 3
                _ -> Debug.todo "buttons: bad case"
        buttonStyles =
            [ style "width" "32%"
            , style "margin-right" "6px"]
    in
    
    div 

    [ style "width" ((Debug.toString pixelSize) ++ "px")
    , style "margin-bottom" "5px"
    , style "margin-left" "5px"]

    [ button (buttonStyles ++ [Html.Events.onClick (SetDifficulty 8 10)] ++ if selected == 1 then [style "background-color" "green"] else []) [Html.text "Easy (8x8, 10 Mines)"]
    , button (buttonStyles ++ [Html.Events.onClick (SetDifficulty 16 40)] ++ if selected == 2 then [style "background-color" "green"] else []) [Html.text "Medium (16x16, 24 Mines)"]
    , button (buttonStyles ++ [Html.Events.onClick (SetDifficulty 24 99)] ++ if selected == 3 then [style "background-color" "green"] else []) [Html.text "Hard (24x24, 99 Mines)"]]

instructionText : Int -> Html Msg
instructionText size =
    div
    [ style "width" ((Debug.toString size) ++ "px")
    , style "text-align" "center"
    , style "font-size" ((Debug.toString (toFloat size / 20)) ++ "px")
    , style "margin-top" "5px"
    ]
    [ Html.text "Left-click to uncover a space"
    , br [] []
    , Html.text "Right-click to flag a space"]

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
                    Html.Events.onClick (Uncover (row,col)),
                    onRightClick (Flag (row,col))
                    ])
                []
        Flagged ->
            div
                (styles ++
                    [
                    style "background-color" "green",
                    style "background-image" "url(images/flag.png",
                    style "background-repeat" "no-repeat",
                    style "background-size" ((Debug.toString size) ++ "px"),
                    onRightClick (Flag (row,col))
                    ])
                []
        Uncovered ->
                case tile of
                    Mine -> 
                        div
                        (styles ++ [style "background-color" "red",
                                    style "background-image" "url(images/mine.png",
                                    style "background-repeat" "no-repeat",
                                    Html.Events.onClick (Uncover (row,col)),
                                    style "background-size" ((Debug.toString size) ++ "px")])
                        []
                    NoMine i ->
                        div
                        styles
                        (case i of
                            0 -> []
                            _ ->
                                [div 
                                    [
                                    style "left" "30%", 
                                    style "top" "10%",
                                    style "position" "absolute",
                                    Html.Events.onClick (Uncover (row,col))
                                    ] 
                                    [text (Debug.toString i)]])

onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Html.Events.custom "contextmenu"
        (Json.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )

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

locToInt : Loc -> Int -> Int
locToInt (row,col) size =
    row * size + col + 1

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
addMine : Int -> Grid -> Int -> Grid
addMine n g size =
    let
        pos = intToLoc n size
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
uncoverGrid : Int -> Int -> (Grid, Status) -> (Grid, Status)
uncoverGrid x y (grid, status) =
    case (Array.get (y-1) grid) of
        Nothing -> (grid, status)
        Just row -> 
            case (Array.get (x-1) row) of
                Nothing -> (grid, status)
                Just (_, _, Uncovered) -> (grid, status) 
                Just (_, _, Flagged) ->  (grid, status) -- Clicking a flag should do nothing
                Just (loc, NoMine number, attr) ->
                    let
                        newGrid = Array.set (y-1) (Array.set (x-1) (loc, NoMine number, Uncovered) row) grid
                    in
                        if number /= 0 then
                            (newGrid, status)
                        else -- A bit messy, but this works!
                            uncoverGrid (x-1) (y-1) (newGrid, status)
                                |> uncoverGrid x (y-1) 
                                |> uncoverGrid (x+1) (y-1) 
                                |> uncoverGrid (x-1) y 
                                |> uncoverGrid (x+1) y 
                                |> uncoverGrid (x-1) (y+1) 
                                |> uncoverGrid x (y+1) 
                                |> uncoverGrid (x+1) (y+1)
                Just (loc, _, _) ->
                    let
                        newGrid = Array.set (y-1) (Array.set (x-1) (loc, Mine, Uncovered) row) grid
                        wholeGrid = uncoverAllGrid newGrid
                    in
                        (wholeGrid, Lost)                      

uncoverAllGrid : Grid -> Grid
uncoverAllGrid grid =
    Array.map (\row -> Array.map uncoverAllHelper row) grid

uncoverAllHelper : Square -> Square
uncoverAllHelper square = 
    case square of
        (loc, NoMine num, Covered) -> (loc, NoMine num, Uncovered)
        _ -> square -- Don't uncover any mines or flags, ignored already uncovered squares

-- Flag a square in the grid
flagInGrid : Int -> Int -> Grid -> (Grid, Int)
flagInGrid x y grid =
    let
        row = extractMaybe (Array.get (y-1) grid)
        (loc, mineInfo, covering) = extractMaybe (Array.get (x-1) row)
    in
        case covering of
            Uncovered -> (grid, 0)
            Flagged -> (Array.set (y-1) (Array.set (x-1) (loc, mineInfo, Covered) row) grid, -1)
            Covered -> (Array.set (y-1) (Array.set (x-1) (loc, mineInfo, Flagged) row) grid, 1)

checkWon : Model -> Bool
checkWon model =
    let
        spacesList = (List.range 1 (model.size ^ 2))
        getSquare (row,col) = Array.get (col) (extractMaybe (Array.get (row) model.grid))
        squaresList = (List.map (\m -> (extractMaybe (getSquare (intToLoc m model.size)))) spacesList)
    in
        -- Only mines remaining
        (List.all (\n -> case n of
                            (_, Mine, _) -> True
                            (_, NoMine _, Uncovered) -> True
                            _ -> False) squaresList)
        ||
        -- All mines flagged
        ((List.all (\n -> case n of
                            (_, Mine, Flagged) -> True
                            (_, Mine, _) -> False
                            _ -> True) squaresList)
            &&
            model.numMines == model.numFlags)

-- Directs the left-click to the proper function
clicking : Int -> Int -> (Grid, Status) -> (Grid, Status)
clicking x y (grid, status) =
    let
        row = extractMaybe (Array.get (y-1) grid)
        (loc, mineInfo, covering) = extractMaybe (Array.get (x-1) row)
    in
        case (mineInfo, covering) of
            (NoMine num, Uncovered) -> neighborUncover x y num (grid, status) -- Uncover the neighbors (using flagging)
            _ -> uncoverGrid x y (grid, status) -- Standard recursive uncovering

-- Uncovers the unflagged neighbors of a valid uncovered square
neighborUncover : Int -> Int -> Int -> (Grid, Status) -> (Grid, Status)
neighborUncover x y numNeeded (grid, status) =
    let
        -- Are there the proper number of flags adjacent?
        count = (flagCheck 0 [(x-1,y-1)
                            ,(x,y-1)
                            ,(x+1,y-1)
                            ,(x-1,y)
                            ,(x+1,y)
                            ,(x-1,y+1)
                            ,(x,y+1)
                            ,(x+1,y+1)] grid)
    in
        if count == numNeeded then
            uncoverGrid (x-1) (y-1) (grid, status)
                |> uncoverGrid x (y-1) 
                |> uncoverGrid (x+1) (y-1) 
                |> uncoverGrid (x-1) y 
                |> uncoverGrid (x+1) y 
                |> uncoverGrid (x-1) (y+1) 
                |> uncoverGrid x (y+1) 
                |> uncoverGrid (x+1) (y+1)
        else
            (grid, status)

-- Finds how many flags are present in the list (helper only ever given a list of neighbors)
flagCheck : Int -> List Loc -> Grid -> Int
flagCheck acc locs grid =
    case locs of
        [] -> acc
        (x,y)::rest ->
            case (Array.get (y-1) grid) of
                Nothing -> flagCheck acc rest grid
                Just row ->
                    case (Array.get (x-1) row) of
                        Just (_, _, Flagged) -> flagCheck (acc+1) rest grid
                        _ -> flagCheck acc rest grid
