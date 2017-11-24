module Maze exposing (..)

import Array exposing (Array)
import Grid exposing (ColNo, Grid, Position, RowNo)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Random exposing (Generator, Seed)
import List.Extra exposing (zip)


type Boundary
    = Wall
    | Path


type Direction
    = North
    | East
    | South
    | West


allDirections =
    [ North, South, East, West ]


type alias Maze =
    Grid Cell


type alias Cell =
    ( Boundary, Boundary, Boundary, Boundary )


validNeighbours : Maze -> Position -> List Position
validNeighbours maze from =
    validDirections maze from
        |> List.map (move from)


validDirections : Maze -> Position -> List Direction
validDirections maze from =
    List.map (canIMove maze from) allDirections
        |> List.Extra.zip allDirections
        |> List.filter Tuple.second
        |> List.map Tuple.first


move : Position -> Direction -> Position
move ( row, col ) dir =
    case dir of
        North ->
            ( row - 1, col )

        South ->
            ( row + 1, col )

        East ->
            ( row, col + 1 )

        West ->
            ( row, col - 1 )


canIMove : Maze -> Position -> Direction -> Bool
canIMove maze pos dir =
    let
        canIMoveEast maze pos =
            let
                maybeCell =
                    Grid.get pos maze
            in
                case maybeCell of
                    Just cell ->
                        boundaryOfCell East cell == Path

                    Nothing ->
                        False

        canIMoveNorth maze pos =
            Grid.get pos maze
                |> Maybe.map (boundaryOfCell North)
                |> Maybe.map ((==) Path)
                |> Maybe.withDefault False

        canIMoveWest maze pos =
            canIMoveEast maze <| move pos West

        canIMoveSouth maze pos =
            canIMoveNorth maze <| move pos South
    in
        case dir of
            North ->
                canIMoveNorth maze pos

            South ->
                canIMoveSouth maze pos

            East ->
                canIMoveEast maze pos

            West ->
                canIMoveWest maze pos


boundaryOfCell : Direction -> Cell -> Boundary
boundaryOfCell dir cell =
    let
        ( n, e, s, w ) =
            cell
    in
        case dir of
            North ->
                n

            South ->
                s

            East ->
                e

            West ->
                w


createCell : Array Direction -> ColNo -> Position -> Cell -> Cell
createCell directions width ( rowNo, colNo ) cell =
    let
        index =
            (rowNo * width) + colNo

        direction =
            Maybe.withDefault North (Array.get index directions)

        filterDirection : Direction -> Maybe Direction
        filterDirection filterDir =
            let
                lastCol =
                    width - 1

                topRow =
                    rowNo == 0

                rightCol =
                    colNo == lastCol
            in
                if topRow && not rightCol then
                    Just East
                else if rightCol && not topRow then
                    Just North
                else if topRow && rightCol then
                    Nothing
                else
                    Just direction
    in
        case filterDirection direction of
            Just x ->
                setBoundaryOfCell
                    Path
                    x
                    cell

            Nothing ->
                cell


mazeModifier : ColNo -> RowNo -> ColNo -> Cell -> Cell
mazeModifier lastCol row col cell =
    let
        mazePath : ColNo -> RowNo -> ColNo -> Maybe Direction
        mazePath lastCol r c =
            if r == 0 && c /= lastCol then
                Just East
            else if c == lastCol && r /= 0 then
                Just North
            else
                Nothing

        dir =
            mazePath lastCol row col
    in
        case dir of
            Just d ->
                setBoundaryOfCell Path d cell

            Nothing ->
                cell


setBoundaryInMaze : Boundary -> Direction -> RowNo -> ColNo -> Maze -> Maze
setBoundaryInMaze b d r c m =
    let
        setBoundaryInRow : Boundary -> Direction -> ColNo -> Array Cell -> Array Cell
        setBoundaryInRow b d c cs =
            let
                cell : Maybe Cell
                cell =
                    Array.get c cs

                newCell : Maybe Cell
                newCell =
                    Maybe.map (setBoundaryOfCell b d) cell

                changeCell : Cell -> Array Cell
                changeCell newCell_ =
                    Array.set c newCell_ cs
            in
                Maybe.map changeCell newCell
                    |> Maybe.withDefault cs

        row : Maybe (Array Cell)
        row =
            Array.get r m

        newRow : Maybe (Array Cell)
        newRow =
            Maybe.map (setBoundaryInRow b d c) row

        changeRow : Array Cell -> Maze
        changeRow newRow_ =
            Array.set r newRow_ m
    in
        Maybe.map changeRow newRow
            |> Maybe.withDefault m


setPathInMaze : Direction -> RowNo -> ColNo -> Maze -> Maze
setPathInMaze =
    setBoundaryInMaze Path


setPathNorth : RowNo -> ColNo -> Maze -> Maze
setPathNorth =
    setPathInMaze North


setPathEast : RowNo -> ColNo -> Maze -> Maze
setPathEast =
    setPathInMaze East


setBoundaryOfCell : Boundary -> Direction -> Cell -> Cell
setBoundaryOfCell boun dir cell =
    let
        ( n, e, s, w ) =
            cell
    in
        case dir of
            North ->
                ( boun, e, s, w )

            East ->
                ( n, boun, s, w )

            South ->
                ( n, e, boun, w )

            West ->
                ( n, e, s, boun )


buildMaze : Int -> Int -> Maze
buildMaze r c =
    let
        cell =
            ( Wall, Wall, Wall, Wall )

        row =
            Array.repeat c cell
    in
        Array.repeat r row


northOrEast : Generator Direction
northOrEast =
    Random.map
        (\b ->
            if b then
                North
            else
                East
        )
        Random.bool


makeSeed : Int -> Random.Seed
makeSeed int =
    Random.initialSeed int


buildRandomMaze : RowNo -> ColNo -> Int -> Maze
buildRandomMaze rowNo colNo seedInt =
    let
        generator =
            Random.list (rowNo * colNo) northOrEast

        seed =
            makeSeed seedInt

        ( directions, newSeed ) =
            Random.step generator seed
    in
        buildMaze rowNo colNo
            |> Grid.indexedMap (createCell (Array.fromList directions) colNo)
