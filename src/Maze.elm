module Maze exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Grid exposing (ColNo, Grid, Position, RowNo)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Random exposing (Generator, Seed)


type Boundary
    = Wall
    | Path


type Direction
    = North
    | East
    | South
    | West


type alias Maze =
    Grid Cell


type alias Cell =
    ( Boundary, Boundary, Boundary, Boundary )


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
        dir =
            mazePath lastCol row col
    in
    case dir of
        Just d ->
            setBoundaryOfCell Path d cell

        Nothing ->
            cell


mazePath : ColNo -> RowNo -> ColNo -> Maybe Direction
mazePath lastCol r c =
    if r == 0 && c /= lastCol then
        Just East
    else if c == lastCol && r /= 0 then
        Just North
    else
        Nothing


canIMove : Maze -> Position -> Direction -> Bool
canIMove maze pos dir =
    --TODO check our boundaries or cell adjacent as well as position
    True


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


setBoundaryInMaze : Boundary -> Direction -> RowNo -> ColNo -> Maze -> Maze
setBoundaryInMaze b d r c m =
    let
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
