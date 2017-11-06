module Main exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import Grid exposing (ColNo, Grid, Position, RowNo)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Random exposing (Generator)


main : Html msg
main =
    let
        rowNo =
            40

        colNo =
            40

        monospace =
            Html.Attributes.style [ ( "font-family", "monospace" ) ]

        generator =
            Random.list (rowNo * colNo) northOrEast

        seed =
            Random.initialSeed 99

        ( directions, newSeed ) =
            Random.step generator seed

        ds =
            buildMaze rowNo colNo
                |> Grid.indexedMap (createCell (Array.fromList directions) colNo)
                |> drawMaze

        t =
            div [ monospace ] <| List.map (\row -> div [] [ text row ]) ds
    in
    t


createCell : Array Direction -> ColNo -> RowNo -> ColNo -> Cell -> Cell
createCell directions width rowNo colNo cell =
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


type Boundary
    = Wall
    | Path


canIMove : Maze -> Position -> Direction -> Bool
canIMove maze pos dir =
    True


type Direction
    = North
    | East
    | South
    | West


type alias Maze =
    Array (Array Cell)


type alias Cell =
    ( Boundary, Boundary, Boundary, Boundary )


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


drawRow : Array Cell -> List String
drawRow cs =
    let
        asHorizontalBlock dir cell =
            let
                boundary =
                    boundaryOfCell dir cell
            in
            case boundary of
                Wall ->
                    "-+"

                Path ->
                    ".+"

        asMiddleBlock cell =
            let
                ( _, e, _, _ ) =
                    cell

                right =
                    case e of
                        Wall ->
                            "|"

                        Path ->
                            "."
            in
            "." ++ right

        top =
            Array.map (asHorizontalBlock North) cs |> Array.foldr (++) "" |> (++) "+"

        middle =
            Array.map asMiddleBlock cs |> Array.foldr (++) "" |> (++) "|"
    in
    top :: middle :: []


copyTopToBottom : List a -> List a
copyTopToBottom cttb =
    let
        top =
            List.take 1 cttb
    in
    cttb ++ top


drawMaze : Maze -> List String
drawMaze maze =
    Array.map drawRow maze
        |> Array.toList
        |> List.foldr (++) []
        |> copyTopToBottom


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
