module Main exposing (main)

import Array
import Grid exposing (ColNo, Grid, Position, RowNo)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Maze exposing (Maze)
import Random exposing (Generator)
import View


type alias Model =
    { rowNo : RowNo, colNo : ColNo, seedInt : Int, maze : Maze }


model : Model
model =
    { rowNo = 12, colNo = 40, seedInt = 1, maze = Maze.buildRandomMaze 12 40 1 }


main : Html msg
main =
    let
        monospace =
            Html.Attributes.style [ ( "font-family", "monospace" ) ]

        ds =
            View.drawMaze model.maze

        t =
            div [ monospace ] <| List.map (\row -> div [] [ text row ]) ds
    in
    t
