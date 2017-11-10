module Main exposing (main)

import Array
import Grid exposing (ColNo, Grid, Position, RowNo)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Maze exposing (Maze)
import Model exposing (..)
import Random exposing (Generator)
import View exposing (..)


main =
    Html.beginnerProgram { model = model, view = View.view, update = update }


update : Message -> Model -> Model
update message model =
    case message of
        Next ->
            let
                newSeedInt =
                    model.seedInt + 1
            in
            buildModel model.rowNo model.colNo newSeedInt

        Bigger ->
            let
                newRowNo =
                    model.rowNo + 5

                newColNo =
                    model.colNo + 5
            in
            buildModel newRowNo newColNo model.seedInt

        Smaller ->
            let
                newRowNo =
                    if model.rowNo <= 5 || model.colNo <= 5 then
                        model.rowNo
                    else
                        model.rowNo - 5

                newColNo =
                    if model.rowNo <= 5 || model.colNo <= 5 then
                        model.colNo
                    else
                        model.colNo - 5
            in
            buildModel newRowNo newColNo model.seedInt


model : Model
model =
    buildModel 12 40 1


buildModel : RowNo -> ColNo -> Int -> Model
buildModel rowNo colNo seedInt =
    { rowNo = rowNo, colNo = colNo, seedInt = seedInt, maze = Maze.buildRandomMaze rowNo colNo seedInt }
