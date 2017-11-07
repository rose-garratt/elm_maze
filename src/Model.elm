module Model exposing (..)

import Grid exposing (ColNo, RowNo)
import Maze exposing (Maze)


type alias Model =
    { rowNo : RowNo, colNo : ColNo, seedInt : Int, maze : Maze }


model : Model
model =
    { rowNo = 12, colNo = 40, seedInt = 1, maze = Maze.buildRandomMaze 12 40 1 }
