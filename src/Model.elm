module Model exposing (..)

import Grid exposing (ColNo, RowNo)
import Maze exposing (Maze)


type alias Model =
    { rowNo : RowNo, colNo : ColNo, seedInt : Int, maze : Maze }


type Message
    = Next
    | Bigger
    | Smaller
