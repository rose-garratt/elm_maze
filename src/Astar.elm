module Astar exposing (..)

import Grid exposing (Grid, Position, height, width)
import Maze exposing (Maze)


colorMaze : Maze -> Position -> Grid (Maybe Int)
colorMaze maze start =
    let
        maxDistance =
            height maze * width maze
    in
    Grid.empty
