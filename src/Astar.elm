module AStar exposing (..)

import Grid exposing (Grid, Position, create, height, width)
import Maze exposing (Maze, validNeighbours)


type alias Distance =
    Int


type alias Distances =
    Grid (Maybe Distance)


breadthFirst : Maze -> Position -> Distances
breadthFirst maze start =
    let
        h =
            height maze

        w =
            width maze

        maxDistance =
            h * w

        g =
            Grid.create h w (Just maxDistance)
                |> Grid.set ( 0, 0 ) (Just 0)
                |> Grid.set ( 1, 1 ) (Just (maxDistance // 2))

        --get start pos, set dist as 0, get neighbours set as +1
        --current cost neighbours to visit carry this on spreading through grid carry answer through grid
    in
    g


setDistanceOfNeighbours : Maze -> ( List Position, Distance, Distances ) -> ( List Position, Distance, Distances )
setDistanceOfNeighbours maze ( currentPositions, distance, distances ) =
    let
        isDistanceless : Position -> Bool
        isDistanceless position =
            Grid.get position distances
                |> Maybe.Extra.join
                |> Maybe.Extra.isNothing

        applyDistance : Position -> Distances -> Distances
        applyDistance position distances =
            Grid.set position distance distances

        --invoke validNeighbours maze on ever element of current positions (and flatten it using concatMap)
        currentNeighbours =
            List.concatMap (Maze.validNeighbours maze) currentPositions

        awesomeNeighbours =
            List.filter isDistanceless currentNeighbours

        newDistances =
            List.foldl applyDistance distances awesomeNeighbours
    in
    ( awesomeNeighbours, distance + 1, newDistances )
