module AStar exposing (..)

import Grid exposing (Grid, Position)
import Maze exposing (Boundary(..), Cell, Direction(..), Maze, allDirections)
import Model exposing (..)
import Html exposing (Html, text)
import Maze exposing (buildRandomMaze)
import Dict exposing (Dict)
import Set


type alias Costs =
    Dict Position Int


type alias Positions =
    List Position


type alias Cost =
    Int


type alias State =
    { costs : Costs
    , adjPositions : Positions
    , adjCost : Cost
    }


breadthFirst : Maze -> Position -> Grid (Maybe Int)
breadthFirst maze start =
    let
        noValueYet : Maybe Int
        noValueYet =
            Nothing

        width =
            Grid.width maze

        height =
            Grid.height maze

        initialState =
            { costs = Dict.empty, adjPositions = [ start ], adjCost = 0 }

        findAdjacents : Positions -> Positions
        findAdjacents ps =
            List.concatMap (Maze.validNeighbours maze) ps |> Set.fromList |> Set.toList

        applyCosts : Costs -> Positions -> Cost -> Costs
        applyCosts costs ps cost =
            let
                apply : Position -> Costs -> Costs
                apply p costsIn =
                    let
                        setCost =
                            Dict.insert p cost

                        existingCost =
                            Dict.get p costsIn
                    in
                        setCost costsIn

                -- case existingCost of
                --     Nothing ->
                --         setCost
                --
                --     -- This should be impossible in a maze with no loops
                --     Just oldCost ->
                --         if oldCost < cost then
                --             costsIn
                --         else
                --             setCost
            in
                List.foldl apply costs ps

        discardVisitedPositions : Costs -> Positions -> Positions
        discardVisitedPositions cs ps =
            let
                notFound c p =
                    Dict.member p c == False
            in
                List.filter (notFound cs) ps

        fn : State -> State
        fn inbound =
            let
                costs_ =
                    applyCosts inbound.costs inbound.adjPositions inbound.adjCost

                adjPositions_ =
                    findAdjacents inbound.adjPositions
                        |> discardVisitedPositions costs_

                adjCost_ =
                    inbound.adjCost + 1
            in
                case inbound.adjPositions of
                    [] ->
                        { costs = costs_, adjPositions = adjPositions_, adjCost = adjCost_ }

                    ps ->
                        fn { costs = costs_, adjPositions = adjPositions_, adjCost = adjCost_ }

        initialGrid =
            Grid.create height width noValueYet

        costs : List ( Position, Cost )
        costs =
            fn initialState
                |> .costs
                |> Dict.toList
    in
        List.foldl (\tuple gridIn -> Grid.set (Tuple.first tuple) (Just (Tuple.second tuple)) gridIn) initialGrid costs
