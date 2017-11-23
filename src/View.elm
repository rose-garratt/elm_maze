module View exposing (view)

import Array exposing (Array)
import Grid exposing (Position)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (concat)
import Maze exposing (Boundary(..), Cell, Direction(..), Maze, boundaryOfCell)
import Model exposing (..)
import Svg as S exposing (Svg)
import Svg.Attributes as SA


mainContainer : Html.Attribute msg
mainContainer =
    style
        [ ( "backgroundColor", "" )
        , ( "width", "auto" )
        , ( "padding", "25px" )
        , ( "position", "center" )
        , ( "text-align", "center" )
        , ( "margin", "auto" )
        ]


buttonStyler : Html.Attribute msg
buttonStyler =
    style
        [ ( "padding", "25px" ) ]


view : Model -> Html Message
view model =
    div [] [ div [ mainContainer ] [ drawLineyMaze model.maze, buttonView model, test model.maze ] ]


test : Maze -> Html msg
test maze =
    let
        position =
            ( 0, 0 )

        canIMove : Bool
        canIMove =
            Maze.canIMove maze position

        dir =
            [ canIMove North, canIMove South, canIMove East, canIMove West ]
    in
    toString dir
        |> text


drawNorthWall : Int -> Coordinate -> S.Svg msg
drawNorthWall scale { x, y } =
    drawWall { x = x, y = y } { x = x + scale, y = y }


drawEastWall : Int -> Coordinate -> S.Svg msg
drawEastWall scale { x, y } =
    drawWall { x = x + scale, y = y } { x = x + scale, y = y + scale }


drawWall : Coordinate -> Coordinate -> S.Svg msg
drawWall c1 c2 =
    S.line [ SA.x1 (toString c1.x), SA.y1 (toString c1.y), SA.x2 (toString c2.x), SA.y2 (toString c2.y), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []



--TODO call draw cell for every cell in our maze
--drawLineyMaze : Maze -> List (S.Svg msg)


drawLineyMaze maze =
    --turn maze into grid of cell and position using indexedMap
    let
        --fn takes (position, cell) -> List of Svg
        scale =
            40

        cols =
            Grid.width maze

        rows =
            Grid.height maze

        fn : ( Position, Cell ) -> List (S.Svg a)
        fn ( position, cell ) =
            drawCell scale position cell

        width =
            cols
                * scale
                |> (+) 2
                |> toString

        height =
            rows
                * scale
                |> (+) 2
                |> toString

        viewbox : String
        viewbox =
            "-1 -1 " ++ width ++ " " ++ height

        asHtml : List (S.Svg msg) -> Html msg
        asHtml svgMsgs =
            S.svg [ SA.width width, SA.height height, SA.viewBox viewbox ] svgMsgs
    in
    Grid.indexedMap gridToCellNPosition maze
        |> Grid.map fn
        |> Grid.toList
        |> List.concat
        |> List.append (drawBoundary scale rows cols)
        |> asHtml


gridToCellNPosition : Position -> Cell -> ( Position, Cell )
gridToCellNPosition position cell =
    ( position, cell )


drawBoundary : Int -> Int -> Int -> List (S.Svg msg)
drawBoundary scale rows cols =
    let
        actualHeight =
            scale * rows

        actualWidth =
            scale * cols
    in
    [ drawWall { x = 0, y = 0 } { x = 0, y = actualHeight }, drawWall { x = 0, y = actualHeight } { x = actualWidth, y = actualHeight } ]


drawCell : Int -> Position -> Cell -> List (S.Svg msg)
drawCell scale position cell =
    let
        coordinateOfCell =
            asCoordinate position scale

        isNorthWall =
            boundaryOfCell North cell == Wall

        isEastWall =
            boundaryOfCell East cell == Wall

        drawNorthWithScaleAndCoords =
            drawNorthWall scale coordinateOfCell

        drawEastWithScaleAndCoords =
            drawEastWall scale coordinateOfCell
    in
    case ( isNorthWall, isEastWall ) of
        ( True, True ) ->
            [ drawNorthWithScaleAndCoords, drawEastWithScaleAndCoords ]

        ( True, False ) ->
            [ drawNorthWithScaleAndCoords ]

        ( False, True ) ->
            [ drawEastWithScaleAndCoords ]

        ( False, False ) ->
            []


type alias Coordinate =
    { x : Int, y : Int }


asCoordinate : Position -> Int -> Coordinate
asCoordinate ( row, col ) scale =
    { x = col * scale, y = row * scale }


buttonView : Model -> Html Message
buttonView model =
    let
        smallable =
            not (model.rowNo <= 5 || model.colNo <= 5)
    in
    div [ buttonStyler ]
        [ button [ onClick Next ] [ text "Next" ]
        , button [ onClick Bigger ] [ text "Bigger" ]
        , button [ onClick Smaller, disabled (not smallable) ] [ text "Smaller" ]
        ]
