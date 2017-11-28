module View exposing (view)

import Array exposing (Array)
import Grid exposing (Grid, Position)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (concat)
import Maze exposing (Boundary(..), Cell, Direction(..), Maze, boundaryOfCell)
import Model exposing (..)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import AStar
import Color exposing (Color)


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
    div [] [ div [ mainContainer ] [ drawLineyMaze model.maze, buttonView model ] ]


drawNorthWall : Int -> Coordinate -> S.Svg msg
drawNorthWall scale { x, y } =
    drawWall { x = x, y = y } { x = x + scale, y = y }


drawEastWall : Int -> Coordinate -> S.Svg msg
drawEastWall scale { x, y } =
    drawWall { x = x + scale, y = y } { x = x + scale, y = y + scale }


drawWall : Coordinate -> Coordinate -> S.Svg msg
drawWall c1 c2 =
    S.line [ SA.x1 (toString c1.x), SA.y1 (toString c1.y), SA.x2 (toString c2.x), SA.y2 (toString c2.y), SA.style "stroke:black;stroke-linecap:round;stroke-width:3" ] []


colorBlock : Int -> Coordinate -> Color -> S.Svg msg
colorBlock scale coord color =
    let
        fillStyle =
            "fill:" ++ colorToHtmlRgb color

        _ =
            Debug.log "Style = " fillStyle
    in
        S.rect [ SA.x (toString coord.x), SA.y (toString coord.y), SA.width (toString scale), SA.height (toString scale), SA.style fillStyle ] []


colorToHtmlRgb : Color -> String
colorToHtmlRgb color =
    let
        rgb =
            Color.toRgb color
    in
        "rgb(" ++ (toString rgb.red) ++ "," ++ (toString rgb.green) ++ "," ++ (toString rgb.blue) ++ ")"


colorCell : Int -> Position -> Color -> S.Svg msg
colorCell scale position color =
    let
        coordinateOfCell =
            asCoordinate position scale
    in
        colorBlock scale coordinateOfCell color


drawLineyMaze : Maze -> Html msg
drawLineyMaze maze =
    --turn maze into grid of cell and position using indexedMap
    let
        scale =
            40

        cols =
            Grid.width maze

        rows =
            Grid.height maze

        cg =
            colorGrid ( rows // 2, cols // 2 ) maze

        fnB : ( Position, Cell ) -> List (S.Svg a)
        fnB ( position, cell ) =
            drawCell scale position cell

        fnC : ( Position, Cell ) -> List (S.Svg a)
        fnC ( position, _ ) =
            let
                color =
                    Grid.get position cg
            in
                Maybe.map (\c -> [ colorCell scale position c ]) color
                    |> Maybe.withDefault []

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

        cellBorders =
            Grid.indexedMap gridToCellNPosition maze
                |> Grid.map fnB
                |> Grid.toList
                |> List.concat

        cellColors =
            Grid.indexedMap gridToCellNPosition maze
                |> Grid.map fnC
                |> Grid.toList
                |> List.concat

        boundaries =
            drawBoundary scale rows cols
    in
        List.concat
            [ cellColors
            , cellBorders
            , boundaries
            ]
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


colorGrid : Position -> Maze -> Grid.Grid Color
colorGrid origin maze =
    let
        absMax =
            Grid.height maze * Grid.width maze

        dists : Grid.Grid Int
        dists =
            AStar.breadthFirst maze origin
                |> Grid.map (Maybe.withDefault absMax)

        maxDist =
            dists
                |> Grid.toList
                |> List.maximum
                |> Maybe.withDefault absMax

        shade : Int -> Color
        shade dist =
            let
                intensity : Float
                intensity =
                    toFloat (maxDist - dist) / toFloat maxDist

                dark : Int
                dark =
                    (255.0 * intensity) |> round

                bright : Int
                bright =
                    128 + (128.0 * intensity |> round)
            in
                Color.rgb dark bright dark
    in
        Grid.map shade dists
