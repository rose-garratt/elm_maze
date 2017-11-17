module View exposing (view)

import Array exposing (Array)
import Grid exposing (Position)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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


drawRow : Array Cell -> List String
drawRow cs =
    let
        asHorizontalBlock dir cell =
            let
                boundary =
                    Maze.boundaryOfCell dir cell
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


view : Model -> Html Message
view model =
    div [] [ div [ mainContainer ] [ mazeView model, buttonView model, asHtml [ drawNorthWall 10 { x = 0, y = 0 }, drawEastWall 10 { x = 0, y = 0 } ] ] ]


asHtml : List (S.Svg msg) -> Html msg
asHtml svgMsgs =
    S.svg [ SA.width "120", SA.height "120", SA.viewBox "0 0 120 120" ] svgMsgs


drawNorthWall : Int -> Coordinate -> S.Svg msg
drawNorthWall scale { x, y } =
    drawWall { x = x, y = y } { x = x + scale, y = y }


drawEastWall : Int -> Coordinate -> S.Svg msg
drawEastWall scale { x, y } =
    drawWall { x = x + scale, y = y } { x = x + scale, y = y + scale }


drawWall : Coordinate -> Coordinate -> S.Svg msg
drawWall c1 c2 =
    S.line [ SA.x1 (toString c1.x), SA.y1 (toString c1.y), SA.x2 (toString c2.x), SA.y2 (toString c2.y), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []


drawCell : Cell -> Position -> List (S.Svg msg)
drawCell cell position =
    let
        scale =
            10

        coordinateOfCell =
            asCoordinate position

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


asCoordinate : Position -> Coordinate
asCoordinate ( row, col ) =
    let
        scale =
            10
    in
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


mazeView : Model -> Html Message
mazeView model =
    let
        monospace =
            Html.Attributes.style [ ( "font-family", "monospace" ) ]

        ds =
            drawMaze model.maze

        t =
            div [ monospace ] <| List.map (\row -> div [] [ text row ]) ds
    in
    t
