module View exposing (view)

import Array exposing (Array)
import Grid exposing (Position)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maze exposing (Boundary(..), Cell, Direction(..), Maze)
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
    div [] [ div [ mainContainer ] [ mazeView model, buttonView model, asHtml [ drawNorthWall { x = 0, y = 0 } 10, drawEastWall { x = 0, y = 0 } 10 ] ] ]


asHtml : List (S.Svg msg) -> Html msg
asHtml svgMsgs =
    S.svg [ SA.width "120", SA.height "120", SA.viewBox "0 0 120 120" ] svgMsgs


drawNorthWall : Coordinate -> Int -> S.Svg msg
drawNorthWall { x, y } scale =
    S.line [ SA.x1 (toString x), SA.y1 (toString y), SA.x2 (toString (x + scale)), SA.y2 (toString y), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []


drawEastWall : Coordinate -> Int -> S.Svg msg
drawEastWall { x, y } scale =
    S.line [ SA.x1 (toString (x + scale)), SA.y1 (toString y), SA.x2 (toString (x + scale)), SA.y2 (toString (y + scale)), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []


drawWall : Coordinate -> Coordinate -> Int -> S.Svg msg
drawWall { x1, y1 } { x2, y2 } scale =
    S.line [ SA.x1 (toString x1), SA.y1 (toString y1), SA.x2 (toString x2), SA.y2 (toString y2), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []


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
