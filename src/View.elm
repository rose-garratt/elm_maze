module View exposing (view)

import Array exposing (Array)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maze exposing (Boundary(..), Cell, Direction(..), Maze)
import Model exposing (..)
import Svg as S
import Svg.Attributes as SA


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
    div [] [ mazeView model, buttonView model, roundRect ]


roundRect : Html msg
roundRect =
    S.svg
        [ SA.width "120", SA.height "120", SA.viewBox "0 0 120 120" ]
        [ S.rect [ SA.x "10", SA.y "10", SA.width "100", SA.height "100", SA.rx "15", SA.ry "15" ] [] ]


buttonView : Model -> Html Message
buttonView model =
    let
        smallable =
            not (model.rowNo <= 5 || model.colNo <= 5)
    in
    div []
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
