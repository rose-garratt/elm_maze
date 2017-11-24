module Grid exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text)


type alias RowNo =
    Int


type alias ColNo =
    Int


type alias Position =
    ( RowNo, ColNo )


type alias Grid a =
    Array (Array a)


main : Html msg
main =
    let
        grid =
            create 4 6 0
                |> indexedMap (\row col a -> ( row, col ))
    in
        text <| toString grid


empty : Grid a
empty =
    Array.empty


height : Grid a -> RowNo
height grid =
    Array.length grid


width : Grid a -> ColNo
width grid =
    Array.get 0 grid
        |> Maybe.withDefault Array.empty
        |> Array.length


flatten : Grid a -> Array a
flatten =
    Array.foldl
        (\row accum ->
            Array.append accum row
        )
        Array.empty


toList : Grid a -> List a
toList grid =
    flatten grid |> Array.toList


get : Position -> Grid a -> Maybe a
get ( row, col ) grid =
    Array.get row grid
        |> Maybe.andThen (Array.get col)


set : Position -> a -> Grid a -> Grid a
set ( r, c ) value grid =
    let
        setInRow : Array a -> Array a
        setInRow cols =
            let
                oldValue =
                    Array.get c cols
            in
                case oldValue of
                    Nothing ->
                        cols

                    Just _ ->
                        Array.set c value cols

        replaceRow : Array a -> Grid a
        replaceRow newRow =
            Array.set r newRow grid
    in
        Array.get r grid
            |> Maybe.map setInRow
            |> Maybe.map replaceRow
            |> Maybe.withDefault grid


map : (a -> b) -> Grid a -> Grid b
map fn gd =
    Array.map (\row -> Array.map fn row) gd


indexedMap : (Position -> a -> b) -> Grid a -> Grid b
indexedMap fn gd =
    Array.indexedMap (\rownum row -> Array.indexedMap (\colnum col -> fn ( rownum, colnum ) col) row) gd


create : RowNo -> ColNo -> a -> Grid a
create rowNos colNos thing =
    Array.repeat colNos thing
        |> Array.repeat rowNos
