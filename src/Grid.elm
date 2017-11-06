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
            createGrid 4 6 0
                |> indexedMap (\row col a -> ( row, col ))
    in
    text <| toString grid


create : RowNo -> ColNo -> a -> Grid a
create rowNo colNo thing =
    let
        row =
            Array.repeat colNo thing
    in
    Array.repeat rowNo row


height : Grid a -> RowNo
height grid =
    Array.length grid



-- width : Grid a -> ColNo
-- width grid =
--   Array.length (Array.get 0 grid) --this doesn't work because It's outputting a Maybe
--
-- get : Position -> Grid a -> Maybe a
-- get position grid =
--   let


map : (a -> b) -> Grid a -> Grid b
map fn gd =
    Array.map (\row -> Array.map fn row) gd


indexedMap : (RowNo -> ColNo -> a -> b) -> Grid a -> Grid b
indexedMap fn gd =
    Array.indexedMap (\rownum row -> Array.indexedMap (\colnum col -> fn rownum colnum col) row) gd


createGrid : RowNo -> ColNo -> a -> Grid a
createGrid r c intval =
    Array.repeat r (Array.repeat c intval)
