module Board exposing (..)

import Dict exposing (Dict)
import Player exposing (Player)


type alias Pos =
    ( Int, Int )


type alias Board =
    Dict Pos Player


empty : Board
empty =
    Dict.empty


update : Pos -> Player -> Board -> ( Board, Bool )
update pos player board =
    let
        upFn : Maybe Player -> Maybe Player
        upFn mp =
            case mp of
                Nothing ->
                    Just player

                Just p ->
                    Just p

        isJust : Maybe a -> Bool
        isJust m =
            case m of
                Just _ ->
                    True

                _ ->
                    False
    in
        if isJust (Dict.get pos board) then
            ( board, False )
        else
            ( Dict.update pos upFn board, True )


toStringPos : Pos -> Board -> String
toStringPos pos board =
    Dict.get pos board
        |> Maybe.map Player.toString
        |> Maybe.withDefault ""
