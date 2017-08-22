module Board exposing (..)

import Player exposing (Player)
import Matrix exposing (Matrix, Location)
import Maybe.Extra as Maybe
import List.Extra as List


type alias Board =
    Matrix (Maybe Player)


empty : Board
empty =
    Matrix.fromList
        [ [ Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing ]
        ]


update : Location -> Player -> Board -> ( Board, Bool )
update loc player board =
    if Maybe.isJust (Matrix.get loc board |> Maybe.join) then
        ( board, False )
    else
        ( Matrix.set loc (Just player) board, True )


toStringPos : Location -> Board -> String
toStringPos loc board =
    Matrix.get loc board
        |> Maybe.join
        |> Maybe.map Player.toString
        |> Maybe.withDefault ""


winner : Board -> Maybe Player
winner board =
    let
        asList =
            Matrix.toList board

        count : Player -> List (Maybe Player) -> Int
        count player =
            List.foldl
                (\p c ->
                    if p == Just player then
                        c + 1
                    else
                        c
                )
                0

        listWinner : List (Maybe Player) -> Maybe Player
        listWinner list =
            if (count Player.X list) == 3 then
                Just Player.X
            else if (count Player.O list) == 3 then
                Just Player.O
            else
                Nothing

        rowsWinner : List (List (Maybe Player)) -> List (Maybe Player)
        rowsWinner =
            List.map listWinner

        mainDiag =
            Matrix.mapWithLocation
                (\( r, c ) mP ->
                    if r == c then
                        mP
                    else
                        Nothing
                )
                board
                |> Matrix.flatten
                |> List.filter Maybe.isJust

        mainDiagWinner =
            listWinner mainDiag

        secondaryDiag =
            Matrix.mapWithLocation
                (\( r, c ) mP ->
                    if r + c == 2 then
                        mP
                    else
                        Nothing
                )
                board
                |> Matrix.flatten
                |> List.filter Maybe.isJust

        secondaryDiagWinner =
            listWinner secondaryDiag

        winners =
            List.concat
                [ rowsWinner asList
                , rowsWinner <| List.transpose asList
                , [ mainDiagWinner, secondaryDiagWinner ]
                ]
    in
        winners
            |> List.filter Maybe.isJust
            |> List.head
            |> Maybe.join
