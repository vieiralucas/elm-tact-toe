module BoardTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Matrix exposing (Matrix)
import Board
import Player exposing (Player)
import List.Extra as List


suite : Test
suite =
    describe "Board"
        [ describe "Board.update"
            [ test "returns True when board could be updated" <|
                \_ ->
                    Expect.equal True
                        (Board.empty
                            |> Board.update ( 0, 0 ) Player.X
                            |> Tuple.second
                        )
            , test "updates Board" <|
                \_ ->
                    Expect.equal
                        (Matrix.fromList
                            [ [ Just Player.X, Nothing, Nothing ]
                            , [ Nothing, Nothing, Nothing ]
                            , [ Nothing, Nothing, Nothing ]
                            ]
                        )
                        (Board.empty
                            |> Board.update ( 0, 0 ) Player.X
                            |> Tuple.first
                        )
            ]
        , describe "Board.toStringPos"
            [ test "returns empty string when location has nothing" <|
                \_ -> Expect.equal "" (Board.toStringPos ( 0, 0 ) Board.empty)
            , test "returns empty string when location is out of bounds nothing" <|
                \_ -> Expect.equal "" (Board.toStringPos ( 4, 4 ) Board.empty)
            , test "returns X string if location has a Player.X" <|
                \_ ->
                    let
                        ( board, _ ) =
                            Board.empty
                                |> Board.update ( 0, 0 ) Player.X
                    in
                        Expect.equal "X" (Board.toStringPos ( 0, 0 ) board)
            ]
        , describe "Board.winner"
            [ test "returns Nothing when theres no winner" <|
                \_ -> Expect.equal Nothing (Board.winner Board.empty)
            , test "returns the winning player when first row win" <|
                \_ ->
                    Expect.equal (Just Player.X) (Board.winner (buildWinRow 0))
            , test "returns the winning player when second row win" <|
                \_ ->
                    Expect.equal (Just Player.X) (Board.winner (buildWinRow 1))
            , test "returns the winning player when third row win" <|
                \_ ->
                    Expect.equal (Just Player.X) (Board.winner (buildWinRow 2))
            , test "returns the winning player when first col win" <|
                \_ ->
                    Expect.equal (Just Player.X) (Board.winner (buildWinCol 0))
            , test "returns the winning player when second col win" <|
                \_ ->
                    Expect.equal (Just Player.X) (Board.winner (buildWinCol 1))
            , test "returns the winning player when third col win" <|
                \_ ->
                    Expect.equal (Just Player.X) (Board.winner (buildWinCol 2))
            , test "returns the winning player when main diagonal wins" <|
                \_ ->
                    Expect.equal (Just Player.X)
                        (Board.winner
                            (Matrix.fromList
                                [ [ Just Player.X, Nothing, Nothing ]
                                , [ Nothing, Just Player.X, Nothing ]
                                , [ Nothing, Nothing, Just Player.X ]
                                ]
                            )
                        )
            , test "returns the Nothing if main diagonal is not a win " <|
                \_ ->
                    Expect.equal (Nothing)
                        (Board.winner
                            (Matrix.fromList
                                [ [ Just Player.X, Nothing, Nothing ]
                                , [ Nothing, Just Player.O, Nothing ]
                                , [ Nothing, Nothing, Just Player.X ]
                                ]
                            )
                        )
            , test "returns the winning player when secondary diagonal wins" <|
                \_ ->
                    Expect.equal (Just Player.X)
                        (Board.winner
                            (Matrix.fromList
                                [ [ Nothing, Nothing, Just Player.X ]
                                , [ Nothing, Just Player.X, Nothing ]
                                , [ Just Player.X, Nothing, Nothing ]
                                ]
                            )
                        )
            ]
        ]


buildWinCol : Int -> Matrix (Maybe Player)
buildWinCol col =
    Matrix.matrix 3
        3
        (\( y, x ) ->
            if x == col then
                Just Player.X
            else
                Nothing
        )


buildWinRow : Int -> Matrix (Maybe Player)
buildWinRow row =
    List.range 0 3
        |> List.map
            (\i ->
                if i == row then
                    [ Just Player.X, Just Player.X, Just Player.X ]
                else
                    [ Nothing, Nothing, Nothing ]
            )
        |> Matrix.fromList
