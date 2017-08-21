module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Board exposing (Board, Pos)
import Player exposing (Player)


type alias Model =
    { board : Board
    , player : Player
    }


init : Model
init =
    { board = Board.empty
    , player = Player.X
    }


type Msg
    = Mark ( Int, Int ) Player


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark pos player ->
            let
                ( newBoard, didUpdate ) =
                    Board.update pos player model.board
            in
                { model
                    | board = newBoard
                    , player =
                        if didUpdate then
                            Player.switch model.player
                        else
                            model.player
                }


view : Model -> Html Msg
view { board, player } =
    let
        col y x =
            div
                [ style
                    [ ( "display", "flex" )
                    , ( "width", "145px" )
                    , ( "height", "175px" )
                    , ( "border", "1px solid black" )
                    , ( "font-size", "150px" )
                    , ( "padding-left", "55px" )
                    , ( "padding-top", "15px" )
                    , ( "text-align", "center" )
                    , ( "font-family", "Courier New, Courier, monospace" )
                    ]
                , onClick (Mark ( y, x ) player)
                ]
                [ text (Board.toStringPos ( y, x ) board) ]

        row y =
            List.range 0 2
                |> List.map (col y)

        rowStyle =
            style [ ( "display", "flex" ), ( "justify-content", "center" ) ]
    in
        div []
            [ h1 [] [ text "Welcome to Elm Tac Toe" ]
            , div []
                [ div [ rowStyle ] (row 0)
                , div [ rowStyle ] (row 1)
                , div [ rowStyle ] (row 2)
                ]
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
