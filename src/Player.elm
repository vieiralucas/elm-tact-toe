module Player exposing (..)


type Player
    = O
    | X


toString : Player -> String
toString p =
    case p of
        O ->
            "O"

        X ->
            "X"


switch : Player -> Player
switch p =
    case p of
        O ->
            X

        X ->
            O
