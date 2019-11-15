module Game exposing
    ( GameStatus(..)
    , Position
    , Size
    , begin
    , info
    , title
    )


type alias Size =
    { w : Int, h : Int }


type alias Position =
    { x : Int, y : Int }


type alias NibblerStatus =
    { nibbler : List Position, lifes : Int, size : Size }


type alias Score =
    Int


type GameStatus
    = NewGame
    | Playing NibblerStatus Score
    | Over Score


begin : Int -> Int -> NibblerStatus
begin width height =
    { nibbler = [ { x = width // 2, y = height // 2 } ]
    , lifes = 3
    , size = { w = width, h = height }
    }


title : GameStatus -> String
title x =
    case x of
        NewGame ->
            "Welcome to Nibbler with Elm! Press any arrow key to start"

        Over score ->
            "Thank you for playng Nibbler with Elm! Your final score is " ++ String.fromInt score

        Playing _ score ->
            "Nibbler with Elm: Your current score is " ++ String.fromInt score


info : GameStatus -> List String
info x =
    case x of
        NewGame ->
            [ "Curent Score: 0 pts" ]

        Over score ->
            [ "Final Score: " ++ String.fromInt score ++ " pts" ]

        Playing gs score ->
            [ "Curren Score: " ++ String.fromInt score ++ " pts"
            , "Remaining Lifes: " ++ String.fromInt gs.lifes
            ]
