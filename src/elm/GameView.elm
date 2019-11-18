module GameView exposing (renderGame)

import Game exposing (GameStatus)
import GameEngine exposing (Action)
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class, src, style)


canvasStyleW : Int -> Html.Attribute Action
canvasStyleW w =
    style "grid-template-columns" ("repeat(" ++ String.fromInt w ++ ", 1fr);")


canvasStyleH : Int -> Html.Attribute Action
canvasStyleH h =
    style "grid-template-row" ("repeat(" ++ String.fromInt h ++ ", 1fr);")


nibblerBodyCell pos =
    div
        [ class "nibbler-cell"
        , style "grid-column-start" (String.fromInt pos.x)
        , style "grid-row-start" (String.fromInt pos.y)
        ]
        []


nibblerHeadCell pos =
    div
        [ class "nibbler-cell head"
        , style "grid-column-start" (String.fromInt pos.x)
        , style "grid-row-start" (String.fromInt pos.y)
        ]
        []


nibblerTail : List Game.Position -> List (Html Action)
nibblerTail nibbs =
    let
        tail =
            List.tail nibbs
    in
    case tail of
        Just aTail ->
            List.map (\c -> nibblerBodyCell c) aTail

        Nothing ->
            []


nibblerHead : List Game.Position -> List (Html Action)
nibblerHead nibs =
    case List.head nibs of
        Just cell ->
            [ nibblerHeadCell cell ]

        Nothing ->
            []


cheeseStar cheesePos =
    case cheesePos of
        Just pos ->
            [ div
                [ class "cheese"
                , style "grid-column-start" (String.fromInt (pos.x - 1))
                , style "grid-row-start" (String.fromInt (pos.y - 1))
                , style "grid-column-end" (String.fromInt (pos.x + 2))
                , style "grid-row-end" (String.fromInt (pos.y + 2))
                ]
                [ Html.text "🍪" ]
            ]

        Nothing ->
            []


gameField : GameStatus -> List (Html Action)
gameField gs =
    case gs of
        Game.Playing ns ->
            nibblerHead ns.nibbler ++ nibblerTail ns.nibbler ++ cheeseStar ns.cheese

        Game.NewGame ->
            [ text "Ready!" ]

        Game.Over score ->
            [ text ("Game Over: " ++ String.fromInt score ++ " points") ]


renderGame : List (Html.Attribute Action) -> GameStatus -> Game.Size -> Html Action
renderGame moreAttr gs size =
    div ([ class "nibbler-game" ] ++ moreAttr)
        [ div
            [ class "nibbler-canvas"
            , style "grid-template-columns" "repeat(11);"
            , canvasStyleW size.w
            , canvasStyleH size.h
            ]
            (gameField gs)
        ]
