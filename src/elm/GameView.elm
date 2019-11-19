module GameView exposing (renderGame)

import Game exposing (GameStatus)
import GameEngine exposing (Action)
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class, src, style)



---- Grid style utilities


styleGridWidth : Int -> Html.Attribute Action
styleGridWidth w =
    style "grid-template-columns" ("repeat(" ++ String.fromInt w ++ ", 1fr);")


styleGridHeight : Int -> Html.Attribute Action
styleGridHeight h =
    style "grid-template-row" ("repeat(" ++ String.fromInt h ++ ", 1fr);")


styleGridStartX x =
    style "grid-column-start" (String.fromInt x)


styleGridStartY y =
    style "grid-row-start" (String.fromInt y)


styleGridEndX x =
    style "grid-column-end" (String.fromInt x)


styleGridEndY y =
    style "grid-row-end" (String.fromInt y)



---- Nibbler HTML elements


nibblerBodyCell pos =
    div
        [ class "nibbler-cell"
        , styleGridStartX pos.x
        , styleGridStartY pos.y
        ]
        []


nibblerHeadCell pos =
    div
        [ class "nibbler-cell head"
        , styleGridStartX pos.x
        , styleGridStartY pos.y
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
            List.map nibblerBodyCell aTail

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
                , styleGridStartX (pos.x - 1)
                , styleGridStartY (pos.y - 1)
                , styleGridEndX (pos.x + 2)
                , styleGridEndY (pos.y + 2)
                ]
                [ Html.text "🍕" ]
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
            , styleGridWidth size.w
            , styleGridHeight size.h
            ]
            (gameField gs)
        ]
