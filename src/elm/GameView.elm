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


nibbler : List Game.Position -> List (Html Action)
nibbler nibs =
    List.map
        nibblerHeadCell
        nibs


gameField : GameStatus -> List (Html Action)
gameField gs =
    case gs of
        Game.Playing ns _ ->
            nibbler ns.nibbler

        Game.NewGame ->
            [ text "Ready!" ]

        Game.Over _ ->
            [ text "Game Over" ]


renderGame : GameStatus -> Game.Size -> Html Action
renderGame gs size =
    div [ class "nibbler-game" ]
        [ div
            [ class "nibbler-canvas"
            , style "grid-template-columns" "repeat(11);"
            , canvasStyleW size.w
            , canvasStyleH size.h
            ]
            (gameField gs)
        ]
