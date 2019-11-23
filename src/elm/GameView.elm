module GameView exposing
    ( renderGame
    , renderInfo
    , renderTitle
    )

import Game exposing (GameStatus, NibblerStatus, Position)
import GameEngine exposing (Action)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class, style)



---- Grid style utilities


styleGridWidth : Int -> Html.Attribute Action
styleGridWidth w =
    style "grid-template-columns" ("repeat(" ++ String.fromInt w ++ ", 1fr);")


styleGridHeight : Int -> Html.Attribute Action
styleGridHeight h =
    style "grid-template-row" ("repeat(" ++ String.fromInt h ++ ", 1fr);")


styleGridStartX : Int -> Html.Attribute Action
styleGridStartX x =
    style "grid-column-start" (String.fromInt x)


styleGridStartY : Int -> Html.Attribute Action
styleGridStartY y =
    style "grid-row-start" (String.fromInt y)


styleGridEndX : Int -> Html.Attribute Action
styleGridEndX x =
    style "grid-column-end" (String.fromInt x)


styleGridEndY : Int -> Html.Attribute Action
styleGridEndY y =
    style "grid-row-end" (String.fromInt y)



---- Nibbler HTML elements


nibblerBodyCell : Position -> Html Action
nibblerBodyCell pos =
    div
        [ class "nibbler-cell"
        , styleGridStartX pos.x
        , styleGridStartY pos.y
        ]
        []


nibblerHeadCell : Position -> Html Action
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


cheeseStar : Maybe Position -> List (Html Action)
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


paintNibbler : NibblerStatus -> List (Html Action)
paintNibbler ns =
    nibblerHead ns.nibbler ++ nibblerTail ns.nibbler ++ cheeseStar ns.cheese


gameField : GameStatus -> List (Html Action)
gameField gs =
    case gs of
        Game.Playing ns ->
            paintNibbler ns

        Game.NewGame ->
            [ div [ class "message" ]
                [ p [] [ text "Ready!" ]
                , p [] [ text "Use a,d and w,s to change direction" ]
                ]
            ]

        Game.Over score ->
            [ div [ class "message" ]
                [ text ("Game Over: " ++ String.fromInt score ++ " points") ]
            ]

        Game.Pause ns ->
            paintNibbler ns
                ++ [ div [ class "message" ]
                        [ p [] [ text "Game Paused!" ]
                        , p [] [ text "Use a,d and w,s to change direction" ]
                        ]
                   ]


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


renderTitle : GameStatus -> Html Action
renderTitle gs =
    div [ class "nibbler-title" ]
        [ h1 [] [ text (Game.title gs) ] ]


renderInfo : GameStatus -> Html Action
renderInfo gs =
    div [ class "nibbler-scores" ] (scoreInfo gs)


scoreInfo : GameStatus -> List (Html Action)
scoreInfo gs =
    List.map (\s -> div [] [ text s ]) (Game.info gs)
