module Main exposing (main)

import Browser
import Game exposing (GameStatus)
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class, src, style)



---- Model ----


type Action
    = Nothing
    | Start


type alias Model =
    { gameStatus : GameStatus }



---- Init ----


init : flags -> ( Model, Cmd Action )
init _ =
    ( { gameStatus = Game.NewGame }
    , Cmd.none
    )



--- Update ----


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Start ->
            ( { gameStatus = Game.Playing (Game.begin 20 20) 0 }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- View ----


scoreInfo : GameStatus -> List (Html Action)
scoreInfo gs =
    List.map (\s -> div [] [ text s ]) (Game.info gs)


nibbler : List Game.Position -> List (Html Action)
nibbler nibs =
    List.map (\pos -> div [ class "nibbler-cell" ] []) nibs


gameField : GameStatus -> List (Html Action)
gameField gs =
    case gs of
        Game.Playing ns _ ->
            nibbler ns.nibbler

        Game.NewGame ->
            [ text "Ready!" ]

        Game.Over _ ->
            [ text "Game Over" ]


view : Model -> Html Action
view model =
    div [ class "d-flex flex-column justify-content-center align-content-center", style "height" "100vh" ]
        [ div [ class "nibbler-title" ]
            [ h1 [] [ text (Game.title model.gameStatus) ] ]
        , div [ class "nibbler-canvas" ] (gameField model.gameStatus)
        , div [ class "nibbler-scores" ] (scoreInfo model.gameStatus)
        ]



---- PROGRAM ----


main : Program () Model Action
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
