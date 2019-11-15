module Main exposing (main)

import Browser
import Game exposing (GameStatus)
import GameEngine exposing (Action)
import GameView exposing (renderGame)
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class, src, style)



---- Model ----


type alias Model =
    { gameStatus : GameStatus
    , gameSize : Game.Size
    }



---- Init ----


init : flags -> ( Model, Cmd Action )
init _ =
    ( { gameStatus = Game.NewGame
      , gameSize = { w = 51, h = 51 }
      }
    , Cmd.none
    )



--- Update ----


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        GameEngine.Start ->
            let
                xs =
                    model.gameSize.w // 2

                ys =
                    model.gameSize.h // 2
            in
            ( { model | gameStatus = Game.Playing (Game.begin xs ys) 0 }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- View ----


scoreInfo : GameStatus -> List (Html Action)
scoreInfo gs =
    List.map (\s -> div [] [ text s ]) (Game.info gs)


view : Model -> Html Action
view model =
    div [ class "d-flex flex-column justify-content-center align-content-center", style "height" "100vh" ]
        [ div [ class "nibbler-title" ]
            [ h1 [] [ text (Game.title model.gameStatus) ] ]
        , renderGame model.gameStatus model.gameSize
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
