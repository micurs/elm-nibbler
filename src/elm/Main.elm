module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Game exposing (Direction, GameStatus, rollCoordinates)
import GameEngine exposing (Action)
import GameView exposing (renderGame)
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode
import Random
import Time exposing (..)



---- Model ----


type alias Model =
    { gameStatus : GameStatus
    , gameSize : Game.Size
    , speed : Float
    }



---- Init ----


init : flags -> ( Model, Cmd Action )
init _ =
    ( { gameStatus = Game.NewGame
      , gameSize = { w = 41, h = 41 }
      , speed = 200
      }
    , Cmd.none
    )



--- Update ----


play : Maybe Game.Direction -> Model -> Model
play mdir model =
    case model.gameStatus of
        Game.Playing ns ->
            if ns.lifes > 0 then
                { model | gameStatus = Game.Playing (Game.update mdir ns) }

            else
                { model | gameStatus = Game.Over ns.score }

        Game.NewGame ->
            { model | gameStatus = Game.Playing (Game.begin model.gameSize) }

        Game.Over _ ->
            model


grow n model =
    case model.gameStatus of
        Game.Playing ns ->
            { model | gameStatus = Game.Playing (Game.grow ns n) }

        _ ->
            model


addCheese : Model -> ( Int, Int ) -> Model
addCheese model cheesePos =
    case model.gameStatus of
        Game.Playing ns ->
            case ns.cheese of
                Nothing ->
                    { model | gameStatus = Game.Playing (Game.addCheese ns cheesePos) }

                _ ->
                    model

        _ ->
            model


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        GameEngine.RollCheese ->
            ( model, Random.generate GameEngine.AddCheese (rollCoordinates model.gameSize) )

        GameEngine.AddCheese pos ->
            ( addCheese model pos, Cmd.none )

        GameEngine.SpeedUp n ->
            ( { model | speed = model.speed - model.speed * 0.1 }, Cmd.none )

        GameEngine.Play mdir ->
            ( play mdir model, Cmd.none )

        GameEngine.Start mdir ->
            ( { model | gameStatus = Game.NewGame }, Cmd.none )

        _ ->
            ( model, Cmd.none )


scoreInfo : GameStatus -> List (Html Action)
scoreInfo gs =
    List.map (\s -> div [] [ text s ]) (Game.info gs)


view : Model -> Html Action
view model =
    div
        [ class "d-flex flex-column justify-content-center align-content-center"
        , style "height" "100vh"
        ]
        [ div [ class "nibbler-title" ]
            [ h1 [] [ text (Game.title model.gameStatus) ] ]
        , renderGame [] model.gameStatus model.gameSize
        , div [ class "nibbler-scores" ] (scoreInfo model.gameStatus)
        ]



---- SUBSCRIPTIONS ----


toDirection : String -> Action
toDirection k =
    let
        key =
            String.uncons k
    in
    case key of
        Just ( 'p', _ ) ->
            GameEngine.Play (Just Game.Still)

        Just ( 'w', _ ) ->
            GameEngine.Play (Just Game.Up)

        Just ( 'x', _ ) ->
            GameEngine.Play (Just Game.Down)

        Just ( 's', _ ) ->
            GameEngine.Play (Just Game.Down)

        Just ( 'a', _ ) ->
            GameEngine.Play (Just Game.Left)

        Just ( 'd', _ ) ->
            GameEngine.Play (Just Game.Right)

        _ ->
            GameEngine.Play Nothing


keyDecoder : Decode.Decoder Action
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


animate : Model -> Sub Action
animate model =
    case model.gameStatus of
        Game.Playing _ ->
            Time.every model.speed (\_ -> GameEngine.Play Nothing)

        _ ->
            Sub.none


speedUp : Model -> Sub Action
speedUp model =
    case model.gameStatus of
        Game.Playing _ ->
            Time.every 10000 (\_ -> GameEngine.SpeedUp 1)

        _ ->
            Sub.none


placeCheese : Model -> Sub Action
placeCheese model =
    case model.gameStatus of
        Game.Playing _ ->
            Time.every 1000 (\_ -> GameEngine.RollCheese)

        _ ->
            Sub.none


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ onKeyPress keyDecoder
        , animate model
        , speedUp model
        , placeCheese model
        ]



---- PROGRAM ----


main : Program () Model Action
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
