module GameEngine exposing (Action(..))

import Game exposing (Direction)


type Action
    = Nothing
    | Start (Maybe Direction)
    | Play (Maybe Direction)
    | StepUp Int
    | RollCheese
    | AddCheese ( Int, Int )
