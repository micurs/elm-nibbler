module GameEngine exposing (Action(..))

import Game exposing (Direction)


type Action
    = NoOp
    | Start (Maybe Direction)
    | Play (Maybe Direction)
    | Pause
    | SpeedUp Int
    | RollCheese
    | AddCheese ( Int, Int )
