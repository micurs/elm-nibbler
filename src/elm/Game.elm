module Game exposing
    ( Direction(..)
    , GameStatus(..)
    , Position
    , Size
    , addCheese
    , begin
    , grow
    , info
    , rollCoordinates
    , title
    , update
    )

import Random


type alias Size =
    { w : Int, h : Int }


type alias Position =
    { x : Int, y : Int }


type Direction
    = Up
    | Down
    | Left
    | Right
    | Still


type alias Score =
    Int


type alias NibblerStatus =
    { nibbler : List Position
    , lifes : Int
    , size : Size
    , length : Int
    , direction : Direction
    , cheese : Maybe Position
    , score : Score
    }


type GameStatus
    = NewGame
    | Playing NibblerStatus
    | Over Score


areEqualPosition : Position -> Position -> Bool
areEqualPosition p1 p2 =
    p1.x == p2.x && p1.y == p2.y


positionFromTuple : ( Int, Int ) -> Position
positionFromTuple tp =
    { x = Tuple.first tp, y = Tuple.second tp }


rollCoordinates : Size -> Random.Generator ( Int, Int )
rollCoordinates size =
    Random.pair (Random.int 2 (size.w - 2)) (Random.int 2 (size.h - 2))


positionInTheMiddle : Size -> Position
positionInTheMiddle size =
    { x = size.w // 2, y = size.h // 2 }


buildNibblerHead : Size -> Position
buildNibblerHead =
    positionInTheMiddle


begin : Size -> NibblerStatus
begin size =
    { nibbler = [ positionInTheMiddle size ]
    , lifes = 3
    , size = size
    , length = 30
    , direction = Up
    , cheese = Nothing
    , score = 0
    }


newLife : NibblerStatus -> NibblerStatus
newLife ns =
    { ns
        | nibbler = [ positionInTheMiddle ns.size ]
        , lifes = ns.lifes - 1
        , length = 30
        , direction = Up
    }


actualDirection currDir newDir =
    case currDir of
        Still ->
            newDir

        Up ->
            if newDir == Down then
                Up

            else
                newDir

        Down ->
            if newDir == Up then
                Down

            else
                newDir

        Left ->
            if newDir == Right then
                Left

            else
                newDir

        Right ->
            if newDir == Left then
                Right

            else
                newDir


calcNewHead : Direction -> Position -> Position
calcNewHead dir pos =
    case dir of
        Still ->
            pos

        Up ->
            { x = pos.x, y = pos.y - 1 }

        Down ->
            { x = pos.x, y = pos.y + 1 }

        Left ->
            { x = pos.x - 1, y = pos.y }

        Right ->
            { x = pos.x + 1, y = pos.y }


trimTail : Int -> List Position -> List Position
trimTail len tail =
    if List.length tail <= len then
        tail

    else
        List.take len tail


outOfBoundary : Size -> Position -> Bool
outOfBoundary size pos =
    pos.x <= 1 || pos.x > size.w + 1 || pos.y <= 1 || pos.y > size.h + 1


calcNibbler : NibblerStatus -> Direction -> ( List Position, Bool )
calcNibbler ns direction =
    let
        newHead =
            case List.head ns.nibbler of
                Just pos ->
                    calcNewHead direction pos

                Nothing ->
                    calcNewHead direction (buildNibblerHead ns.size)

        newTail =
            trimTail ns.length ns.nibbler

        outOfField =
            outOfBoundary ns.size newHead

        selfHit =
            List.any
                (areEqualPosition newHead)
                newTail

        shouldDie =
            outOfField || selfHit
    in
    if direction == Still then
        ( ns.nibbler, False )

    else if shouldDie then
        ( ns.nibbler, shouldDie )

    else
        ( newHead :: newTail, shouldDie )


checkHitCheese : List Position -> Maybe Position -> Bool
checkHitCheese nibbler cheese =
    let
        mhead =
            List.head nibbler
    in
    case mhead of
        Just head ->
            case cheese of
                Just pos ->
                    areEqualPosition pos head

                Nothing ->
                    False

        _ ->
            False


updateNibbler : Direction -> NibblerStatus -> NibblerStatus
updateNibbler newDirection ns =
    let
        direction =
            actualDirection ns.direction newDirection

        cNibbler =
            calcNibbler ns direction

        shouldDie =
            Tuple.second cNibbler

        newNibbler =
            Tuple.first cNibbler

        hitTheCheese =
            checkHitCheese newNibbler ns.cheese

        growth =
            if hitTheCheese then
                10

            else
                0
    in
    if shouldDie then
        newLife ns

    else
        { ns
            | nibbler = newNibbler
            , direction = direction
            , length = ns.length + growth
            , score = ns.score + growth
            , cheese =
                if hitTheCheese then
                    Nothing

                else
                    ns.cheese
        }


addCheese : NibblerStatus -> ( Int, Int ) -> NibblerStatus
addCheese ns cheesePos =
    case ns.cheese of
        Nothing ->
            { ns | cheese = Just (positionFromTuple cheesePos) }

        Just _ ->
            ns


grow : NibblerStatus -> Int -> NibblerStatus
grow ns n =
    { ns | length = ns.length + n }


update : Maybe Direction -> NibblerStatus -> NibblerStatus
update mdir ns =
    case mdir of
        Just dir ->
            updateNibbler dir ns

        _ ->
            updateNibbler ns.direction ns


title : GameStatus -> String
title x =
    case x of
        NewGame ->
            "Welcome to Nibbler with Elm! Press any arrow key to start"

        Over score ->
            "Thank you for playng Nibbler with Elm! Your final score is " ++ String.fromInt score

        Playing ns ->
            "Nibbler with Elm: Your current score is " ++ String.fromInt ns.score


info : GameStatus -> List String
info x =
    case x of
        NewGame ->
            [ "Curent Score: 0 pts" ]

        Over score ->
            [ "Final Score: " ++ String.fromInt score ++ " pts" ]

        Playing ns ->
            [ "Curren Score: " ++ String.fromInt ns.score ++ " pts"
            , "Remaining Lifes: " ++ String.fromInt ns.lifes
            ]
