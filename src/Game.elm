module Game exposing (beginGame, updateGame)

import Config
    exposing
        ( getZoneColors
        )
import Move
    exposing
        ( maybeAutoMove
        , maybeGetOutViaDiscard
        , moveToEndLoc
        )
import Piece
    exposing
        ( configPieces
        )
import Player
    exposing
        ( activateCard
        , configPlayers
        , coverCard
        , discardCard
        , ensureHandNotEmpty
        , getPlayer
        , replenishHand
        , setStartLocation
        , setTurn
        , setTurnToNeedCard
        , updateActivePlayer
        )
import Random
import Time
import Type
    exposing
        ( AppState(..)
        , Color
        , Game
        , GameMsg(..)
        , PieceLocation
        , Turn(..)
        )
import WhatIf


beginGame : Time.Posix -> Game
beginGame time =
    let
        numPlayers =
            4

        zoneColors =
            getZoneColors numPlayers

        activeColor =
            getActiveColor zoneColors

        seed =
            seedFromTime time
    in
    { zoneColors = zoneColors
    , pieceMap = configPieces zoneColors
    , players = configPlayers zoneColors
    , seed = seed
    , activeColor = activeColor
    }
        |> beginActiveTurn


updateGame : GameMsg -> Game -> Game
updateGame msg game =
    case msg of
        ActivateCard idx ->
            game
                |> updateActivePlayer (activateCard idx)

        DiscardCard idx ->
            game
                |> updateActivePlayer (discardCard idx)
                |> maybeGetOutViaDiscard
                |> ensureHandNotEmpty

        CoverCard idx ->
            game
                |> updateActivePlayer (coverCard idx)
                |> ensureHandNotEmpty

        RotateBoard ->
            game
                |> rotateBoard

        SetStartLocation clickedLoc ->
            game
                |> handleStartLocClick clickedLoc

        SetEndLocation clickedLoc ->
            game
                |> handleEndLocClick clickedLoc


seedFromTime : Time.Posix -> Random.Seed
seedFromTime time =
    Random.initialSeed (Time.posixToMillis time)


getActiveColor : List Color -> Color
getActiveColor zoneColors =
    -- appease compiler with Maybe
    List.head zoneColors
        |> Maybe.withDefault "bogus"


rotateColors : List Color -> List Color
rotateColors zones =
    List.drop 1 zones ++ List.take 1 zones


rotateBoard : Game -> Game
rotateBoard game =
    let
        oldPlayerColor =
            getActiveColor game.zoneColors

        newZoneColors =
            rotateColors game.zoneColors

        newPlayerColor =
            getActiveColor newZoneColors

        players =
            game.players
                |> setTurn oldPlayerColor TurnIdle
                |> setTurn newPlayerColor TurnBegin
    in
    { game
        | zoneColors = newZoneColors
        , players = players
        , activeColor = newPlayerColor
    }
        |> beginActiveTurn


beginActiveTurn : Game -> Game
beginActiveTurn game =
    game
        |> replenishHand
        |> setTurnToNeedCard
        |> WhatIf.debugWhatIf


handleStartLocClick : PieceLocation -> Game -> Game
handleStartLocClick location game =
    let
        activeColor =
            game.activeColor

        players =
            game.players

        activePlayer =
            getPlayer players activeColor
    in
    case activePlayer.turn of
        TurnNeedStartLoc _ ->
            game
                |> updateActivePlayer (setStartLocation location)
                |> maybeAutoMove location

        _ ->
            -- something is wrong with our click handlers
            game


handleEndLocClick : PieceLocation -> Game -> Game
handleEndLocClick endLoc game =
    let
        activeColor =
            game.activeColor

        players =
            game.players

        activePlayer =
            getPlayer players activeColor
    in
    case activePlayer.turn of
        TurnNeedEndLoc info ->
            let
                startLoc =
                    info.startLocation
            in
            game
                |> moveToEndLoc activePlayer startLoc endLoc

        _ ->
            -- something is wrong with our click handlers
            game
