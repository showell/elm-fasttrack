module Game exposing (beginGame, updateGame)

import Color
    exposing
        ( getActiveColor
        , getZoneColors
        , rotateColors
        )
import History
    exposing
        ( History
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


updateGame : GameMsg -> History Game -> Game -> ( History Game, Game )
updateGame msg history game =
    let
        {--
            We reset history **after** we rotate the board to the next
            player.  Then before any play with a card, we save the history
            **before** we apply that event.  And then as we're moving,
            we don't create any intermediate checkpoints.
        --}
        resetToNew newGame =
            let
                newHistory =
                    History.reset newGame
            in
            ( newHistory, newGame )

        savePrior newGame =
            let
                newHistory =
                    History.update history game
            in
            ( newHistory, newGame )

        leaveHistoryAlone newGame =
            ( history, newGame )
    in
    case msg of
        UndoAction ->
            History.undo history game

        ActivateCard idx ->
            game
                |> updateActivePlayer (activateCard idx)
                |> savePrior

        DiscardCard idx ->
            game
                |> updateActivePlayer (discardCard idx)
                |> maybeGetOutViaDiscard
                |> ensureHandNotEmpty
                |> savePrior

        CoverCard idx ->
            game
                |> updateActivePlayer (coverCard idx)
                |> ensureHandNotEmpty
                |> savePrior

        RotateBoard ->
            game
                |> rotateBoard
                |> resetToNew

        SetStartLocation clickedLoc ->
            game
                |> handleStartLocClick clickedLoc
                |> leaveHistoryAlone

        SetEndLocation clickedLoc ->
            game
                |> handleEndLocClick clickedLoc
                |> leaveHistoryAlone


seedFromTime : Time.Posix -> Random.Seed
seedFromTime time =
    Random.initialSeed (Time.posixToMillis time)


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
