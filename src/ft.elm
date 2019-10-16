module Main exposing (..)

import Browser
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
        , beginTurn
        , configPlayers
        , coverCard
        , discardCard
        , getPlayer
        , getPlayerMoveType
        , replenishHand
        , setStartLocation
        , setTurn
        , updateActivePlayer
        )
import Random
import Set
import Task
import Time
import Type
    exposing
        ( AppState(..)
        , Color
        , Location
        , Model
        , Msg(..)
        , PieceLocation
        , Player
        , Turn(..)
        , TurnNeedEndLocInfo
        , TurnNeedStartLocInfo
        )
import View
    exposing
        ( view
        )
import WhatIf



-- MODEL / INIT


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        numPlayers =
            4

        zoneColors =
            getZoneColors numPlayers

        model =
            { zoneColors = zoneColors
            , pieceMap = configPieces zoneColors
            , players = configPlayers zoneColors
            , seed = Random.initialSeed 42
            , state = Loading
            , getActiveColor = getActiveColor
            }
    in
    ( model, Task.perform LoadGame Time.now )


replenishActiveHand : Model -> Model
replenishActiveHand model =
    let
        activeColor =
            getActiveColor model.zoneColors
    in
    model
        |> replenishHand activeColor


beginActiveTurn : Model -> Model
beginActiveTurn model =
    let
        activeColor =
            getActiveColor model.zoneColors
    in
    model
        |> beginTurn activeColor
        |> WhatIf.debugWhatIf


seedFromTime : Time.Posix -> Random.Seed
seedFromTime time =
    Random.initialSeed (Time.posixToMillis time)


getActiveColor : List Color -> Color
getActiveColor zoneColors =
    -- appease compiler with Maybe
    List.head zoneColors
        |> Maybe.withDefault "bogus"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadGame time ->
            let
                seed =
                    seedFromTime time

                model_ =
                    { model
                        | seed = seed
                        , state = Ready
                    }
                        |> beginActiveTurn
            in
            ( model_, Cmd.none )

        SetStartLocation clickedLoc ->
            let
                model_ =
                    model
                        |> handleStartLocClick clickedLoc
            in
            ( model_, Cmd.none )

        SetEndLocation clickedLoc ->
            let
                model_ =
                    model
                        |> handleEndLocClick clickedLoc
            in
            ( model_, Cmd.none )

        ReplenishHand ->
            let
                model_ =
                    model
                        |> replenishActiveHand
            in
            ( model_, Cmd.none )

        ActivateCard playerColor idx ->
            let
                model_ =
                    model
                        |> updateActivePlayer (activateCard idx)
            in
            ( model_, Cmd.none )

        DiscardCard playerColor idx ->
            let
                model_ =
                    model
                        |> updateActivePlayer (discardCard idx)
                        |> maybeGetOutViaDiscard playerColor
            in
            ( model_, Cmd.none )

        CoverCard playerColor idx ->
            let
                model_ =
                    model
                        |> updateActivePlayer (coverCard idx)
                        |> maybeGetOutViaDiscard playerColor
            in
            ( model_, Cmd.none )

        RotateBoard ->
            let
                oldPlayerColor =
                    getActiveColor model.zoneColors

                newZoneColors =
                    rotateBoard model.zoneColors

                newPlayerColor =
                    getActiveColor newZoneColors

                players =
                    model.players
                        |> setTurn oldPlayerColor TurnIdle
                        |> setTurn newPlayerColor TurnBegin

                model_ =
                    { model
                        | zoneColors = newZoneColors
                        , players = players
                    }
                        |> beginActiveTurn
            in
            ( model_, Cmd.none )


rotateBoard : List Color -> List Color
rotateBoard zones =
    List.drop 1 zones ++ List.take 1 zones


handleStartLocClick : PieceLocation -> Model -> Model
handleStartLocClick location model =
    let
        activeColor =
            getActiveColor model.zoneColors

        players =
            model.players

        activePlayer =
            getPlayer players activeColor
    in
    case activePlayer.turn of
        TurnNeedStartLoc _ ->
            model
                |> updateActivePlayer (setStartLocation location)
                |> maybeAutoMove location

        _ ->
            -- something is wrong with our click handlers
            model


handleEndLocClick : PieceLocation -> Model -> Model
handleEndLocClick endLoc model =
    let
        activeColor =
            getActiveColor model.zoneColors

        players =
            model.players

        activePlayer =
            getPlayer players activeColor
    in
    case activePlayer.turn of
        TurnNeedEndLoc info ->
            let
                startLoc =
                    info.startLocation
            in
            model
                |> moveToEndLoc activePlayer activeColor startLoc endLoc

        _ ->
            -- something is wrong with our click handlers
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- for VIEW, see View.elm
