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
        , configPlayers
        , coverCard
        , discardCard
        , ensureHandNotEmpty
        , getPlayer
        , getPlayerMoveType
        , replenishHand
        , setStartLocation
        , setTurn
        , setTurnToNeedCard
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

        activeColor =
            getActiveColor zoneColors

        model =
            { zoneColors = zoneColors
            , pieceMap = configPieces zoneColors
            , players = configPlayers zoneColors
            , seed = Random.initialSeed 42
            , state = Loading
            , activeColor = activeColor
            }
    in
    ( model, Task.perform LoadGame Time.now )


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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        LoadGame time ->
            model
                |> updateSeed time
                |> makeReady
                |> beginActiveTurn

        ActivateCard playerColor idx ->
            model
                |> updateActivePlayer (activateCard idx)

        DiscardCard playerColor idx ->
            model
                |> updateActivePlayer (discardCard idx)
                |> maybeGetOutViaDiscard
                |> ensureHandNotEmpty

        CoverCard playerColor idx ->
            model
                |> updateActivePlayer (coverCard idx)
                |> ensureHandNotEmpty

        RotateBoard ->
            model
                |> rotateBoard

        SetStartLocation clickedLoc ->
            model
                |> handleStartLocClick clickedLoc

        SetEndLocation clickedLoc ->
            model
                |> handleEndLocClick clickedLoc


rotateBoard : Model -> Model
rotateBoard model =
    let
        oldPlayerColor =
            getActiveColor model.zoneColors

        newZoneColors =
            rotateColors model.zoneColors

        newPlayerColor =
            getActiveColor newZoneColors

        players =
            model.players
                |> setTurn oldPlayerColor TurnIdle
                |> setTurn newPlayerColor TurnBegin
    in
    { model
        | zoneColors = newZoneColors
        , players = players
        , activeColor = newPlayerColor
    }
        |> beginActiveTurn


beginActiveTurn : Model -> Model
beginActiveTurn model =
    let
        activeColor =
            model.activeColor
    in
    model
        |> replenishHand
        |> setTurnToNeedCard
        |> WhatIf.debugWhatIf


updateSeed : Time.Posix -> Model -> Model
updateSeed time model =
    let
        seed =
            seedFromTime time
    in
    { model
        | seed = seed
    }


makeReady : Model -> Model
makeReady model =
    { model
        | state = Ready
    }


handleStartLocClick : PieceLocation -> Model -> Model
handleStartLocClick location model =
    let
        activeColor =
            model.activeColor

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
            model.activeColor

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
                |> moveToEndLoc activePlayer startLoc endLoc

        _ ->
            -- something is wrong with our click handlers
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- for VIEW, see View.elm
