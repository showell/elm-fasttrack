module WhatIf exposing
    ( debugWhatIf
    , getWhatIfGameStates
    )

import Config
    exposing
        ( isMoveAgainCard
        , numCreditsToGetOut
        )
import Debug
import Graph
    exposing
        ( getFinalStates
        )
import LegalMove
    exposing
        ( getMovesForCards
        , getMovesForMoveType
        )
import List.Extra
import Piece
    exposing
        ( bringPlayerOut
        , movePiece
        )
import Player
    exposing
        ( getPlayer
        )
import Set
import Type
    exposing
        ( Card
        , Color
        , Model
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        )


type NextStep
    = NormalPlay
    | DiscardCard
    | CoverCard
    | FinishSeven Int PieceLocation
    | Nada


type Action
    = DoMove Move
    | DoDiscard Card
    | DoCover Card


type alias GameState =
    { zoneColors : List Color
    , activeColor : Color
    , pieceMap : PieceDict
    , cards : List Card
    , credits : Int
    , nextStep : NextStep
    , actions : List Action
    }


getStateFromAction : GameState -> Action -> GameState
getStateFromAction priorState action =
    case action of
        DoMove move ->
            getStateFromMove priorState move

        DoDiscard card ->
            getStateFromDiscard priorState card

        DoCover card ->
            getStateFromCover priorState card


getStateFromDiscard : GameState -> Card -> GameState
getStateFromDiscard priorState card =
    let
        activeColor =
            priorState.activeColor

        pieceMap =
            priorState.pieceMap

        newActions =
            priorState.actions ++ [ DoDiscard card ]

        newCards =
            priorState.cards
                |> List.Extra.remove card

        credits =
            priorState.credits + 1

        gettingOut =
            credits >= numCreditsToGetOut

        newPieceMap =
            if gettingOut then
                pieceMap
                    |> bringPlayerOut activeColor

            else
                pieceMap

        newCredits =
            if gettingOut then
                0

            else
                credits

        nextStep =
            if isMoveAgainCard card then
                if gettingOut then
                    CoverCard

                else
                    DiscardCard

            else
                Nada
    in
    { priorState
        | cards = newCards
        , credits = newCredits
        , pieceMap = newPieceMap
        , actions = newActions
        , nextStep = nextStep
    }


getStateFromCover : GameState -> Card -> GameState
getStateFromCover priorState card =
    let
        newActions =
            priorState.actions ++ [ DoCover card ]

        newCards =
            priorState.cards
                |> List.Extra.remove card

        nextStep =
            if isMoveAgainCard card then
                CoverCard

            else
                Nada
    in
    { priorState
        | cards = newCards
        , nextStep = nextStep
        , actions = newActions
    }


getStateFromMove : GameState -> Move -> GameState
getStateFromMove priorState move =
    let
        ( moveType, _, endLoc ) =
            move

        newActions =
            priorState.actions ++ [ DoMove move ]

        newPieceMap =
            movePiece move priorState.pieceMap

        nextStep =
            case moveType of
                WithCard card ->
                    if isMoveAgainCard card then
                        NormalPlay

                    else
                        Nada

                Reverse card ->
                    if isMoveAgainCard card then
                        NormalPlay

                    else
                        Nada

                StartSplit count ->
                    FinishSeven (7 - count) endLoc

                FinishSplit _ _ ->
                    Nada

                JackTrade ->
                    NormalPlay

                ComeOutWithCredits ->
                    -- We aren't handling credits yet.
                    Nada

        maybeCard =
            case moveType of
                WithCard card ->
                    Just card

                Reverse card ->
                    Just card

                StartSplit _ ->
                    Nothing

                FinishSplit _ _ ->
                    Just "7"

                JackTrade ->
                    Just "J"

                ComeOutWithCredits ->
                    Nothing

        newCards =
            case maybeCard of
                Nothing ->
                    priorState.cards

                Just card ->
                    priorState.cards
                        |> List.Extra.remove card
    in
    { priorState
        | pieceMap = newPieceMap
        , cards = newCards
        , nextStep = nextStep
        , actions = newActions
        , credits = 0
    }


getNeighborStates : GameState -> List GameState
getNeighborStates state =
    let
        cards =
            state.cards
                |> Set.fromList

        discardActions =
            cards
                |> Set.toList
                |> List.map DoDiscard

        coverActions =
            cards
                |> Set.toList
                |> List.map DoCover

        pieceMap =
            state.pieceMap

        zoneColors =
            state.zoneColors

        activeColor =
            state.activeColor

        actions =
            case state.nextStep of
                Nada ->
                    []

                NormalPlay ->
                    let
                        moves =
                            getMovesForCards cards pieceMap zoneColors activeColor
                    in
                    if List.length moves > 0 then
                        moves
                            |> List.map DoMove

                    else if List.length state.actions == 0 then
                        discardActions

                    else
                        coverActions

                DiscardCard ->
                    discardActions

                CoverCard ->
                    coverActions

                FinishSeven count excludeLoc ->
                    let
                        moveType =
                            FinishSplit count excludeLoc

                        moves =
                            getMovesForMoveType moveType pieceMap zoneColors activeColor
                    in
                    moves
                        |> List.map DoMove
    in
    actions
        |> List.map (getStateFromAction state)


isFinalState : GameState -> Bool
isFinalState state =
    state.nextStep == Nada || List.length state.cards == 0


getWhatIfGameStates : Model -> List GameState
getWhatIfGameStates model =
    let
        zoneColors =
            model.zoneColors

        activeColor =
            model.activeColor

        pieceMap =
            model.pieceMap

        players =
            model.players

        activePlayer =
            getPlayer players activeColor

        cards =
            activePlayer.hand

        credits =
            activePlayer.getOutCredits

        nextStep =
            NormalPlay

        actions =
            []

        state =
            { zoneColors = zoneColors
            , activeColor = activeColor
            , pieceMap = pieceMap
            , cards = cards
            , credits = credits
            , nextStep = nextStep
            , actions = actions
            }
    in
    Graph.getFinalStates getNeighborStates isFinalState state


debugWhatIf : Model -> Model
debugWhatIf model =
    let
        whatIfs =
            getWhatIfGameStates model

        _ =
            whatIfs
                |> List.map (\w -> ( w.credits, w.actions, w.pieceMap ))
                |> List.map (Debug.log "what if...")
    in
    model
