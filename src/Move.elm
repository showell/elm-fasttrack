module Move exposing
    ( maybeAutoMove
    , maybeGetOutViaDiscard
    , moveToEndLoc
    )

import Config
    exposing
        ( numCreditsToGetOut
        )
import Piece
    exposing
        ( bringPlayerOut
        , getPiece
        , movePiece
        )
import Player
    exposing
        ( clearCredits
        , endLocsForPlayer
        , finishMove
        , getPlayer
        , getPlayerMoveType
        , maybeReplenishHand
        , updateActivePlayer
        )
import Set
import Type
    exposing
        ( Color
        , Model
        , Move
        , MoveType(..)
        , PieceLocation
        , Player
        )


performMove : Model -> Move -> Color -> Model
performMove model move activeColor =
    let
        ( _, startLoc, _ ) =
            move

        pieceMap =
            model.pieceMap

        pieceColor =
            getPiece pieceMap startLoc
    in
    case pieceColor of
        Nothing ->
            model

        Just _ ->
            let
                zoneColors =
                    model.zoneColors

                newPieceMap =
                    pieceMap
                        |> movePiece move

                model_ =
                    { model
                        | pieceMap = newPieceMap
                    }
            in
            model_
                |> maybeReplenishHand activeColor
                |> updateActivePlayer (finishMove newPieceMap zoneColors activeColor move)


maybeAutoMove : PieceLocation -> Model -> Model
maybeAutoMove startLoc model =
    let
        zoneColors =
            model.zoneColors

        players =
            model.players

        activeColor =
            model.getActiveColor zoneColors

        activePlayer =
            getPlayer players activeColor

        endLocs =
            endLocsForPlayer activePlayer

        uniqueEndLoc =
            if Set.size endLocs == 1 then
                endLocs
                    |> Set.toList
                    |> List.head

            else
                Nothing
    in
    case uniqueEndLoc of
        Nothing ->
            -- If we don't have exactly one location, don't update the
            -- model (and we have UI to let the user pick the location).
            model

        Just endLoc ->
            model
                |> moveToEndLoc activePlayer activeColor startLoc endLoc


maybeGetOutViaDiscard : Color -> Model -> Model
maybeGetOutViaDiscard activeColor model =
    let
        player =
            getPlayer model.players activeColor
    in
    if player.getOutCredits < numCreditsToGetOut then
        model

    else
        let
            newPieceMap =
                model.pieceMap
                    |> bringPlayerOut activeColor
        in
        { model
            | pieceMap = newPieceMap
        }
            |> updateActivePlayer clearCredits


moveToEndLoc : Player -> Color -> PieceLocation -> PieceLocation -> Model -> Model
moveToEndLoc activePlayer activeColor startLoc endLoc model =
    let
        moveType_ =
            getPlayerMoveType activePlayer endLoc
    in
    case moveType_ of
        Just moveType ->
            let
                move =
                    ( moveType, startLoc, endLoc )
            in
            performMove model move activeColor

        Nothing ->
            -- this is actually a programming error
            model
