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
        , ensureHandNotEmpty
        , finishMove
        , getPlayer
        , getPlayerMoveType
        , updateActivePlayer
        )
import Set
import Type
    exposing
        ( Game
        , Move
        , MoveType(..)
        , PieceLocation
        , Player
        )


performMove : Move -> Game -> Game
performMove move game =
    let
        activeColor =
            game.activeColor

        ( _, startLoc, _ ) =
            move

        pieceMap =
            game.pieceMap

        pieceColor =
            getPiece pieceMap startLoc
    in
    case pieceColor of
        Nothing ->
            game

        Just _ ->
            let
                zoneColors =
                    game.zoneColors

                newPieceMap =
                    pieceMap
                        |> movePiece move

                game_ =
                    { game
                        | pieceMap = newPieceMap
                    }
            in
            game_
                |> ensureHandNotEmpty
                |> updateActivePlayer (finishMove newPieceMap zoneColors activeColor move)


maybeAutoMove : PieceLocation -> Game -> Game
maybeAutoMove startLoc game =
    let
        players =
            game.players

        activeColor =
            game.activeColor

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
            -- game (and we have UI to let the user pick the location).
            game

        Just endLoc ->
            game
                |> moveToEndLoc activePlayer startLoc endLoc


maybeGetOutViaDiscard : Game -> Game
maybeGetOutViaDiscard game =
    let
        activeColor =
            game.activeColor

        player =
            getPlayer game.players activeColor
    in
    if player.getOutCredits < numCreditsToGetOut then
        game

    else
        let
            newPieceMap =
                game.pieceMap
                    |> bringPlayerOut activeColor
        in
        { game
            | pieceMap = newPieceMap
        }
            |> updateActivePlayer clearCredits


moveToEndLoc : Player -> PieceLocation -> PieceLocation -> Game -> Game
moveToEndLoc activePlayer startLoc endLoc game =
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
            game
                |> performMove move

        Nothing ->
            -- this is actually a programming error
            game
