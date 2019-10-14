module Piece exposing
    ( bringPlayerOut
    , configPieces
    , fakeLocation
    , getPiece
    , getThePiece
    , movePiece
    )

import Config
    exposing
        ( holdingPenLocations
        )
import Dict
import List.Extra
import Setup
    exposing
        ( startingLocations
        )
import Type
    exposing
        ( Color
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        )


fakeLocation : PieceLocation
fakeLocation =
    ( "bogus", "bogus" )


getPiece : PieceDict -> PieceLocation -> Maybe String
getPiece pieceMap pieceLoc =
    Dict.get pieceLoc pieceMap


getThePiece : PieceDict -> PieceLocation -> String
getThePiece pieceMap pieceLoc =
    -- This should be called ONLY when we know there's
    -- a piece at the location.  Most use cases are when
    -- our callers already know the location has a valid
    -- piece, because they're iterating through some kind
    -- of list of moves that have already been validated.
    Dict.get pieceLoc pieceMap
        |> Maybe.withDefault "bogus"


isOpenLocation : PieceDict -> PieceLocation -> Bool
isOpenLocation pieceMap pieceLoc =
    case
        getPiece pieceMap pieceLoc
    of
        Nothing ->
            True

        Just _ ->
            False


occupiedHoldingPenLocation : PieceDict -> Color -> Maybe PieceLocation
occupiedHoldingPenLocation pieceMap color =
    let
        isOccupied id =
            getPiece pieceMap ( color, id ) /= Nothing
    in
    holdingPenLocations
        |> List.Extra.find isOccupied
        |> Maybe.map (\id -> ( color, id ))


openHoldingPenLocation : PieceDict -> Color -> PieceLocation
openHoldingPenLocation pieceMap color =
    -- We expect to only be called when we know we're sending a
    -- piece home, so there should always be a square.
    let
        isOpen id =
            isOpenLocation pieceMap ( color, id )
    in
    List.Extra.find isOpen holdingPenLocations
        |> Maybe.andThen (\id -> Just ( color, id ))
        |> Maybe.withDefault fakeLocation


configZonePieces : String -> PieceDict -> PieceDict
configZonePieces color pieceMap =
    let
        assign loc =
            Dict.insert loc color
    in
    List.foldl assign pieceMap (startingLocations color)


configPieces : List Color -> PieceDict
configPieces zoneColors =
    let
        dct =
            Dict.empty
    in
    List.foldl configZonePieces dct zoneColors


bringPlayerOut : Color -> PieceDict -> PieceDict
bringPlayerOut color pieceMap =
    let
        penLoc =
            occupiedHoldingPenLocation pieceMap color
    in
    case penLoc of
        Nothing ->
            -- probably a bug
            pieceMap

        Just startLoc ->
            let
                endLoc =
                    ( color, "L0" )

                moveType =
                    ComeOutWithCredits

                move =
                    ( moveType, startLoc, endLoc )
            in
            pieceMap
                |> movePiece move


movePiece : Move -> PieceDict -> PieceDict
movePiece move pieceMap =
    let
        ( moveType, startLoc, endLoc ) =
            move

        wantTrade =
            moveType == JackTrade

        startColor =
            getThePiece pieceMap startLoc

        maybeEndColor =
            getPiece pieceMap endLoc
    in
    case maybeEndColor of
        Just endColor ->
            if wantTrade then
                pieceMap
                    |> Dict.insert startLoc endColor
                    |> Dict.insert endLoc startColor

            else
                let
                    penLoc =
                        openHoldingPenLocation pieceMap endColor
                in
                pieceMap
                    |> Dict.insert penLoc endColor
                    |> Dict.remove startLoc
                    |> Dict.insert endLoc startColor

        _ ->
            pieceMap
                |> Dict.remove startLoc
                |> Dict.insert endLoc startColor
