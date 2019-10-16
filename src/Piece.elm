module Piece exposing
    ( bringPlayerOut
    , configPieces
    , fakeLocation
    , getPiece
    , getThePiece
    , hasPieceOnFastTrack
    , isNormalLoc
    , movablePieces
    , movePiece
    , otherNonPenPieces
    , swappableLocs
    )

import Config
    exposing
        ( holdingPenLocations
        , isBaseId
        , isHoldingPenId
        )
import Dict
import List.Extra
import Set
import Setup
    exposing
        ( startingLocations
        )
import Type
    exposing
        ( Color
        , Move
        , MoveFlavor(..)
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


pieceToMoveOutOfPen : PieceDict -> Color -> Maybe PieceLocation
pieceToMoveOutOfPen pieceMap color =
    let
        isOccupied id =
            getPiece pieceMap ( color, id ) /= Nothing
    in
    holdingPenLocations
        |> List.filter isOccupied
        |> List.reverse
        |> List.head
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


isColor : PieceDict -> Color -> PieceLocation -> Bool
isColor pieceMap color loc =
    let
        locColor =
            Dict.get loc pieceMap
    in
    locColor == Just color


isNormalLoc : PieceLocation -> Bool
isNormalLoc ( _, id ) =
    not (isHoldingPenId id) && not (isBaseId id)


swappableLocs : PieceDict -> Color -> Set.Set PieceLocation
swappableLocs pieceMap activeColor =
    let
        isThem loc =
            not (isColor pieceMap activeColor loc)
    in
    Dict.keys pieceMap
        |> List.filter isThem
        |> List.filter isNormalLoc
        |> Set.fromList


myPieces : PieceDict -> Color -> Set.Set PieceLocation
myPieces pieceMap activeColor =
    Dict.keys pieceMap
        |> List.filter (isColor pieceMap activeColor)
        |> Set.fromList


movablePieces : PieceDict -> Color -> Set.Set PieceLocation
movablePieces pieceMap color =
    -- This excludes redundant pieces from the holding pen.
    let
        pieces =
            nonPenPieces pieceMap color
    in
    case pieceToMoveOutOfPen pieceMap color of
        Nothing ->
            pieces

        Just pen_loc ->
            Set.insert pen_loc pieces


nonPenPieces : PieceDict -> Color -> Set.Set PieceLocation
nonPenPieces pieceMap activeColor =
    let
        isMobile ( _, id ) =
            not (isHoldingPenId id)
    in
    myPieces pieceMap activeColor
        |> Set.filter isMobile


otherNonPenPieces : PieceDict -> Color -> PieceLocation -> Set.Set PieceLocation
otherNonPenPieces pieceMap activeColor loc =
    nonPenPieces pieceMap activeColor
        |> Set.remove loc


hasPieceOnFastTrack : PieceDict -> Color -> Bool
hasPieceOnFastTrack pieceMap activeColor =
    let
        isFt ( _, id ) =
            id == "FT"
    in
    myPieces pieceMap activeColor
        |> Set.toList
        |> List.any isFt


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
            pieceToMoveOutOfPen pieceMap color
    in
    case penLoc of
        Nothing ->
            -- probably a bug
            pieceMap

        Just startLoc ->
            let
                endLoc =
                    ( color, "L0" )
            in
            pieceMap
                |> executeMove RegularMove startLoc endLoc


movePiece : Move -> PieceDict -> PieceDict
movePiece move pieceMap =
    let
        ( moveType, startLoc, endLoc ) =
            move

        moveFlavor =
            case moveType of
                JackTrade ->
                    TradePieces

                _ ->
                    RegularMove
    in
    pieceMap
        |> executeMove moveFlavor startLoc endLoc


executeMove : MoveFlavor -> PieceLocation -> PieceLocation -> PieceDict -> PieceDict
executeMove moveFlavor startLoc endLoc pieceMap =
    let
        startColor =
            getThePiece pieceMap startLoc

        maybeEndColor =
            getPiece pieceMap endLoc
    in
    case maybeEndColor of
        Just endColor ->
            if moveFlavor == TradePieces then
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
