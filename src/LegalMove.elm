module LegalMove exposing
    ( endLocations
    , getCanGoNSpaces
    , getCardForMoveType
    , getCardForPlayType
    , getMovesForCards
    , getMovesForMoveType
    , getMovesFromLocation
    , nextZoneColor
    , prevZoneColor
    )

import Config
    exposing
        ( isBaseId
        , isHoldingPenId
        , moveCountForCard
        , nextIdsInZone
        , prevIdInZone
        )
import Dict
import Graph
    exposing
        ( canTravelNEdges
        , getNodesNEdgesAway
        )
import List.Extra
import Piece
    exposing
        ( getPiece
        , getThePiece
        , hasPieceOnFastTrack
        , isNormalLoc
        , movablePieces
        , movePiece
        , otherNonPenPieces
        , swappableLocs
        )
import Set
import Type
    exposing
        ( Card
        , Color
        , FindLocParams
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Turn(..)
        )


nextZoneColor : Color -> List Color -> Color
nextZoneColor color zoneColors =
    let
        idx =
            List.Extra.elemIndex color zoneColors
                |> Maybe.withDefault -1

        len =
            List.length zoneColors

        nextIdx =
            (idx + 1) |> modBy len
    in
    List.Extra.getAt nextIdx zoneColors
        |> Maybe.withDefault "bogus"


prevZoneColor : Color -> List Color -> Color
prevZoneColor color zoneColors =
    let
        idx =
            List.Extra.elemIndex color zoneColors
                |> Maybe.withDefault 1

        len =
            List.length zoneColors

        nextIdx =
            (idx - 1) |> modBy len
    in
    List.Extra.getAt nextIdx zoneColors
        |> Maybe.withDefault "bogus"


getCanGoNSpaces : PieceDict -> PieceLocation -> List Color -> Int -> Bool
getCanGoNSpaces pieceMap loc zoneColors n =
    -- This function should only be called in the context of splitting
    -- sevens, so we don't account for cards being able to leave the
    -- holding pen.
    let
        ( _, id ) =
            loc

        canFastTrack =
            id == "FT"

        canLeaveBullsEye =
            False

        pieceColor =
            getThePiece pieceMap loc

        canMove =
            canFastTrack || not (hasPieceOnFastTrack pieceMap pieceColor)
    in
    if canMove then
        let
            params =
                { reverseMode = False
                , canFastTrack = canFastTrack
                , canLeavePen = False
                , canLeaveBullsEye = canLeaveBullsEye
                , pieceColor = pieceColor
                , pieceMap = pieceMap
                , zoneColors = zoneColors
                }

            getNeighbors =
                getNextLocs params
        in
        Graph.canTravelNEdges getNeighbors n loc

    else
        False


getMovesForCards : Set.Set Card -> PieceDict -> List Color -> Color -> List Move
getMovesForCards cards pieceMap zoneColors activeColor =
    let
        normalMoveType : Card -> MoveType
        normalMoveType card =
            if card == "4" then
                Reverse card

            else
                WithCard card

        f : (Card -> MoveType) -> List Move
        f makeMoveType =
            let
                getMoves : Card -> List Move
                getMoves card =
                    let
                        moveType =
                            makeMoveType card

                        moves =
                            getMovesForMoveType moveType pieceMap zoneColors activeColor
                    in
                    moves
            in
            cards
                |> Set.toList
                |> List.map getMoves
                |> List.concat

        forwardMoves =
            f normalMoveType
    in
    if List.length forwardMoves > 0 then
        forwardMoves

    else
        f Reverse


getMovesForMoveType : MoveType -> PieceDict -> List Color -> Color -> List Move
getMovesForMoveType moveType pieceMap zoneColors activeColor =
    let
        startLocs : Set.Set PieceLocation
        startLocs =
            case moveType of
                FinishSplit _ excludeLoc ->
                    otherNonPenPieces pieceMap activeColor excludeLoc

                _ ->
                    movablePieces pieceMap activeColor

        getMoves : PieceLocation -> List Move
        getMoves startLoc =
            getMovesFromLocation moveType pieceMap zoneColors startLoc
    in
    startLocs
        |> Set.toList
        |> List.map getMoves
        |> List.concat


getMovesFromLocation : MoveType -> PieceDict -> List Color -> PieceLocation -> List Move
getMovesFromLocation moveType pieceMap zoneColors startLoc =
    let
        ( _, id ) =
            startLoc

        canFastTrack =
            id == "FT"

        pieceColor =
            getThePiece pieceMap startLoc

        activeCard =
            getCardForMoveType moveType

        canLeaveBullsEye =
            List.member activeCard [ "J", "Q", "K" ] && id == "bullseye"

        canLeavePen =
            List.member activeCard [ "A", "joker", "6" ]

        reverseMode =
            case moveType of
                Reverse _ ->
                    True

                _ ->
                    activeCard == "4"

        movesLeft =
            moveCountForMoveType moveType id

        canMove =
            canFastTrack || not (hasPieceOnFastTrack pieceMap pieceColor)
    in
    if canMove then
        let
            params =
                { reverseMode = reverseMode
                , canFastTrack = canFastTrack
                , canLeavePen = canLeavePen
                , canLeaveBullsEye = canLeaveBullsEye
                , pieceColor = pieceColor
                , pieceMap = pieceMap
                , zoneColors = zoneColors
                }

            makeMove : PieceLocation -> Move
            makeMove endLoc =
                ( moveType, startLoc, endLoc )
        in
        if moveType == WithCard "7" then
            getMovesForSeven params startLoc

        else if moveType == WithCard "J" then
            getMovesForJack params startLoc

        else
            endLocations params startLoc movesLeft
                |> List.map makeMove

    else
        []


canFinishSplit : List Color -> Set.Set PieceLocation -> PieceDict -> Int -> Move -> Bool
canFinishSplit zoneColors otherLocs pieceMap count move =
    let
        modifiedPieceMap =
            movePiece move pieceMap

        canGo otherLoc =
            getCanGoNSpaces modifiedPieceMap otherLoc zoneColors count
    in
    otherLocs
        |> Set.toList
        |> List.any canGo


getMovesForJack : FindLocParams -> PieceLocation -> List Move
getMovesForJack params startLoc =
    let
        pieceMap =
            params.pieceMap

        pieceColor =
            getThePiece pieceMap startLoc

        movesLeft =
            1

        forwardMoves =
            endLocations params startLoc movesLeft
                |> List.map (\endLoc -> ( WithCard "J", startLoc, endLoc ))

        tradeMoves =
            if isNormalLoc startLoc then
                swappableLocs pieceMap pieceColor
                    |> Set.toList
                    |> List.map (\endLoc -> ( JackTrade, startLoc, endLoc ))

            else
                []
    in
    List.concat [ forwardMoves, tradeMoves ]


getMovesForSeven : FindLocParams -> PieceLocation -> List Move
getMovesForSeven params startLoc =
    let
        pieceMap =
            params.pieceMap

        zoneColors =
            params.zoneColors

        pieceColor =
            getThePiece pieceMap startLoc

        getLocs : Int -> List PieceLocation
        getLocs moveCount =
            endLocations params startLoc moveCount

        fullMoves =
            getLocs 7
                |> List.map (\endLoc -> ( WithCard "7", startLoc, endLoc ))

        otherLocs =
            otherNonPenPieces pieceMap pieceColor startLoc
    in
    if Set.size otherLocs == 0 then
        fullMoves

    else
        let
            getPartialMoves moveCount =
                let
                    otherCount =
                        7 - moveCount

                    moveType =
                        StartSplit moveCount

                    candidateMoves =
                        getLocs moveCount
                            |> List.filter (\endLoc -> endLoc /= ( "black", "bullseye" ))
                            |> List.map (\endLoc -> ( moveType, startLoc, endLoc ))
                in
                candidateMoves
                    |> List.filter (canFinishSplit zoneColors otherLocs pieceMap otherCount)

            partialMoves =
                List.range 1 6
                    |> List.map getPartialMoves
                    |> List.concat
        in
        partialMoves ++ fullMoves


endLocations : FindLocParams -> PieceLocation -> Int -> List PieceLocation
endLocations params startLoc movesLeft =
    let
        getNeighbors =
            if params.reverseMode then
                getPrevLocs params

            else
                getNextLocs params
    in
    Graph.getNodesNEdgesAway getNeighbors movesLeft startLoc


getNextLocs : FindLocParams -> PieceLocation -> List PieceLocation
getNextLocs params loc =
    let
        ( zoneColor, id ) =
            loc

        zoneColors =
            params.zoneColors

        nextColor =
            nextZoneColor zoneColor zoneColors

        pieceColor =
            params.pieceColor

        canFastTrack =
            params.canFastTrack

        canLeavePen =
            params.canLeavePen

        canLeaveBullsEye =
            params.canLeaveBullsEye

        pieceMap =
            params.pieceMap

        isFree loc_ =
            isLocFree pieceMap pieceColor loc_

        filter lst =
            lst
                |> List.filter isFree
    in
    if isHoldingPenId id then
        if canLeavePen then
            filter [ ( zoneColor, "L0" ) ]

        else
            []

    else if id == "FT" then
        if pieceColor == zoneColor then
            if canFastTrack then
                filter
                    [ ( nextColor, "FT" )
                    , ( nextColor, "R4" )
                    , ( "black", "bullseye" )
                    ]

            else
                filter
                    [ ( nextColor, "R4" )
                    , ( "black", "bullseye" )
                    ]

        else if canFastTrack && (pieceColor /= nextColor) then
            filter
                [ ( nextColor, "FT" )
                , ( nextColor, "R4" )
                ]

        else
            filter
                [ ( nextColor, "R4" )
                ]

    else if id == "bullseye" then
        if canLeaveBullsEye then
            let
                prevColor =
                    prevZoneColor pieceColor zoneColors
            in
            filter
                [ ( prevColor, "FT" )
                ]

        else
            []

    else
        let
            nextIds =
                nextIdsInZone id pieceColor zoneColor
        in
        List.map (\id_ -> ( zoneColor, id_ )) nextIds
            |> filter


getPrevLocs : FindLocParams -> PieceLocation -> List PieceLocation
getPrevLocs params loc =
    let
        ( zoneColor, id ) =
            loc

        zoneColors =
            params.zoneColors

        prevColor =
            prevZoneColor zoneColor zoneColors

        pieceColor =
            params.pieceColor

        pieceMap =
            params.pieceMap

        isFree loc_ =
            isLocFree pieceMap pieceColor loc_

        filter lst =
            lst
                |> List.filter isFree
    in
    if isHoldingPenId id then
        []

    else if isBaseId id then
        []

    else if id == "bullseye" then
        []

    else if id == "R4" then
        filter [ ( prevColor, "FT" ) ]

    else
        let
            prevId =
                prevIdInZone id
        in
        [ ( zoneColor, prevId ) ]
            |> filter


getCardForPlayType : PlayType -> Card
getCardForPlayType playType =
    case playType of
        PlayCard card ->
            card

        FinishSeven _ ->
            "7"


getCardForMoveType : MoveType -> Card
getCardForMoveType moveType =
    case moveType of
        WithCard card ->
            card

        Reverse card ->
            card

        StartSplit _ ->
            "7"

        FinishSplit _ _ ->
            "7"

        JackTrade ->
            "J"

        ComeOutWithCredits ->
            -- should never be called
            "via discards"


moveCountForMoveType : MoveType -> String -> Int
moveCountForMoveType moveType id =
    case moveType of
        WithCard card ->
            moveCountForCard card id

        Reverse card ->
            moveCountForCard card id

        StartSplit count ->
            count

        FinishSplit count _ ->
            count

        JackTrade ->
            -- we never call this for J trades
            0

        ComeOutWithCredits ->
            -- should never be called
            1


isLocFree : PieceDict -> Color -> PieceLocation -> Bool
isLocFree pieceMap pieceColor loc =
    let
        otherPiece =
            getPiece pieceMap loc
    in
    case otherPiece of
        Nothing ->
            True

        Just color ->
            color /= pieceColor
