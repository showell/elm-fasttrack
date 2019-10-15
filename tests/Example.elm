module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import LegalMove
    exposing
        ( endLocations
        , getCanGoNSpaces
        , getMovesForCards
        , getMovesForMoveType
        , getMovesFromLocation
        , nextZoneColor
        , prevZoneColor
        )
import Piece
    exposing
        ( hasPieceOnFastTrack
        , movablePieces
        , otherNonPenPieces
        , swappableLocs
        )
import Set
import Test exposing (..)
import Type
    exposing
        ( Card
        , Color
        , FindLocParams
        , MoveType(..)
        , PieceDict
        , PieceLocation
        )


zoneColors : List Color
zoneColors =
    [ "red", "blue", "green" ]


getParams : PieceDict -> FindLocParams
getParams pieceMap =
    { reverseMode = False
    , canFastTrack = False
    , canLeavePen = False
    , canLeaveBullsEye = False
    , pieceColor = "blue"
    , pieceMap = pieceMap
    , zoneColors = zoneColors
    }



-- These fixers are partly a symptom of Elm not allowing union types in
-- sets, but they also remove a bit of noise from comparisons.


fixMoveType moveType =
    case moveType of
        WithCard card ->
            card

        Reverse card ->
            "R" ++ card

        StartSplit count ->
            "SS" ++ String.fromInt count

        FinishSplit count _ ->
            "FS" ++ String.fromInt count

        _ ->
            "other"


fixMove ( moveType, start, end ) =
    ( fixMoveType moveType, start, end )


fixMoves moves =
    moves
        |> List.map fixMove
        |> Set.fromList


testZoneColors : Test
testZoneColors =
    Test.concat
        [ test "can find prev colors" <|
            \_ ->
                prevZoneColor "green" zoneColors
                    |> Expect.equal "blue"
        , test "can find next colors" <|
            \_ ->
                nextZoneColor "green" zoneColors
                    |> Expect.equal "red"
        ]


testGetMovesForCards : Test
testGetMovesForCards =
    Test.concat
        [ test "get moves 2/3 away" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "red", "L0" ) activeColor
                            |> Dict.insert ( "green", "R3" ) activeColor
                            |> Dict.insert ( "blue", "L3" ) activeColor
                            |> Dict.insert ( "blue", "L0" ) activeColor
                            |> Dict.insert ( "green", "L3" ) "green"

                    cards =
                        Set.fromList [ "2", "3" ]

                    moves =
                        getMovesForCards cards pieceMap zoneColors activeColor
                            |> fixMoves

                    expected =
                        Set.fromList
                            [ ( "2", ( "red", "L0" ), ( "red", "L2" ) )
                            , ( "2", ( "green", "R3" ), ( "green", "R1" ) )
                            , ( "2", ( "blue", "L3" ), ( "blue", "FT" ) )
                            , ( "2", ( "blue", "L0" ), ( "blue", "L2" ) )
                            , ( "3", ( "red", "L0" ), ( "red", "L3" ) )
                            , ( "3", ( "blue", "L3" ), ( "green", "R4" ) )
                            , ( "3", ( "blue", "L3" ), ( "black", "bullseye" ) )
                            , ( "3", ( "green", "R3" ), ( "green", "R0" ) )
                            ]
                in
                moves |> Expect.equal expected
        , test "get forced reverse" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "blue", "B1" ) activeColor
                            |> Dict.insert ( "blue", "B2" ) activeColor
                            |> Dict.insert ( "blue", "R0" ) activeColor

                    cards =
                        Set.fromList [ "3", "8" ]

                    moves =
                        getMovesForCards cards pieceMap zoneColors activeColor
                            |> fixMoves

                    expected =
                        Set.fromList
                            [ ( "R3", ( "blue", "R0" ), ( "blue", "R3" ) )
                            , ( "R8", ( "blue", "R0" ), ( "red", "L2" ) )
                            ]
                in
                moves |> Expect.equal expected
        , test "reverse with seven" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "blue", "B3" ) activeColor
                            |> Dict.insert ( "blue", "B1" ) activeColor
                            |> Dict.insert ( "blue", "R0" ) activeColor

                    cards =
                        Set.fromList [ "7" ]

                    moves =
                        getMovesForCards cards pieceMap zoneColors activeColor
                            |> fixMoves

                    expected =
                        Set.fromList
                            [ ( "R7", ( "blue", "R0" ), ( "red", "L3" ) )
                            ]
                in
                moves |> Expect.equal expected
        , test "seven with FT edge case" <|
            \_ ->
                -- when splitting sevens, don't land your first piece on the fast
                -- track if you're trying to split
                let
                    activeColor =
                        "blue"

                    -- we can move 4, 6, or 7 with L0 on the 7
                    -- then we can move 1, 2, or 3 with B3
                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "blue", "B1" ) activeColor
                            |> Dict.insert ( "blue", "L0" ) activeColor

                    cards =
                        Set.fromList [ "7" ]

                    moves =
                        getMovesForCards cards pieceMap zoneColors activeColor
                            |> fixMoves

                    expected =
                        Set.fromList
                            [ ( "SS1", ( "blue", "B1" ), ( "blue", "B2" ) )
                            , ( "SS2", ( "blue", "B1" ), ( "blue", "B3" ) )
                            , ( "SS3", ( "blue", "B1" ), ( "blue", "B4" ) )
                            , ( "SS4", ( "blue", "L0" ), ( "blue", "L4" ) )
                            , ( "SS6", ( "blue", "L0" ), ( "green", "R4" ) )
                            , ( "7", ( "blue", "L0" ), ( "green", "R3" ) )
                            ]
                in
                moves |> Expect.equal expected
        ]


testGetMovesForMoveType : Test
testGetMovesForMoveType =
    Test.concat
        [ test "get locs 2 away" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "red", "L0" ) activeColor
                            |> Dict.insert ( "green", "R4" ) activeColor
                            |> Dict.insert ( "blue", "L3" ) activeColor
                            |> Dict.insert ( "green", "L3" ) "green"

                    moveType =
                        WithCard "2"

                    moves =
                        getMovesForMoveType moveType pieceMap zoneColors activeColor
                            |> fixMoves

                    expected =
                        Set.fromList
                            [ ( "2", ( "red", "L0" ), ( "red", "L2" ) )
                            , ( "2", ( "green", "R4" ), ( "green", "R2" ) )
                            , ( "2", ( "blue", "L3" ), ( "blue", "FT" ) )
                            ]
                in
                moves |> Expect.equal expected
        , test "finish split" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    excludeLoc =
                        ( "green", "R4" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert excludeLoc activeColor
                            |> Dict.insert ( "blue", "B1" ) activeColor
                            |> Dict.insert ( "green", "L2" ) activeColor
                            |> Dict.insert ( "red", "L2" ) "red"

                    moveType =
                        FinishSplit 3 excludeLoc

                    moves =
                        getMovesForMoveType moveType pieceMap zoneColors activeColor
                            |> fixMoves

                    expected =
                        Set.fromList
                            [ ( "FS3", ( "blue", "B1" ), ( "blue", "B4" ) )
                            , ( "FS3", ( "green", "L2" ), ( "green", "FT" ) )
                            ]
                in
                moves |> Expect.equal expected
        ]


testMovablePieces : Test
testMovablePieces =
    Test.concat
        [ test "find my pieces" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "red", "L0" ) activeColor
                            |> Dict.insert ( "red", "L2" ) "red"
                            |> Dict.insert ( "green", "FT" ) activeColor
                            |> Dict.insert ( "blue", "L3" ) activeColor
                            |> Dict.insert ( "blue", "L4" ) "green"
                            |> Dict.insert ( "blue", "HP1" ) activeColor
                            |> Dict.insert ( "blue", "HP2" ) activeColor
                            |> Dict.insert ( "blue", "B2" ) activeColor
                            |> Dict.insert ( "blue", "FT" ) "red"

                    locs =
                        movablePieces pieceMap activeColor

                    expected =
                        Set.fromList
                            [ ( "red", "L0" )
                            , ( "green", "FT" )
                            , ( "blue", "L3" )
                            , ( "blue", "HP2" )
                            , ( "blue", "B2" )
                            ]
                in
                locs |> Expect.equal expected
        ]


testOtherNonPenPieces : Test
testOtherNonPenPieces =
    Test.concat
        [ test "other pieces can be found" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    loc =
                        ( "red", "L1" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc activeColor
                            |> Dict.insert ( "green", "FT" ) activeColor
                            |> Dict.insert ( "blue", "L3" ) activeColor
                            |> Dict.insert ( "blue", "HP1" ) activeColor
                            |> Dict.insert ( "blue", "B2" ) activeColor
                            |> Dict.insert ( "blue", "FT" ) "red"

                    locs =
                        otherNonPenPieces pieceMap activeColor loc

                    expected =
                        Set.fromList
                            [ ( "green", "FT" )
                            , ( "blue", "L3" )
                            , ( "blue", "B2" )
                            ]
                in
                locs |> Expect.equal expected
        ]


testEndLocs : Test
testEndLocs =
    let
        toEndLocs moves =
            moves
                |> List.map (\( _, _, end ) -> end)
                |> Set.fromList
    in
    Test.concat
        [ test "can move 8" <|
            \_ ->
                let
                    pieceMap =
                        Dict.empty

                    params =
                        getParams pieceMap

                    loc =
                        ( "red", "L1" )

                    expected =
                        Set.fromList [ ( "blue", "R1" ) ]
                in
                endLocations params loc 8
                    |> Set.fromList
                    |> Expect.equal expected
        , test "seven full" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    loc =
                        ( "blue", "L2" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc activeColor

                    moveType =
                        WithCard "7"

                    locs =
                        getMovesFromLocation moveType pieceMap zoneColors loc
                            |> toEndLocs

                    expected =
                        Set.fromList
                            [ ( "green", "R1" )
                            ]
                in
                locs |> Expect.equal expected
        , test "seven split" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    loc =
                        ( "blue", "B1" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "blue", "R1" ) activeColor
                            |> Dict.insert loc activeColor

                    moveType =
                        WithCard "7"

                    locs =
                        getMovesFromLocation moveType pieceMap zoneColors loc
                            |> toEndLocs

                    expected =
                        Set.fromList
                            [ ( "blue", "B3" )
                            , ( "blue", "B4" )
                            ]
                in
                locs |> Expect.equal expected
        , test "can only move FT piece" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    loc =
                        ( "red", "L1" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "green", "FT" ) activeColor
                            |> Dict.insert loc activeColor

                    moveType =
                        WithCard "8"

                    locs =
                        getMovesFromLocation moveType pieceMap zoneColors loc
                            |> toEndLocs

                    expected =
                        Set.empty
                in
                locs |> Expect.equal expected
        , test "can't jump own piece" <|
            \_ ->
                let
                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "blue", "R3" ) "blue"

                    params =
                        getParams pieceMap

                    loc =
                        ( "red", "L1" )

                    expected =
                        Set.empty
                in
                endLocations params loc 8
                    |> Set.fromList
                    |> Expect.equal expected
        ]


testHasPieceOnFastTrack : Test
testHasPieceOnFastTrack =
    Test.concat
        [ test "has piece on fast track" <|
            \_ ->
                let
                    loc =
                        ( "red", "FT" )

                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"

                    hasPiece =
                        hasPieceOnFastTrack pieceMap activeColor
                in
                hasPiece |> Expect.equal True
        , test "piece on fast track is not mine" <|
            \_ ->
                let
                    loc =
                        ( "red", "FT" )

                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "green"

                    hasPiece =
                        hasPieceOnFastTrack pieceMap activeColor
                in
                hasPiece |> Expect.equal False
        , test "pieces are mine but not on fast track" <|
            \_ ->
                let
                    loc1 =
                        ( "blue", "L0" )

                    loc2 =
                        ( "green", "R4" )

                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc1 activeColor
                            |> Dict.insert loc2 activeColor

                    hasPiece =
                        hasPieceOnFastTrack pieceMap activeColor
                in
                hasPiece |> Expect.equal False
        ]


testSwappableLocs : Test
testSwappableLocs =
    Test.concat
        [ test "swappableLocs" <|
            \_ ->
                let
                    activeColor =
                        "blue"

                    pieceMap =
                        Dict.empty
                            |> Dict.insert ( "blue", "L0" ) activeColor
                            |> Dict.insert ( "green", "L1" ) activeColor
                            |> Dict.insert ( "green", "HP1" ) "green"
                            |> Dict.insert ( "red", "HP1" ) "red"
                            |> Dict.insert ( "red", "B1" ) "red"
                            |> Dict.insert ( "green", "L0" ) "green"
                            |> Dict.insert ( "blue", "L1" ) "green"
                            |> Dict.insert ( "red", "R3" ) "red"

                    swapLocs =
                        swappableLocs pieceMap activeColor

                    expected =
                        Set.fromList
                            [ ( "green", "L0" )
                            , ( "blue", "L1" )
                            , ( "red", "R3" )
                            ]
                in
                swapLocs |> Expect.equal expected
        ]


testCanGoNSpaces : Test
testCanGoNSpaces =
    Test.concat
        [ test "can move full 7 when clear" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"

                    canGo =
                        getCanGoNSpaces pieceMap loc zoneColors 7
                in
                canGo |> Expect.equal True
        , test "cannot move 1 when other piece on FT" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blockedLoc =
                        ( "green", "FT" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blockedLoc "blue"

                    canGo =
                        getCanGoNSpaces pieceMap loc zoneColors 1
                in
                canGo |> Expect.equal False
        , test "cannot move 2 when blocked" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blockedLoc =
                        ( "red", "L3" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blockedLoc "blue"

                    canGo =
                        getCanGoNSpaces pieceMap loc zoneColors 2
                in
                canGo |> Expect.equal False
        , test "can move 4 into base" <|
            \_ ->
                let
                    loc =
                        ( "blue", "DS" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"

                    canGo =
                        getCanGoNSpaces pieceMap loc zoneColors 4
                in
                canGo |> Expect.equal True
        , test "can move 4 when partially blocked" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blockedLoc =
                        ( "blue", "R4" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blockedLoc "blue"

                    canGo =
                        getCanGoNSpaces pieceMap loc zoneColors 4
                in
                canGo |> Expect.equal True
        , test "cannot move 5 when blocked" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blockedLoc =
                        ( "blue", "R4" )

                    pieceMap =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blockedLoc "blue"

                    canGo =
                        getCanGoNSpaces pieceMap loc zoneColors 5
                in
                canGo |> Expect.equal False
        ]
