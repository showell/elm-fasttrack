module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import LegalMove
    exposing
        ( distance
        , end_locations
        , get_can_go_n_spaces
        , get_end_locs
        , get_moves_for_cards
        , get_moves_for_move_type
        , has_piece_on_fast_track
        , my_pieces
        , next_zone_color
        , other_mobile_pieces
        , prev_zone_color
        , swappable_locs
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


zone_colors : List Color
zone_colors =
    [ "red", "blue", "green" ]


test_zone_colors : Test
test_zone_colors =
    Test.concat
        [ test "can find prev colors" <|
            \_ ->
                prev_zone_color "green" zone_colors
                    |> Expect.equal "blue"
        , test "can find next colors" <|
            \_ ->
                next_zone_color "green" zone_colors
                    |> Expect.equal "red"
        ]


get_params : Int -> PieceDict -> PieceLocation -> FindLocParams
get_params moves_left piece_map loc =
    { reverse_mode = False
    , can_fast_track = False
    , can_leave_pen = False
    , moves_left = moves_left
    , loc = loc
    , piece_color = "blue"
    , piece_map = piece_map
    , zone_colors = zone_colors
    }

-- These fixers are partly a symptom of Elm not allowing union types in
-- sets, but they also remove a bit of noise from comparisons.

fix_move_type move_type =
    case move_type of
        WithCard card ->
            card

        Reverse card ->
            "R" ++ card

        FinishSplit count _ ->
            "FS" ++ String.fromInt count

        _ ->
            "other"

fix_move (move_type, start, end) =
    (fix_move_type move_type, start, end)

fix_moves moves =
    moves
        |> List.map fix_move
        |> Set.fromList

test_get_moves_for_cards : Test
test_get_moves_for_cards =
    Test.concat
        [ test "get moves 2/3 away" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "red", "L0" ) active_color
                            |> Dict.insert ( "green", "R3" ) active_color
                            |> Dict.insert ( "blue", "L3" ) active_color
                            |> Dict.insert ( "blue", "L0" ) active_color
                            |> Dict.insert ( "green", "L3" ) "green"

                    cards =
                        Set.fromList [ "2", "3" ]

                    moves =
                        get_moves_for_cards cards piece_map zone_colors active_color
                            |> fix_moves

                    expected =
                        Set.fromList
                            [ ( "2", ( "red", "L0" ), ( "red", "L2" ) )
                            , ( "2", ( "green", "R3" ), ( "green", "R1" ) )
                            , ( "2", ( "blue", "L3" ), ( "blue", "FT" ) )
                            , ( "2", ( "blue", "L0" ), ( "blue", "L2" ) )
                            , ( "3", ( "red", "L0" ), ( "red", "L3" ) )
                            , ( "3", ( "blue", "L3" ), ( "green", "R4" ) )
                            , ( "3", ( "green", "R3" ), ( "green", "R0" ) )
                            ]
                in
                moves |> Expect.equal expected
        , test "get forced reverse" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "B1" ) active_color
                            |> Dict.insert ( "blue", "B2" ) active_color
                            |> Dict.insert ( "blue", "R0" ) active_color

                    cards =
                        Set.fromList [ "3", "8" ]

                    moves =
                        get_moves_for_cards cards piece_map zone_colors active_color
                            |> fix_moves

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
                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "B3" ) active_color
                            |> Dict.insert ( "blue", "B1" ) active_color
                            |> Dict.insert ( "blue", "R0" ) active_color

                    cards =
                        Set.fromList [ "7" ]

                    moves =
                        get_moves_for_cards cards piece_map zone_colors active_color
                            |> fix_moves

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
                    active_color =
                        "blue"

                    -- we can move 4, 6, or 7 with L0 on the 7
                    -- then we can move 1, 2, or 3 with B3
                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "B1" ) active_color
                            |> Dict.insert ( "blue", "L0" ) active_color

                    cards =
                        Set.fromList [ "7" ]

                    moves =
                        get_moves_for_cards cards piece_map zone_colors active_color
                            |> fix_moves

                    expected =
                        Set.fromList
                            [ ( "7", ( "blue", "B1" ), ( "blue", "B2" ) )
                            , ( "7", ( "blue", "B1" ), ( "blue", "B3" ) )
                            , ( "7", ( "blue", "B1" ), ( "blue", "B4" ) )
                            , ( "7", ( "blue", "L0" ), ( "blue", "L4" ) )
                            , ( "7", ( "blue", "L0" ), ( "green", "R3" ) )
                            , ( "7", ( "blue", "L0" ), ( "green", "R4" ) )
                            ]
                in
                moves |> Expect.equal expected
        ]


test_get_moves_for_move_type : Test
test_get_moves_for_move_type =
    Test.concat
        [ test "get locs 2 away" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "red", "L0" ) active_color
                            |> Dict.insert ( "green", "R4" ) active_color
                            |> Dict.insert ( "blue", "L3" ) active_color
                            |> Dict.insert ( "green", "L3" ) "green"

                    move_type =
                        WithCard "2"

                    moves =
                        get_moves_for_move_type move_type piece_map zone_colors active_color
                            |> fix_moves

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
                    active_color =
                        "blue"

                    exclude_loc =
                        ( "green", "R4" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert exclude_loc active_color
                            |> Dict.insert ( "blue", "B1" ) active_color
                            |> Dict.insert ( "green", "L2" ) active_color
                            |> Dict.insert ( "red", "L2" ) "red"

                    move_type =
                        FinishSplit 3 exclude_loc

                    moves =
                        get_moves_for_move_type move_type piece_map zone_colors active_color
                            |> fix_moves

                    expected =
                        Set.fromList
                            [ ( "FS3", ( "blue", "B1" ), ( "blue", "B4" ) )
                            , ( "FS3", ( "green", "L2" ), ( "green", "FT" ) )
                            ]
                in
                moves |> Expect.equal expected
        ]


test_my_pieces : Test
test_my_pieces =
    Test.concat
        [ test "find my pieces" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "red", "L0" ) active_color
                            |> Dict.insert ( "red", "L2" ) "red"
                            |> Dict.insert ( "green", "FT" ) active_color
                            |> Dict.insert ( "blue", "L3" ) active_color
                            |> Dict.insert ( "blue", "L4" ) "green"
                            |> Dict.insert ( "blue", "HP1" ) active_color
                            |> Dict.insert ( "blue", "B2" ) active_color
                            |> Dict.insert ( "blue", "FT" ) "red"

                    locs =
                        my_pieces piece_map active_color

                    expected =
                        Set.fromList
                            [ ( "red", "L0" )
                            , ( "green", "FT" )
                            , ( "blue", "L3" )
                            , ( "blue", "HP1" )
                            , ( "blue", "B2" )
                            ]
                in
                locs |> Expect.equal expected
        ]


test_other_mobile_pieces : Test
test_other_mobile_pieces =
    Test.concat
        [ test "other pieces can be found" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    loc =
                        ( "red", "L1" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc active_color
                            |> Dict.insert ( "green", "FT" ) active_color
                            |> Dict.insert ( "blue", "L3" ) active_color
                            |> Dict.insert ( "blue", "HP1" ) active_color
                            |> Dict.insert ( "blue", "B2" ) active_color
                            |> Dict.insert ( "blue", "FT" ) "red"

                    locs =
                        other_mobile_pieces piece_map active_color loc

                    expected =
                        Set.fromList
                            [ ( "green", "FT" )
                            , ( "blue", "L3" )
                            , ( "blue", "B2" )
                            ]
                in
                locs |> Expect.equal expected
        ]


test_distance : Test
test_distance =
    Test.concat
        [ test "basic distance" <|
            \_ ->
                let
                    start_loc =
                        ( "blue", "L1" )

                    end_loc =
                        ( "blue", "L4" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 3
        , test "backward" <|
            \_ ->
                let
                    start_loc =
                        ( "blue", "R0" )

                    end_loc =
                        ( "blue", "R4" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 99
        , test "backward around corner" <|
            \_ ->
                let
                    start_loc =
                        ( "blue", "R3" )

                    end_loc =
                        ( "red", "DS" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 99
        , test "start fasttrack" <|
            \_ ->
                let
                    start_loc =
                        ( "blue", "FT" )

                    end_loc =
                        ( "blue", "R3" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 4
        , test "start holding pen" <|
            \_ ->
                let
                    start_loc =
                        ( "red", "HP3" )

                    end_loc =
                        ( "red", "L0" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 1
        , test "rounding corner" <|
            \_ ->
                let
                    start_loc =
                        ( "red", "L4" )

                    end_loc =
                        ( "blue", "R0" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 6
        , test "approaching base" <|
            \_ ->
                let
                    start_loc =
                        ( "blue", "BR" )

                    end_loc =
                        ( "blue", "DS" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 1
        , test "entering base" <|
            \_ ->
                let
                    start_loc =
                        ( "red", "L4" )

                    end_loc =
                        ( "blue", "B2" )
                in
                distance zone_colors "blue" start_loc end_loc
                    |> Expect.equal 10
        ]


test_end_locs : Test
test_end_locs =
    Test.concat
        [ test "can move 8" <|
            \_ ->
                let
                    piece_map =
                        Dict.empty

                    params =
                        get_params 8 piece_map ( "red", "L1" )

                    expected =
                        Set.fromList [ ( "blue", "R1" ) ]
                in
                end_locations params
                    |> Expect.equal expected
        , test "seven full" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    loc =
                        ( "blue", "L2" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc active_color

                    move_type =
                        WithCard "7"

                    locs =
                        get_end_locs move_type piece_map zone_colors loc

                    expected =
                        Set.fromList
                            [ ( "green", "R1" )
                            ]
                in
                locs |> Expect.equal expected
        , test "seven split" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    loc =
                        ( "blue", "B1" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "R1" ) active_color
                            |> Dict.insert loc active_color

                    move_type =
                        WithCard "7"

                    locs =
                        get_end_locs move_type piece_map zone_colors loc

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
                    active_color =
                        "blue"

                    loc =
                        ( "red", "L1" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "green", "FT" ) active_color
                            |> Dict.insert loc active_color

                    move_type =
                        WithCard "8"

                    locs =
                        get_end_locs move_type piece_map zone_colors loc

                    expected =
                        Set.empty
                in
                locs |> Expect.equal expected
        , test "can't jump own piece" <|
            \_ ->
                let
                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "R3" ) "blue"

                    params =
                        get_params 8 piece_map ( "red", "L1" )

                    expected =
                        Set.empty
                in
                end_locations params
                    |> Expect.equal expected
        ]


test_has_piece_on_fast_track : Test
test_has_piece_on_fast_track =
    Test.concat
        [ test "has piece on fast track" <|
            \_ ->
                let
                    loc =
                        ( "red", "FT" )

                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"

                    has_piece =
                        has_piece_on_fast_track piece_map active_color
                in
                has_piece |> Expect.equal True
        , test "piece on fast track is not mine" <|
            \_ ->
                let
                    loc =
                        ( "red", "FT" )

                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "green"

                    has_piece =
                        has_piece_on_fast_track piece_map active_color
                in
                has_piece |> Expect.equal False
        , test "pieces are mine but not on fast track" <|
            \_ ->
                let
                    loc1 =
                        ( "blue", "L0" )

                    loc2 =
                        ( "green", "R4" )

                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc1 active_color
                            |> Dict.insert loc2 active_color

                    has_piece =
                        has_piece_on_fast_track piece_map active_color
                in
                has_piece |> Expect.equal False
        ]


test_swappable_locs : Test
test_swappable_locs =
    Test.concat
        [ test "swappable_locs" <|
            \_ ->
                let
                    active_color =
                        "blue"

                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "L0" ) active_color
                            |> Dict.insert ( "green", "L1" ) active_color
                            |> Dict.insert ( "green", "HP1" ) "green"
                            |> Dict.insert ( "red", "HP1" ) "red"
                            |> Dict.insert ( "red", "B1" ) "red"
                            |> Dict.insert ( "green", "L0" ) "green"
                            |> Dict.insert ( "blue", "L1" ) "green"
                            |> Dict.insert ( "red", "R3" ) "red"

                    swap_locs =
                        swappable_locs piece_map active_color

                    expected =
                        Set.fromList
                            [ ( "green", "L0" )
                            , ( "blue", "L1" )
                            , ( "red", "R3" )
                            ]
                in
                swap_locs |> Expect.equal expected
        ]


test_can_go_n_spaces : Test
test_can_go_n_spaces =
    Test.concat
        [ test "can move full 7 when clear" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 7
                in
                can_go |> Expect.equal True
        , test "cannot move 1 when other piece on FT" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blocked_loc =
                        ( "green", "FT" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blocked_loc "blue"

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 1
                in
                can_go |> Expect.equal False
        , test "cannot move 2 when blocked" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blocked_loc =
                        ( "red", "L3" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blocked_loc "blue"

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 2
                in
                can_go |> Expect.equal False
        , test "can move 4 into base" <|
            \_ ->
                let
                    loc =
                        ( "blue", "DS" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 4
                in
                can_go |> Expect.equal True
        , test "can move 4 when partially blocked" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blocked_loc =
                        ( "blue", "R4" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blocked_loc "blue"

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 4
                in
                can_go |> Expect.equal True
        , test "cannot move 5 when blocked" <|
            \_ ->
                let
                    loc =
                        ( "red", "L1" )

                    blocked_loc =
                        ( "blue", "R4" )

                    piece_map =
                        Dict.empty
                            |> Dict.insert loc "blue"
                            |> Dict.insert blocked_loc "blue"

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 5
                in
                can_go |> Expect.equal False
        ]
