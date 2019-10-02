module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import LegalMove
    exposing
        ( FindLocParams
        , distance
        , get_can_go_n_spaces
        , get_reachable_locs
        , has_piece_on_fast_track
        , next_zone_color
        , other_mobile_pieces
        , prev_zone_color
        , reachable_locs
        )
import Set
import Test exposing (..)
import Type
    exposing
        ( Card
        , Color
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


test_reachable_locs : Test
test_reachable_locs =
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
                reachable_locs params
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
                        get_reachable_locs move_type piece_map zone_colors loc

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
                        get_reachable_locs move_type piece_map zone_colors loc

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
                        get_reachable_locs move_type piece_map zone_colors loc

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
                reachable_locs params
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
