module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import LegalMove
    exposing
        ( FindLocParams
        , get_can_go_n_spaces
        , has_piece_on_fast_track
        , next_zone_color
        , prev_zone_color
        , reachable_locs
        )
import Set
import Test exposing (..)
import Type
    exposing
        ( Card
        , PieceDict
        , PieceLocation
        )


test_zone_colors : Test
test_zone_colors =
    Test.concat
        [ test "can find prev colors" <|
            \_ ->
                prev_zone_color "green" [ "blue", "red", "green" ]
                    |> Expect.equal "red"
        , test "can find next colors" <|
            \_ ->
                next_zone_color "green" [ "blue", "red", "green" ]
                    |> Expect.equal "blue"
        ]


get_params : Int -> Card -> PieceDict -> PieceLocation -> FindLocParams
get_params moves_left active_card piece_map loc =
    { reverse_mode = False
    , can_fast_track = False
    , moves_left = moves_left
    , loc = loc
    , active_card = active_card
    , piece_color = "blue"
    , piece_map = piece_map
    , zone_colors = [ "red", "blue", "green" ]
    }


test_reachable_locs : Test
test_reachable_locs =
    Test.concat
        [ test "can move 8" <|
            \_ ->
                let
                    piece_map =
                        Dict.empty

                    params =
                        get_params 8 "8" piece_map ( "red", "L1" )

                    expected =
                        Set.fromList [ ( "blue", "R1" ) ]
                in
                reachable_locs params
                    |> Expect.equal expected
        , test "can't jump own piece" <|
            \_ ->
                let
                    piece_map =
                        Dict.empty
                            |> Dict.insert ( "blue", "R3" ) "blue"

                    params =
                        get_params 8 "8" piece_map ( "red", "L1" )

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

                    zone_colors =
                        [ "red", "blue", "green" ]

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 7
                in
                can_go |> Expect.equal True
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

                    zone_colors =
                        [ "red", "blue", "green" ]

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 2
                in
                can_go |> Expect.equal False
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

                    zone_colors =
                        [ "red", "blue", "green" ]

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

                    zone_colors =
                        [ "red", "blue", "green" ]

                    can_go =
                        get_can_go_n_spaces piece_map loc zone_colors 5
                in
                can_go |> Expect.equal False
        ]
