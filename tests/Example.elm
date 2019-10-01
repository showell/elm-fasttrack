module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import LegalMove
    exposing
        ( next_zone_color
        , prev_zone_color
        , reachable_locs
        )
import Set
import Test exposing (..)


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


test_reachable_locs : Test
test_reachable_locs =
    Test.concat
        [ test "can move 8" <|
            \_ ->
                let
                    params =
                        { reverse_mode = False
                        , can_fast_track = False
                        , moves_left = 8
                        , loc = ( "red", "L1" )
                        , active_card = "8"
                        , piece_color = "blue"
                        , piece_map = Dict.empty
                        , zone_colors = [ "red", "blue", "green" ]
                        }

                    expected =
                        Set.fromList [ ( "blue", "R1" ) ]
                in
                reachable_locs params
                    |> Expect.equal expected
        ]
