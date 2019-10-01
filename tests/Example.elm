module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import LegalMove
    exposing
        ( FindLocParams
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
