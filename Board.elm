module Board exposing
    ( board_view
    , rotate_board
    )

import Config
    exposing
        ( config_locations
        , gutter_size
        , square_size
        )
import Html exposing (..)
import Html.Events
    exposing
        ( onClick
        )
import Location
    exposing
        ( location_view
        )
import Msg exposing (..)
import Piece
    exposing
        ( player_pieces
        )
import Player
    exposing
        ( get_active_location
        , get_player
        , reachable_locs_for_player
        , ready_to_play
        )
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Type
    exposing
        ( Color
        , PieceDict
        , PieceLocation
        , PlayerDict
        )


rotate_board : List Color -> List Color
rotate_board zones =
    List.drop 1 zones ++ List.take 1 zones



-- VIEW


board_view : PieceDict -> List Color -> PlayerDict -> Color -> Html Msg
board_view piece_map zone_colors players active_color =
    let
        active_player =
            get_player players active_color

        active_location =
            get_active_location active_player

        playable_locs =
            if ready_to_play active_player then
                player_pieces piece_map active_color

            else
                Set.empty

        reachable_locs =
            reachable_locs_for_player active_player piece_map zone_colors

        content =
            List.map (draw_zone piece_map playable_locs reachable_locs active_location zone_colors) zone_colors

        board_size =
            String.fromFloat (2 * get_zone_height zone_colors + 3 * square_size)
    in
    svg
        [ width board_size, height board_size ]
        content


zone_index : Color -> List Color -> Int
zone_index x lst =
    case lst of
        [] ->
            -- should never happen, just appease compiler
            -1

        first :: rest ->
            if first == x then
                0

            else
                1 + zone_index x rest


get_angle : List Color -> Float
get_angle zone_colors =
    360.0 / toFloat (List.length zone_colors)


get_zone_height : List Color -> Float
get_zone_height zone_colors =
    let
        angle =
            get_angle zone_colors

        half_angle =
            angle / 2 |> degrees

        num_squares =
            5 + (2.0 / tan half_angle)
    in
    num_squares * square_size


draw_zone : PieceDict -> Set.Set PieceLocation -> Set.Set PieceLocation -> Maybe PieceLocation -> List Color -> Color -> Html Msg
draw_zone piece_map playable_locs reachable_locs active_location zone_colors zone_color =
    let
        locations =
            config_locations

        idx =
            zone_index zone_color zone_colors

        angle =
            toFloat idx * get_angle zone_colors

        zone_height =
            get_zone_height zone_colors

        color =
            zone_color

        center =
            String.fromFloat (zone_height + square_size)

        translate =
            "translate(" ++ center ++ " " ++ center ++ ")"

        rotate =
            "rotate(" ++ String.fromFloat angle ++ ")"

        transform_ =
            translate ++ " " ++ rotate

        drawn_locations =
            List.map (location_view zone_height piece_map color playable_locs reachable_locs active_location) locations
    in
    g [ transform transform_ ] drawn_locations
