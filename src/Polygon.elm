module Polygon exposing
    ( get_center_offset
    , make_polygon
    )

import Svg
    exposing
        ( g
        )
import Svg.Attributes
    exposing
        ( transform
        )


get_angle : Int -> Float
get_angle side_count =
    360.0 / toFloat side_count


incircle_radius : Int -> Float -> Float
incircle_radius side_count panel_width =
    let
        angle =
            180.0
                / toFloat side_count
                |> degrees

        half_side =
            panel_width / 2
    in
    half_side / tan angle


get_center_offset : Int -> Float -> Float -> Float
get_center_offset side_count panel_width panel_height =
    panel_height + incircle_radius side_count panel_width


make_polygon : Float -> Float -> List (Svg.Svg msg) -> Svg.Svg msg
make_polygon panel_width panel_height panels =
    {--
        This function takes N svg elements (which I call panels) and
        arrange them in a sort of circular fashion so that the edges
        of the panels form a polygon of N sides around some center
        point.  Based on the width of the panels, we align the
        corners automatically.

        The nice thing is that you can draw the original panels
        without thinking about the eventual rotation.

        We expect the (0, 0) coordinate to be on the middle of
        the side of the eventual polygon.
    --}
    let
        side_count =
            List.length panels

        pad_one_panel panel =
            let
                offset =
                    incircle_radius side_count panel_width

                translate =
                    "translate(0 " ++ String.fromFloat offset ++ ")"
            in
            g [ transform translate ] [ panel ]

        arrange_one_panel idx panel =
            let
                angle =
                    toFloat idx * get_angle side_count

                center_offset =
                    get_center_offset side_count panel_width panel_height

                center =
                    String.fromFloat center_offset

                translate =
                    "translate(" ++ center ++ " " ++ center ++ ")"

                rotate =
                    "rotate(" ++ String.fromFloat angle ++ ")"

                transform_ =
                    translate ++ " " ++ rotate
            in
            g [ transform transform_ ] [ panel ]

        new_panels =
            panels
                |> List.map pad_one_panel
                |> List.indexedMap arrange_one_panel
    in
    g [] new_panels
