module Polygon exposing
    ( get_full_height
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



{--

TODO:

I am not completely happy with the API here yet.  I don't want the callers
to be concerned with get_full_height.  Instead, I want to let them always
work in the same coordinate space, and I'll translate the SVG pieces as
necessary away from the origin, rather than having my app views do their
own arithmetic.  I also want to explain better what I'm doing here in
general, haha.

--}


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


get_full_height : Int -> Float -> Float -> Float
get_full_height side_count panel_width panel_height =
    panel_height + incircle_radius side_count panel_width


make_polygon : Float -> Float -> List (Svg.Svg msg) -> Svg.Svg msg
make_polygon panel_width panel_height panels =
    let
        side_count =
            List.length panels

        arrange_one_panel idx panel =
            let
                angle =
                    toFloat idx * get_angle side_count

                full_height =
                    get_full_height side_count panel_width panel_height

                center =
                    String.fromFloat full_height

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
                |> List.indexedMap arrange_one_panel
    in
    g [] new_panels
