module Polygon exposing
    ( getCenterOffset
    , makePolygon
    )

import Svg
    exposing
        ( g
        )
import Svg.Attributes
    exposing
        ( transform
        )


getAngle : Int -> Float
getAngle sideCount =
    360.0 / toFloat sideCount


incircleRadius : Int -> Float -> Float
incircleRadius sideCount panelWidth =
    let
        angle =
            180.0
                / toFloat sideCount
                |> degrees

        halfSide =
            panelWidth / 2
    in
    halfSide / tan angle


getCenterOffset : Int -> Float -> Float -> Float
getCenterOffset sideCount panelWidth panelHeight =
    panelHeight + incircleRadius sideCount panelWidth


makePolygon : Float -> Float -> List (Svg.Svg msg) -> Svg.Svg msg
makePolygon panelWidth panelHeight panels =
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
        sideCount =
            List.length panels

        padOnePanel panel =
            let
                offset =
                    incircleRadius sideCount panelWidth

                translate =
                    "translate(0 " ++ String.fromFloat offset ++ ")"
            in
            g [ transform translate ] [ panel ]

        arrangeOnePanel idx panel =
            let
                angle =
                    toFloat idx * getAngle sideCount

                centerOffset =
                    getCenterOffset sideCount panelWidth panelHeight

                center =
                    String.fromFloat centerOffset

                translate =
                    "translate(" ++ center ++ " " ++ center ++ ")"

                rotate =
                    "rotate(" ++ String.fromFloat angle ++ ")"

                transform_ =
                    translate ++ " " ++ rotate
            in
            g [ transform transform_ ] [ panel ]

        newPanels =
            panels
                |> List.map padOnePanel
                |> List.indexedMap arrangeOnePanel
    in
    g [] newPanels
