module Board
    exposing
        ( board_view
        , board_rotate_button
        , rotate_board
        )

import Html exposing (..)
import Html.Events exposing (
    onClick
    )
import Msg exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Type exposing
    ( Zone
    , Color
    , SquareKey
    )
import Config exposing
    ( gutter_size
    , square_size
    , config_squares
    )
import Square exposing
    ( square_view
    )
import Piece exposing
    ( PieceDict
    )

rotate_board : List Color -> List Color
rotate_board zones =
    (List.drop 1 zones) ++ (List.take 1 zones)

-- VIEW

board_size: String
board_size = String.fromFloat (square_size * 16)

zone_height: Float
zone_height = 7 * square_size

board_view: PieceDict -> List Color -> Maybe SquareKey -> Html Msg
board_view piece_map zone_colors active_square =
    let
        content = List.map (draw_zone piece_map active_square zone_colors) zone_colors
    in
        svg
            [ width board_size, height board_size]
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
                1 + (zone_index x rest)

draw_zone: PieceDict -> Maybe SquareKey -> List Color -> Color -> Html Msg
draw_zone piece_map active_square zone_colors zone_color =
    let
        squares = config_squares

        idx = zone_index zone_color zone_colors

        angle = (toFloat idx) * 360.0 / (toFloat (List.length zone_colors))
        color = zone_color

        center = String.fromFloat (zone_height + 30)

        translate = "translate(" ++ center ++ " " ++ center ++ ")"
        rotate = "rotate(" ++ (String.fromFloat angle) ++ ")"
        transform_ = translate ++ " " ++ rotate

        drawn_squares = List.map (square_view zone_height piece_map color active_square) squares

    in
        g [transform transform_] drawn_squares

board_rotate_button : Html Msg
board_rotate_button =
    div
        []
        [ hr [] []
        , button
            [ onClick RotateBoard ]
            [ Html.text "Rotate" ]
        ]
