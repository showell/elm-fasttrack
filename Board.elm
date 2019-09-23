module Board
    exposing
        ( board_view
        )

import Html exposing (..)
import Msg exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Type exposing
    ( Zone
    , SquareKey
    )
import Config exposing
    ( gutter_size
    , square_size
    )
import Square exposing
    ( square_view
    )
import Piece exposing
    ( PieceDict
    )

board_size: String
board_size = toString (square_size * 16)

zone_height: Float
zone_height = 7 * square_size

board_view: PieceDict -> List Zone -> Maybe SquareKey -> Html Msg
board_view piece_map zones active_square =
    let
        content = List.map (draw_zone piece_map active_square) zones
    in
        svg
            [ width board_size, height board_size]
            content

draw_zone: PieceDict -> Maybe SquareKey -> Zone -> Html Msg
draw_zone piece_map active_square zone =
    let
        squares = zone.squares
        angle = zone.angle
        color = zone.color

        center = toString (zone_height + 30)

        translate = "translate(" ++ center ++ " " ++ center ++ ")"
        rotate = "rotate(" ++ (toString angle) ++ ")"
        transform_ = translate ++ " " ++ rotate

        drawn_squares = List.map (square_view zone_height piece_map color active_square) squares

    in
        g [transform transform_] drawn_squares

