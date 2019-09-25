module Square
    exposing
        ( square_view
        )

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Msg exposing (..)
import Config
    exposing
        ( square_size
        , gutter_size
        )
import Type
    exposing
        ( Square
        , PieceLocation
        , PieceDict
        )
import Piece
    exposing
        ( get_piece
        )


is_active_square : PieceLocation -> Maybe PieceLocation -> Bool
is_active_square square_info active_square =
    case active_square of
        Nothing ->
            False

        Just active ->
            square_info == active


square_view : Float -> PieceDict -> String -> Maybe PieceLocation -> Square -> Html Msg
square_view zone_height piece_map zone_color active_square square =
    let
        piece_location =
            ( zone_color, square.id )

        w =
            square_size - gutter_size

        h =
            square_size - gutter_size

        cx_ =
            square.x * square_size

        cy_ =
            zone_height - (square.y * square_size)

        xpos =
            cx_ - w / 2

        ypos =
            cy_ - h / 2

        my_piece =
            get_piece piece_map ( zone_color, square.id )

        my_pieces =
            case my_piece of
                Just piece_color ->
                    [ piece_color ]

                other ->
                    []

        is_active =
            is_active_square piece_location active_square

        draw_piece color_ =
            let
                radius =
                    if is_active then
                        "7"
                    else
                        "5"
            in
                circle
                    [ cx (String.fromFloat cx_)
                    , cy (String.fromFloat cy_)
                    , fill color_
                    , stroke color_
                    , r radius
                    , onClick (ClickSquare piece_location)
                    ]
                    []

        s_pieces =
            List.map draw_piece my_pieces

        fill_color =
            if is_active then
                "lightblue"
            else
                "white"

        s_square =
            rect
                [ x (String.fromFloat xpos)
                , y (String.fromFloat ypos)
                , fill fill_color
                , stroke zone_color
                , width (String.fromFloat w)
                , height (String.fromFloat h)
                , onClick (ClickSquare piece_location)
                ]
                []

        contents =
            List.concat
                [ [ s_square ]
                , s_pieces
                ]
    in
        g [] contents
