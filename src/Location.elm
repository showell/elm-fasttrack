module Location exposing (location_view)

import Config
    exposing
        ( gutter_size
        , square_size
        )
import Html exposing (..)
import Msg exposing (..)
import Piece
    exposing
        ( get_piece
        , piece_view
        )
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Type
    exposing
        ( Location
        , PieceDict
        , PieceLocation
        )


is_active_location : PieceLocation -> Maybe PieceLocation -> Bool
is_active_location loc active_location =
    case active_location of
        Nothing ->
            False

        Just active ->
            loc == active


location_view : Float -> PieceDict -> String -> Set.Set PieceLocation -> Set.Set PieceLocation -> Maybe PieceLocation -> Location -> Html Msg
location_view zone_height piece_map zone_color playable_locs reachable_locs active_location location_info =
    let
        id =
            location_info.id

        piece_location =
            ( zone_color, id )

        w =
            square_size - gutter_size

        h =
            square_size - gutter_size

        radius =
            w / 2

        cx_ =
            location_info.x * square_size

        cy_ =
            zone_height - (location_info.y * square_size)

        xpos =
            cx_ - w / 2

        ypos =
            cy_ - h / 2

        my_piece =
            get_piece piece_map piece_location

        is_active =
            is_active_location piece_location active_location

        is_playable =
            Set.member piece_location playable_locs

        is_reachable =
            Set.member piece_location reachable_locs

        s_pieces =
            case my_piece of
                Just piece_color ->
                    [ piece_view piece_color is_active is_playable cx_ cy_ piece_location ]

                Nothing ->
                    []

        fill_color =
            if is_active then
                "lightblue"

            else if is_reachable then
                "lightgreen"

            else
                "white"

        stroke_color =
            if is_playable then
                "black"

            else
                zone_color

        is_rect =
            List.member id [ "HP1", "HP2", "HP3", "HP4", "B1", "B2", "B3", "B4" ]

        s_location =
            if is_rect then
                rect
                    [ x (String.fromFloat xpos)
                    , y (String.fromFloat ypos)
                    , fill fill_color
                    , stroke stroke_color
                    , width (String.fromFloat w)
                    , height (String.fromFloat h)
                    , onClick (ClickLocation piece_location)
                    , rx "2"
                    ]
                    []

            else
                circle
                    [ cx (String.fromFloat cx_)
                    , cy (String.fromFloat cy_)
                    , fill fill_color
                    , stroke stroke_color
                    , r (String.fromFloat radius)
                    , onClick (ClickLocation piece_location)
                    ]
                    []

        contents =
            [ s_location ] ++ s_pieces
    in
    g [] contents
