module Piece
    exposing
        ( config_pieces
        , get_piece
        , assign_piece
        , unassign_piece
        , maybe_send_piece_to_pen
        )

import List.Extra
import Dict
import Type
    exposing
        ( SquareKey
        , Color
        , PieceLocation
        , PieceDict
        )
import Config
    exposing
        ( holding_pen_squares
        )


type alias PieceConfig =
    { zone_color : Color
    , color : Color
    , id : String
    }


get_piece : PieceDict -> PieceLocation -> Maybe String
get_piece piece_map piece_loc =
    Dict.get piece_loc piece_map


is_open_square : PieceDict -> PieceLocation -> Bool
is_open_square piece_map piece_loc =
    case
        get_piece piece_map piece_loc
    of
        Nothing ->
            True

        Just _ ->
            False


open_holding_pen_square : PieceDict -> Color -> Maybe PieceConfig
open_holding_pen_square piece_map color =
    let
        is_open id =
            is_open_square piece_map (color, id)
    in
        case List.Extra.find is_open holding_pen_squares of
            Nothing ->
                Nothing

            Just id ->
                Just
                    { zone_color = color
                    , color = color
                    , id = id
                    }


maybe_send_piece_to_pen : PieceConfig -> PieceDict -> PieceDict
maybe_send_piece_to_pen config piece_map =
    let
        color =
            get_piece piece_map (config.zone_color, config.id)
    in
        case color of
            Nothing ->
                -- we are not landing on a piece, so do nothing
                piece_map

            Just color_ ->
                case open_holding_pen_square piece_map color_ of
                    Nothing ->
                        -- this should never happen!
                        piece_map

                    Just new_config ->
                        -- assign piece we "killed" to the
                        -- color's holding pen
                        assign_piece new_config piece_map


config_zone_pieces : String -> PieceDict -> PieceDict
config_zone_pieces color_ dct =
    let
        assign id_ =
            assign_piece { zone_color = color_, color = color_, id = id_ }
    in
        List.foldr assign dct holding_pen_squares


config_pieces : List Color -> PieceDict
config_pieces zone_colors =
    let
        dct =
            Dict.empty
    in
        List.foldl config_zone_pieces dct zone_colors


unassign_piece : SquareKey -> PieceDict -> PieceDict
unassign_piece square_key piece_map =
    let
        color =
            square_key.zone_color
        id =
            square_key.id
    in
        Dict.remove (color, id) piece_map


assign_piece : PieceConfig -> PieceDict -> PieceDict
assign_piece config piece_map =
    let
        zone_color =
            config.zone_color

        id =
            config.id
    in
        Dict.insert (zone_color, id) config.color piece_map
