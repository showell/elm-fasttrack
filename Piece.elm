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


get_piece : PieceDict -> Color -> String -> Maybe String
get_piece dct color id =
    case Dict.get color dct of
        Just sub_dict ->
            Dict.get id sub_dict

        other ->
            Nothing


is_open_square : PieceDict -> Color -> String -> Bool
is_open_square piece_map color id =
    case
        get_piece piece_map color id
    of
        Nothing ->
            True

        Just _ ->
            False


open_holding_pen_square : PieceDict -> Color -> Maybe PieceConfig
open_holding_pen_square piece_map color =
    let
        is_open =
            is_open_square piece_map color
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
            get_piece piece_map config.zone_color config.id
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
unassign_piece square_key dct =
    case Dict.get square_key.zone_color dct of
        Nothing ->
            -- this shouldn't happen if callers are
            -- already checking that a piece is currently
            -- assigned to this square
            dct

        Just sub_dict ->
            let
                new_sub_dict =
                    Dict.remove square_key.id sub_dict
            in
                Dict.insert square_key.zone_color new_sub_dict dct


assign_piece : PieceConfig -> PieceDict -> PieceDict
assign_piece config dct =
    let
        key =
            config.zone_color

        sub_key =
            config.id

        val =
            config.color

        maybe_sub_dict =
            Dict.get key dct

        sub_dict =
            case maybe_sub_dict of
                Just sd ->
                    sd

                Nothing ->
                    Dict.empty

        new_sub_dict =
            Dict.insert sub_key val sub_dict
    in
        Dict.insert key new_sub_dict dct
