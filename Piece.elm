module Piece exposing
    ( PieceDict
    , config_pieces
    )

import Dict

import Config exposing
    ( zone_colors
    , holding_pen_squares
    )

type alias ZonePieceDict = Dict.Dict String String
type alias PieceDict = Dict.Dict String ZonePieceDict
type alias PieceConfig =
    { zone_color: String
    , color: String
    , id: String
    }

config_zone_pieces: String -> PieceDict -> PieceDict
config_zone_pieces color_ dct =
    let
        assign id_ dct =
            assign_piece dct {zone_color = color_, color = color_, id = id_}
    in
        List.foldr assign dct holding_pen_squares

config_pieces: PieceDict
config_pieces =
    let
        dct = Dict.empty
    in
        List.foldr config_zone_pieces dct zone_colors

assign_piece: PieceDict -> PieceConfig -> PieceDict
assign_piece dct config =
    let
        key = config.zone_color
        sub_key = config.id
        val = config.color

        maybe_sub_dict = Dict.get key dct

        sub_dict = case maybe_sub_dict of
            Just sd -> sd
            Nothing -> Dict.empty

        new_sub_dict = Dict.insert sub_key val sub_dict

    in
        Dict.insert key new_sub_dict dct

