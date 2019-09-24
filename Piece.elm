module Piece exposing
    ( config_pieces
    , get_piece
    , assign_piece
    , unassign_piece
    )

import Dict

import Type exposing
    ( SquareKey
    , Color
    , PieceDict
    )

import Config exposing
    ( holding_pen_squares
    )

type alias PieceConfig =
    { zone_color: Color
    , color: Color
    , id: String
    }

get_piece: PieceDict -> Color -> String -> Maybe String
get_piece dct color id =
    case Dict.get color dct of
        Just sub_dict -> Dict.get id sub_dict
        other -> Nothing

config_zone_pieces: String -> PieceDict -> PieceDict
config_zone_pieces color_ dct =
    let
        assign id_ =
            assign_piece {zone_color = color_, color = color_, id = id_}
    in
        List.foldr assign dct holding_pen_squares

config_pieces: List Color -> PieceDict
config_pieces zone_colors =
    let
        dct = Dict.empty
    in
        List.foldl config_zone_pieces dct zone_colors

unassign_piece: SquareKey -> PieceDict -> PieceDict
unassign_piece square_key dct =
    case Dict.get square_key.zone_color dct of
        Nothing ->
            -- this shouldn't happen if callers are
            -- already checking that a piece is currently
            -- assigned to this square
            dct
        Just sub_dict ->
            let
                new_sub_dict = Dict.remove square_key.id sub_dict
            in
                Dict.insert square_key.zone_color new_sub_dict dct

assign_piece: PieceConfig -> PieceDict -> PieceDict
assign_piece config dct =
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

