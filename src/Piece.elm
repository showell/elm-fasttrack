module Piece exposing
    ( config_pieces
    , get_piece
    , move_piece
    )

import Config
    exposing
        ( holding_pen_locations
        )
import Dict
import List.Extra
import Type
    exposing
        ( Color
        , Move
        , PieceDict
        , PieceLocation
        )


get_piece : PieceDict -> PieceLocation -> Maybe String
get_piece piece_map piece_loc =
    Dict.get piece_loc piece_map


is_open_location : PieceDict -> PieceLocation -> Bool
is_open_location piece_map piece_loc =
    case
        get_piece piece_map piece_loc
    of
        Nothing ->
            True

        Just _ ->
            False


open_holding_pen_location : PieceDict -> Color -> PieceLocation
open_holding_pen_location piece_map color =
    -- We expect to only be called when we know we're sending a
    -- piece home, so there should always be a square.
    let
        is_open id =
            is_open_location piece_map ( color, id )
    in
    List.Extra.find is_open holding_pen_locations
        |> Maybe.andThen (\id -> Just ( color, id ))
        |> Maybe.withDefault ( "bogus", "bogus" )


config_zone_pieces : String -> PieceDict -> PieceDict
config_zone_pieces color piece_map =
    let
        assign id =
            Dict.insert ( color, id ) color
    in
    List.foldl assign piece_map holding_pen_locations


config_pieces : List Color -> PieceDict
config_pieces zone_colors =
    let
        dct =
            Dict.empty
    in
    List.foldl config_zone_pieces dct zone_colors


move_piece : Move -> PieceDict -> PieceDict
move_piece move piece_map =
    let
        want_trade =
            move.want_trade

        start_loc =
            move.start

        end_loc =
            move.end

        start_color =
            get_piece piece_map start_loc
                |> Maybe.withDefault "bogus"

        maybe_end_color =
            get_piece piece_map end_loc
    in
    case maybe_end_color of
        Just end_color ->
            if want_trade then
                piece_map
                    |> Dict.insert start_loc end_color
                    |> Dict.insert end_loc start_color

            else
                let
                    pen_loc =
                        open_holding_pen_location piece_map end_color
                in
                piece_map
                    |> Dict.insert pen_loc end_color
                    |> Dict.remove start_loc
                    |> Dict.insert end_loc start_color

        _ ->
            piece_map
                |> Dict.remove start_loc
                |> Dict.insert end_loc start_color
