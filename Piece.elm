module Piece exposing
    ( assign_piece
    , config_pieces
    , get_piece
    , maybe_send_piece_to_pen
    , player_pieces
    , unassign_piece
    , piece_view
    )

import Config
    exposing
        ( holding_pen_locations
        )
import Dict
import Html exposing (..)
import Msg exposing (..)
import List.Extra
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Type
    exposing
        ( Color
        , PieceDict
        , PieceLocation
        )


get_piece : PieceDict -> PieceLocation -> Maybe String
get_piece piece_map piece_loc =
    Dict.get piece_loc piece_map


player_pieces : PieceDict -> Color -> Set.Set PieceLocation
player_pieces piece_map active_color =
    let
        locs =
            Dict.keys piece_map

        is_active loc =
            let
                piece_color =
                    Dict.get loc piece_map
            in
            case piece_color of
                Just piece_color_ ->
                    active_color == piece_color_

                Nothing ->
                    False
    in
    List.filter is_active locs
        |> Set.fromList


is_open_location : PieceDict -> PieceLocation -> Bool
is_open_location piece_map piece_loc =
    case
        get_piece piece_map piece_loc
    of
        Nothing ->
            True

        Just _ ->
            False


open_holding_pen_location : PieceDict -> Color -> Maybe PieceLocation
open_holding_pen_location piece_map color =
    let
        is_open id =
            is_open_location piece_map ( color, id )
    in
    case List.Extra.find is_open holding_pen_locations of
        Nothing ->
            Nothing

        Just id ->
            Just ( color, id )


maybe_send_piece_to_pen : PieceLocation -> PieceDict -> PieceDict
maybe_send_piece_to_pen piece_loc piece_map =
    let
        color =
            get_piece piece_map piece_loc
    in
    case color of
        Nothing ->
            -- we are not landing on a piece, so do nothing
            piece_map

        Just color_ ->
            case open_holding_pen_location piece_map color_ of
                Nothing ->
                    -- this should never happen!
                    piece_map

                Just holding_pen_loc ->
                    -- assign piece we "killed" to the
                    -- color's holding pen
                    assign_piece holding_pen_loc color_ piece_map


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


unassign_piece : PieceLocation -> PieceDict -> PieceDict
unassign_piece piece_loc =
    Dict.remove piece_loc


assign_piece : PieceLocation -> Color -> PieceDict -> PieceDict
assign_piece piece_location piece_color =
    Dict.insert piece_location piece_color


-- VIEW


piece_view : Color -> Bool -> Bool -> Float -> Float -> PieceLocation -> Html Msg
piece_view color is_active is_playable cx_ cy_ piece_location =
    let
        radius =
            if is_active then
                "7"

            else if is_playable then
                "6"

            else
                "4"
    in
    circle
        [ cx (String.fromFloat cx_)
        , cy (String.fromFloat cy_)
        , fill color
        , stroke color
        , r radius
        , onClick (ClickLocation piece_location)
        ]
        []

