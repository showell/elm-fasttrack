module Move exposing
    ( maybe_auto_move
    , move_to_end_loc
    )

import Piece
    exposing
        ( get_piece
        , move_piece
        )
import Player
    exposing
        ( end_locs_for_player
        , finish_move
        , get_player
        , get_player_move_type
        , maybe_replenish_hand
        , update_active_player
        )
import Set
import Type
    exposing
        ( Color
        , Model
        , Move
        , MoveType(..)
        , PieceLocation
        , Player
        )


perform_move : Model -> Move -> Color -> Model
perform_move model move active_color =
    let
        ( _, start_loc, _ ) =
            move

        piece_map =
            model.piece_map

        piece_color =
            get_piece piece_map start_loc
    in
    case piece_color of
        Nothing ->
            model

        Just _ ->
            let
                zone_colors =
                    model.zone_colors

                new_piece_map =
                    piece_map
                        |> move_piece move

                model_ =
                    { model
                        | piece_map = new_piece_map
                    }
            in
            model_
                |> maybe_replenish_hand active_color
                |> update_active_player (finish_move new_piece_map zone_colors active_color move)


maybe_auto_move : PieceLocation -> Model -> Model
maybe_auto_move start_loc model =
    let
        zone_colors =
            model.zone_colors

        players =
            model.players

        active_color =
            model.get_active_color zone_colors

        active_player =
            get_player players active_color

        end_locs =
            end_locs_for_player active_player

        unique_end_loc =
            if Set.size end_locs == 1 then
                end_locs
                    |> Set.toList
                    |> List.head

            else
                Nothing
    in
    case unique_end_loc of
        Nothing ->
            -- If we don't have exactly one location, don't update the
            -- model (and we have UI to let the user pick the location).
            model

        Just end_loc ->
            model
                |> move_to_end_loc active_player active_color start_loc end_loc


move_to_end_loc : Player -> Color -> PieceLocation -> PieceLocation -> Model -> Model
move_to_end_loc active_player active_color start_loc end_loc model =
    let
        move_type_ =
            get_player_move_type active_player end_loc
    in
    case move_type_ of
        Just move_type ->
            let
                move =
                    ( move_type, start_loc, end_loc )
            in
            perform_move model move active_color

        Nothing ->
            -- this is actually a programming error
            model
