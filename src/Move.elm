module Move exposing
    ( maybe_auto_move
    , perform_move
    )

import LegalMove
    exposing
        ( get_moves_for_player
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
        , get_player_cards
        , maybe_replenish_hand
        , player_played_jack
        )
import Set
import Type
    exposing
        ( Color
        , Model
        , Move
        , PieceLocation
        , UpdatePlayerFunc
        )


perform_move : Model -> Move -> Color -> UpdatePlayerFunc -> Model
perform_move model move active_color update_active_player =
    let
        piece_map =
            model.piece_map

        piece_color =
            get_piece piece_map move.start
    in
    case piece_color of
        Nothing ->
            model

        Just _ ->
            let
                new_map =
                    piece_map
                        |> move_piece move

                zone_colors =
                    model.zone_colors

                model_ =
                    update_active_player (finish_move zone_colors active_color move.start move.end)
                        |> maybe_replenish_hand active_color
            in
            { model_ | piece_map = new_map }


maybe_auto_move : PieceLocation -> UpdatePlayerFunc -> Model -> Model
maybe_auto_move start_loc update_active_player model =
    let
        piece_map =
            model.piece_map

        zone_colors =
            model.zone_colors

        players =
            model.players

        active_color =
            model.get_active_color zone_colors

        active_player =
            get_player players active_color

        cards =
            get_player_cards active_player

        moves =
            get_moves_for_player cards piece_map zone_colors active_color

        end_locs =
            end_locs_for_player active_player piece_map zone_colors moves

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
            -- model (and we'll have UI to let the user pick the location).
            model

        Just end_loc ->
            let
                want_trade =
                    player_played_jack active_player

                move =
                    { start = start_loc
                    , end = end_loc
                    , want_trade = want_trade
                    }
            in
            perform_move model move active_color update_active_player
