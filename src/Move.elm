module Move exposing (perform_move)

import Piece
    exposing
        ( get_piece
        , move_piece
        )
import Player
    exposing
        ( finish_move
        , maybe_replenish_hand
        )
import Type
    exposing
        ( Color
        , Model
        , Move
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
