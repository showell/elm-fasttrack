module Move exposing (perform_move)

import Config
    exposing
        ( get_piece_kind
        )
import Piece
    exposing
        ( assign_piece
        , get_piece
        , maybe_send_piece_to_pen
        , unassign_piece
        )
import Player
    exposing
        ( finish_move
        , maybe_replenish_hand
        , set_move_error
        )
import Type
    exposing
        ( Color
        , LocationKind(..)
        , Model
        , PieceDict
        , PieceLocation
        , UpdatePlayerFunc
        )


type alias Move =
    { prev : PieceLocation
    , next : PieceLocation
    }


validate_move : PieceDict -> Move -> Result String String
validate_move piece_map move =
    let
        prev =
            move.prev

        next =
            move.next

        spc =
            get_piece piece_map prev

        tpc =
            get_piece piece_map next
    in
    case spc of
        Nothing ->
            Err "No source piece color"

        Just source_piece_color ->
            if prev == next then
                Err "You must move somewhere else"

            else if same_color source_piece_color tpc then
                Err "You cannot land on your own piece"

            else if wrong_holding_pen source_piece_color next then
                Err "You cannot move to their holding pen"

            else if wrong_base source_piece_color next then
                Err "You cannot move to their base"

            else
                Ok "success"


same_color : Color -> Maybe String -> Bool
same_color source_piece_color tpc =
    case tpc of
        Nothing ->
            False

        Just target_piece_color ->
            source_piece_color == target_piece_color


wrong_holding_pen : Color -> PieceLocation -> Bool
wrong_holding_pen piece_color loc =
    let
        ( zone_color, id ) =
            loc
    in
    case get_piece_kind id of
        HoldingPen ->
            zone_color /= piece_color

        other ->
            False


wrong_base : Color -> PieceLocation -> Bool
wrong_base piece_color loc =
    let
        ( zone_color, id ) =
            loc
    in
    case get_piece_kind id of
        Base ->
            zone_color /= piece_color

        other ->
            False


perform_move : Model -> Move -> Color -> UpdatePlayerFunc -> Model
perform_move model move active_color update_active_player =
    let
        piece_map =
            model.piece_map

        prev_loc =
            move.prev

        next_loc =
            move.next

        piece_color =
            get_piece piece_map prev_loc
    in
    case piece_color of
        Nothing ->
            model

        Just piece_color_ ->
            case validate_move piece_map move of
                Err status ->
                    update_active_player (set_move_error status)

                Ok _ ->
                    let
                        new_map =
                            piece_map
                                |> maybe_send_piece_to_pen next_loc
                                |> unassign_piece prev_loc
                                |> assign_piece next_loc piece_color_

                        model_ =
                            update_active_player finish_move
                                |> maybe_replenish_hand active_color
                    in
                    { model_ | piece_map = new_map }
