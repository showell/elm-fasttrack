module Move
    exposing
        ( perform_move
        )

import Type
    exposing
        ( SquareKind(..)
        , SquareKey
        , Color
        , PieceDict
        , Model
        )
import Piece
    exposing
        ( get_piece
        , maybe_send_piece_to_pen
        , assign_piece
        , unassign_piece
        )
import Player
    exposing
        ( clear_active_square
        , set_move_error
        )


type alias Move =
    { prev : SquareKey
    , next : SquareKey
    }


validate_move : PieceDict -> Move -> Result String String
validate_move piece_map move =
    let
        prev =
            move.prev

        next =
            move.next

        spc =
            get_piece piece_map ( prev.zone_color, prev.id )

        tpc =
            get_piece piece_map ( next.zone_color, next.id )
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


wrong_holding_pen : Color -> SquareKey -> Bool
wrong_holding_pen piece_color next =
    case next.kind of
        HoldingPen ->
            next.zone_color /= piece_color

        other ->
            False


wrong_base : Color -> SquareKey -> Bool
wrong_base piece_color next =
    case next.kind of
        Base ->
            next.zone_color /= piece_color

        other ->
            False


perform_move : Model -> Move -> Color -> Model
perform_move model move active_color =
    let
        piece_map =
            model.piece_map

        prev =
            move.prev

        next =
            move.next

        prev_loc =
            ( prev.zone_color, prev.id )

        next_loc =
            ( next.zone_color, next.id )

        piece_color =
            get_piece piece_map ( prev.zone_color, prev.id )
    in
        case piece_color of
            Nothing ->
                model

            Just piece_color_ ->
                case validate_move piece_map move of
                    Err status ->
                        let
                            players =
                                set_move_error model.players active_color status
                        in
                            { model
                                | players = players
                            }

                    Ok _ ->
                        let
                            new_map =
                                piece_map
                                    |> maybe_send_piece_to_pen next_loc
                                    |> unassign_piece prev_loc
                                    |> assign_piece next_loc piece_color_

                            players =
                                clear_active_square model.players active_color
                        in
                            { model
                                | piece_map = new_map
                                , players = players
                            }
