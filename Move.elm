module Move exposing
    ( perform_move
    )

import Type exposing
    ( SquareKind(..)
    , SquareKey
    )

import Piece exposing
    ( PieceDict
    , get_piece
    , assign_piece
    , unassign_piece
    )

type alias Move =
    { prev: SquareKey
    , next: SquareKey
    , piece_map: PieceDict
    }

type alias MoveStatus =
    { piece_map: PieceDict
    , status: String
    , active_square: Maybe SquareKey
    }

validate_move: Move -> Result String String
validate_move move =
    let
        piece_map = move.piece_map
        prev = move.prev
        next = move.next

        source_piece_color = get_piece piece_map prev.zone_color prev.id
        target_piece_color = get_piece piece_map next.zone_color next.id

    in
        if source_piece_color == target_piece_color then
            Err "You cannot land on your own piece"
        else
            Ok "success"

perform_move: Move -> MoveStatus
perform_move move =
    let
        piece_map = move.piece_map
        prev = move.prev
        next = move.next

        piece_color = get_piece piece_map prev.zone_color prev.id
    in
        case piece_color of
            Nothing ->
                { piece_map = piece_map
                , status = "program failure"
                , active_square = Nothing
                }
            Just piece_color_ ->
                case validate_move move of
                Err status ->
                    { piece_map = piece_map
                    , status = status
                    , active_square = Nothing
                    }
                Ok _ ->
                    let
                        new_config =
                            { zone_color = next.zone_color
                            , color = piece_color_
                            , id = next.id
                            }
                        new_map = piece_map
                            -- TODO: banish piece we're landing on
                            |> unassign_piece prev
                            |> assign_piece new_config
                    in
                        { piece_map = new_map
                        , status = "moved!"
                        , active_square = Nothing
                        }

