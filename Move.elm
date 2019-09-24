module Move exposing
    ( perform_move
    )

import Type exposing
    ( SquareKind(..)
    , SquareKey
    , PieceDict
    )

import Piece exposing
    ( get_piece
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

        spc = get_piece piece_map prev.zone_color prev.id
        tpc = get_piece piece_map next.zone_color next.id

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

same_color: String -> Maybe String -> Bool
same_color source_piece_color tpc =
    case tpc of
        Nothing ->
            False
        Just target_piece_color ->
            source_piece_color == target_piece_color

wrong_holding_pen: String -> SquareKey -> Bool
wrong_holding_pen piece_color next =
    case next.kind of
        HoldingPen ->
            next.zone_color /= piece_color
        other ->
            False

wrong_base: String -> SquareKey -> Bool
wrong_base piece_color next =
    case next.kind of
        Base ->
            next.zone_color /= piece_color
        other ->
            False

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

