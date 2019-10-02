module LegalMove exposing
    ( FindLocParams
    , distance
    , get_can_go_n_spaces
    , get_reachable_locs
    , has_piece_on_fast_track
    , next_zone_color
    , other_mobile_pieces
    , prev_zone_color
    , reachable_locs
    )

import Dict
import List.Extra
import Piece
    exposing
        ( get_piece
        , move_piece
        )
import Set
import Type
    exposing
        ( Card
        , Color
        , MoveType(..)
        , PieceDict
        , PieceLocation
        )


type alias FindLocParams =
    { can_fast_track : Bool
    , can_leave_pen : Bool
    , reverse_mode : Bool
    , moves_left : Int
    , loc : PieceLocation
    , piece_color : Color
    , piece_map : PieceDict
    , zone_colors : List Color
    }


is_color : PieceDict -> Color -> PieceLocation -> Bool
is_color piece_map color loc =
    let
        loc_color =
            Dict.get loc piece_map
    in
    loc_color == Just color


other_mobile_pieces : PieceDict -> Color -> PieceLocation -> Set.Set PieceLocation
other_mobile_pieces piece_map active_color loc =
    -- mobile pieces are not in the holding pen (and can theoretically
    -- move forward on a split seven, until we dig deeper)
    let
        is_mobile ( zone_color, id ) =
            not (List.member id [ "HP1", "HP2", "HP3", "HP4" ])
    in
    Dict.keys piece_map
        |> List.filter (is_color piece_map active_color)
        |> List.filter (\loc_ -> loc_ /= loc)
        |> List.filter is_mobile
        |> Set.fromList


has_piece_on_fast_track : PieceDict -> Color -> Bool
has_piece_on_fast_track piece_map active_color =
    let
        is_ft ( zone_color, id ) =
            id == "FT"

        locs =
            Dict.keys piece_map
                |> List.filter is_ft
                |> List.filter (is_color piece_map active_color)
    in
    List.length locs >= 1


distance : List Color -> PieceLocation -> PieceLocation -> Int
distance zone_colors start_loc end_loc =
    -- This only computes distances for presumably
    -- valid moves.  Any two squares that are reachable
    -- from each other are always the same distance apart,
    -- but sometimes that does include the fast track.  (You
    -- could take longer paths between some pairs of locations,
    -- but they would require multiple cards, and those distances
    -- won't matter in the context we're gonna call this function.
    -- Basically, we only intend to use this for splitting sevens,
    -- so we can computer how many places to advance the second
    -- piece.)
    let
        ( zone_color, id ) =
            start_loc
    in
    if List.member id [ "HP1", "HP2", "HP3", "HP4" ] then
        1

    else
        let
            can_fast_track =
                id == "FT"

            next_locs loc =
                get_next_locs
                    { reverse_mode = False
                    , can_fast_track = can_fast_track
                    , can_leave_pen = False
                    , moves_left = 99
                    , loc = loc
                    , piece_color = "ignore"
                    , piece_map = Dict.empty
                    , zone_colors = zone_colors
                    }

            f neighbors cnt =
                if Set.member end_loc neighbors then
                    cnt

                else
                    let
                        next_neighbors =
                            neighbors
                                |> Set.toList
                                |> List.map next_locs
                                |> List.map Set.toList
                                |> List.concat
                                |> Set.fromList
                    in
                    f next_neighbors (cnt + 1)
        in
        f (next_locs start_loc) 1


next_zone_color : Color -> List Color -> Color
next_zone_color color zone_colors =
    let
        idx =
            List.Extra.elemIndex color zone_colors
                |> Maybe.withDefault -1

        len =
            List.length zone_colors

        next_idx =
            (idx + 1) |> modBy len
    in
    List.Extra.getAt next_idx zone_colors
        |> Maybe.withDefault "bogus"


prev_zone_color : Color -> List Color -> Color
prev_zone_color color zone_colors =
    let
        idx =
            List.Extra.elemIndex color zone_colors
                |> Maybe.withDefault 1

        len =
            List.length zone_colors

        next_idx =
            (idx - 1) |> modBy len
    in
    List.Extra.getAt next_idx zone_colors
        |> Maybe.withDefault "bogus"


get_can_go_n_spaces : PieceDict -> PieceLocation -> List Color -> Int -> Bool
get_can_go_n_spaces piece_map loc zone_colors n =
    -- This function should only be called in the context of splitting
    -- sevens, so we don't account for cards being able to leave the
    -- holding pen.
    let
        ( zone_color, id ) =
            loc

        can_fast_track =
            id == "FT"

        piece_color =
            get_piece piece_map loc
                |> Maybe.withDefault "bogus"

        can_move =
            can_fast_track || not (has_piece_on_fast_track piece_map piece_color)
    in
    if can_move then
        can_go_n_spaces
            { reverse_mode = False
            , can_fast_track = can_fast_track
            , can_leave_pen = False
            , moves_left = n
            , loc = loc
            , piece_color = piece_color
            , piece_map = piece_map
            , zone_colors = zone_colors
            }

    else
        False


can_go_n_spaces : FindLocParams -> Bool
can_go_n_spaces params =
    let
        moves_left =
            params.moves_left
    in
    if moves_left <= 0 then
        -- impossible
        False

    else
        let
            locs =
                get_next_locs params |> Set.toList
        in
        if moves_left == 1 then
            if List.length locs >= 1 then
                True

            else
                False

        else
            let
                can_go loc_ =
                    can_go_n_spaces
                        { params
                            | moves_left = moves_left - 1
                            , loc = loc_
                        }

                all_paths =
                    List.filter can_go locs
            in
            List.length all_paths >= 1


get_reachable_locs : MoveType -> PieceDict -> List Color -> PieceLocation -> Set.Set PieceLocation
get_reachable_locs move_type piece_map zone_colors loc =
    let
        ( zone_color, id ) =
            loc

        can_fast_track =
            id == "FT"

        piece_color =
            get_piece piece_map loc
                |> Maybe.withDefault "bogus"

        active_card =
            case move_type of
                WithCard card ->
                    card

                ForceCount count ->
                    "ignore"

        can_leave_pen =
            List.member active_card [ "A", "joker", "6" ]

        reverse_mode =
            active_card == "4"

        moves_left =
            case move_type of
                WithCard card ->
                    get_moves_left active_card id

                ForceCount count ->
                    count

        can_move =
            can_fast_track || not (has_piece_on_fast_track piece_map piece_color)
    in
    if can_move then
        let
            params =
                { reverse_mode = reverse_mode
                , can_fast_track = can_fast_track
                , can_leave_pen = can_leave_pen
                , moves_left = moves_left
                , loc = loc
                , piece_color = piece_color
                , piece_map = piece_map
                , zone_colors = zone_colors
                }
        in
        if active_card == "7" then
            get_locs_for_seven params

        else
            reachable_locs params

    else
        Set.empty


can_finish_split : FindLocParams -> Set.Set PieceLocation -> PieceDict -> Int -> PieceLocation -> PieceLocation -> Bool
can_finish_split params other_locs piece_map count prev_loc next_loc =
    let
        move =
            { prev = prev_loc
            , next = next_loc
            }

        modified_piece_map =
            move_piece move piece_map

        can_go other_loc =
            can_go_n_spaces
                { params
                    | moves_left = count
                    , loc = other_loc
                    , piece_map = modified_piece_map
                }

        other_movable_locs =
            other_locs |> Set.toList |> List.filter can_go
    in
    List.length other_movable_locs > 0


get_locs_for_seven : FindLocParams -> Set.Set PieceLocation
get_locs_for_seven params =
    let
        piece_map =
            params.piece_map

        loc =
            params.loc

        piece_color =
            get_piece piece_map loc
                |> Maybe.withDefault "bogus"

        get_locs move_count =
            reachable_locs
                { params
                    | moves_left = move_count
                }
                |> Set.toList

        full_locs =
            get_locs 7

        other_locs =
            other_mobile_pieces piece_map piece_color loc
    in
    if Set.size other_locs == 0 then
        full_locs |> Set.fromList

    else
        let
            prev_loc =
                loc

            get_partial_locs move_count =
                let
                    other_count =
                        7 - move_count
                in
                get_locs move_count
                    |> List.filter (can_finish_split params other_locs piece_map other_count prev_loc)

            partial_locs =
                List.range 1 6
                    |> List.map get_partial_locs
                    |> List.concat
        in
        partial_locs ++ full_locs |> Set.fromList


reachable_locs : FindLocParams -> Set.Set PieceLocation
reachable_locs params =
    let
        moves_left =
            params.moves_left
    in
    if moves_left < 1 then
        -- impossible
        Set.empty

    else
        let
            locs =
                if params.reverse_mode then
                    get_prev_locs params

                else
                    get_next_locs params
        in
        if moves_left == 1 then
            locs

        else
            let
                recurse loc_ =
                    reachable_locs
                        { params
                            | moves_left = moves_left - 1
                            , loc = loc_
                        }
                        |> Set.toList
            in
            List.map recurse (Set.toList locs) |> List.concat |> Set.fromList


get_next_locs : FindLocParams -> Set.Set PieceLocation
get_next_locs params =
    let
        loc =
            params.loc

        ( zone_color, id ) =
            loc

        zone_colors =
            params.zone_colors

        next_color =
            next_zone_color zone_color zone_colors

        piece_color =
            params.piece_color

        can_fast_track =
            params.can_fast_track

        can_leave_pen =
            params.can_leave_pen

        piece_map =
            params.piece_map

        is_free loc_ =
            is_loc_free piece_map piece_color loc_

        filter lst =
            lst |> List.filter is_free |> Set.fromList
    in
    if List.member id [ "HP1", "HP2", "HP3", "HP4" ] then
        if can_leave_pen then
            filter [ ( zone_color, "L0" ) ]

        else
            Set.empty

    else if id == "FT" then
        if can_fast_track && (next_color /= piece_color) then
            filter
                [ ( next_color, "FT" )
                , ( next_color, "R4" )
                ]

        else
            filter
                [ ( next_color, "R4" )
                ]

    else
        let
            next_ids =
                if id == "HH" then
                    [ "L0" ]

                else if id == "L0" then
                    [ "L1" ]

                else if id == "L1" then
                    [ "L2" ]

                else if id == "L2" then
                    [ "L3" ]

                else if id == "L3" then
                    [ "L4" ]

                else if id == "L4" then
                    [ "FT" ]

                else if id == "R4" then
                    [ "R3" ]

                else if id == "R3" then
                    [ "R2" ]

                else if id == "R2" then
                    [ "R1" ]

                else if id == "R1" then
                    [ "R0" ]

                else if id == "R0" then
                    [ "BR" ]

                else if id == "BR" then
                    [ "DS" ]

                else if id == "DS" then
                    if zone_color == piece_color then
                        [ "B1" ]

                    else
                        [ "HH" ]

                else if id == "B1" then
                    [ "B2" ]

                else if id == "B2" then
                    [ "B3" ]

                else if id == "B3" then
                    [ "B4" ]

                else if id == "B4" then
                    -- we're home!
                    []

                else
                    []
        in
        List.map (\id_ -> ( zone_color, id_ )) next_ids
            |> filter


get_prev_locs : FindLocParams -> Set.Set PieceLocation
get_prev_locs params =
    let
        loc =
            params.loc

        ( zone_color, id ) =
            loc

        zone_colors =
            params.zone_colors

        prev_color =
            prev_zone_color zone_color zone_colors

        piece_color =
            params.piece_color

        piece_map =
            params.piece_map

        is_free loc_ =
            is_loc_free piece_map piece_color loc_

        filter lst =
            lst |> List.filter is_free |> Set.fromList
    in
    if List.member id [ "HP1", "HP2", "HP3", "HP4" ] then
        Set.empty

    else if List.member id [ "B1", "B2", "B3", "B4" ] then
        Set.empty

    else if id == "R4" then
        filter [ ( prev_color, "FT" ) ]

    else
        let
            prev_ids =
                if id == "HH" then
                    [ "DS" ]

                else if id == "L0" then
                    [ "HH" ]

                else if id == "L1" then
                    [ "L0" ]

                else if id == "L2" then
                    [ "L1" ]

                else if id == "L3" then
                    [ "L2" ]

                else if id == "L4" then
                    [ "L3" ]

                else if id == "FT" then
                    [ "L4" ]

                else if id == "R3" then
                    [ "R4" ]

                else if id == "R2" then
                    [ "R3" ]

                else if id == "R1" then
                    [ "R2" ]

                else if id == "R0" then
                    [ "R1" ]

                else if id == "BR" then
                    [ "R0" ]

                else if id == "DS" then
                    [ "BR" ]

                else
                    []
        in
        List.map (\id_ -> ( zone_color, id_ )) prev_ids
            |> filter


get_moves_left : Card -> String -> Int
get_moves_left active_card id =
    case active_card of
        "A" ->
            1

        "2" ->
            2

        "3" ->
            3

        "4" ->
            4

        "5" ->
            5

        "6" ->
            if List.member id [ "HP1", "HP2", "HP3", "HP4" ] then
                1

            else
                6

        "7" ->
            7

        "8" ->
            8

        "9" ->
            9

        "10" ->
            10

        "J" ->
            1

        "Q" ->
            1

        "K" ->
            1

        other ->
            0


is_loc_free piece_map piece_color loc =
    let
        other_piece =
            get_piece piece_map loc
    in
    case other_piece of
        Nothing ->
            True

        Just color ->
            color /= piece_color
