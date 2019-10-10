module LegalMove exposing
    ( end_locations
    , get_can_go_n_spaces
    , get_card_for_move_type
    , get_card_for_play_type
    , get_moves_for_cards
    , get_moves_for_move_type
    , get_moves_from_location
    , has_piece_on_fast_track
    , my_pieces
    , next_zone_color
    , other_mobile_pieces
    , prev_zone_color
    , swappable_locs
    )

import Config
    exposing
        ( is_base_id
        , is_holding_pen_id
        , move_count_for_card
        , next_ids_in_zone
        , prev_id_in_zone
        )
import Dict
import List.Extra
import Piece
    exposing
        ( get_piece
        , get_the_piece
        , move_piece
        )
import Set
import Type
    exposing
        ( Card
        , Color
        , FindLocParams
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Turn(..)
        )


is_color : PieceDict -> Color -> PieceLocation -> Bool
is_color piece_map color loc =
    let
        loc_color =
            Dict.get loc piece_map
    in
    loc_color == Just color


is_normal_loc : PieceLocation -> Bool
is_normal_loc ( _, id ) =
    not (is_holding_pen_id id) && not (is_base_id id)


swappable_locs : PieceDict -> Color -> Set.Set PieceLocation
swappable_locs piece_map active_color =
    let
        is_them loc =
            not (is_color piece_map active_color loc)
    in
    Dict.keys piece_map
        |> List.filter is_them
        |> List.filter is_normal_loc
        |> Set.fromList


my_pieces : PieceDict -> Color -> Set.Set PieceLocation
my_pieces piece_map active_color =
    Dict.keys piece_map
        |> List.filter (is_color piece_map active_color)
        |> Set.fromList


other_mobile_pieces : PieceDict -> Color -> PieceLocation -> Set.Set PieceLocation
other_mobile_pieces piece_map active_color loc =
    -- mobile pieces are not in the holding pen (and can theoretically
    -- move forward on a split seven, until we dig deeper)
    let
        is_mobile ( _, id ) =
            not (is_holding_pen_id id)
    in
    my_pieces piece_map active_color
        |> Set.remove loc
        |> Set.filter is_mobile


has_piece_on_fast_track : PieceDict -> Color -> Bool
has_piece_on_fast_track piece_map active_color =
    let
        is_ft ( _, id ) =
            id == "FT"

        locs =
            my_pieces piece_map active_color
                |> Set.filter is_ft
    in
    Set.size locs >= 1


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
        ( _, id ) =
            loc

        can_fast_track =
            id == "FT"

        piece_color =
            get_the_piece piece_map loc

        can_move =
            can_fast_track || not (has_piece_on_fast_track piece_map piece_color)
    in
    if can_move then
        can_go_n_spaces can_fast_track piece_color piece_map zone_colors n loc

    else
        False


can_go_n_spaces : Bool -> Color -> PieceDict -> List Color -> Int -> PieceLocation -> Bool
can_go_n_spaces can_fast_track piece_color piece_map zone_colors n_spaces location =
    {--
        This implementation may be slight overkill, as it would be easy enough
        to just call get_next_locs with the full move count and check its length.

        We get minor optimizations here insofar as we short-circuit the
        depth-first search as soon as we find a possible path for the piece to
        go N spaces.  Of course, most of the time there's only valid path for
        a piece to move, so you don't save much.  Multiple paths come into
        play when a piece starts on the fast track.
    --}
    let
        recurse n loc =
            if n <= 0 then
                -- impossible
                False

            else
                let
                    params =
                        { reverse_mode = False
                        , can_fast_track = can_fast_track
                        , can_leave_pen = False
                        , moves_left = 1
                        , loc = loc
                        , piece_color = piece_color
                        , piece_map = piece_map
                        , zone_colors = zone_colors
                        }

                    locs =
                        get_next_locs params |> Set.toList
                in
                if n == 1 then
                    if List.length locs >= 1 then
                        True

                    else
                        False

                else
                    let
                        can_go next_loc =
                            recurse (n - 1) next_loc
                    in
                    List.any can_go locs
    in
    recurse n_spaces location


get_moves_for_cards : Set.Set Card -> PieceDict -> List Color -> Color -> List Move
get_moves_for_cards cards piece_map zone_colors active_color =
    let
        normal_move_type : Card -> MoveType
        normal_move_type card =
            if card == "4" then
                Reverse card

            else
                WithCard card

        f : (Card -> MoveType) -> List Move
        f make_move_type =
            let
                get_moves : Card -> List Move
                get_moves card =
                    let
                        move_type =
                            make_move_type card

                        moves =
                            get_moves_for_move_type move_type piece_map zone_colors active_color
                    in
                    moves
            in
            cards
                |> Set.toList
                |> List.map get_moves
                |> List.concat

        forward_moves =
            f normal_move_type
    in
    if List.length forward_moves > 0 then
        forward_moves

    else
        f Reverse


get_moves_for_move_type : MoveType -> PieceDict -> List Color -> Color -> List Move
get_moves_for_move_type move_type piece_map zone_colors active_color =
    let
        start_locs : Set.Set PieceLocation
        start_locs =
            case move_type of
                FinishSplit _ exclude_loc ->
                    other_mobile_pieces piece_map active_color exclude_loc

                _ ->
                    my_pieces piece_map active_color

        get_moves : PieceLocation -> List Move
        get_moves start_loc =
            get_moves_from_location move_type piece_map zone_colors start_loc
    in
    start_locs
        |> Set.toList
        |> List.map get_moves
        |> List.concat


get_moves_from_location : MoveType -> PieceDict -> List Color -> PieceLocation -> List Move
get_moves_from_location move_type piece_map zone_colors start_loc =
    let
        ( _, id ) =
            start_loc

        can_fast_track =
            id == "FT"

        piece_color =
            get_the_piece piece_map start_loc

        active_card =
            get_card_for_move_type move_type

        can_leave_pen =
            List.member active_card [ "A", "joker", "6" ]

        reverse_mode =
            case move_type of
                Reverse _ ->
                    True

                _ ->
                    active_card == "4"

        moves_left =
            move_count_for_move_type move_type id

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
                , loc = start_loc
                , piece_color = piece_color
                , piece_map = piece_map
                , zone_colors = zone_colors
                }

            make_move : PieceLocation -> Move
            make_move end_loc =
                ( move_type, start_loc, end_loc )
        in
        if move_type == WithCard "7" then
            get_moves_for_seven params

        else if move_type == WithCard "J" then
            get_moves_for_jack params

        else
            end_locations params
                |> Set.toList
                |> List.map make_move

    else
        []


can_finish_split : List Color -> Set.Set PieceLocation -> PieceDict -> Int -> PieceLocation -> PieceLocation -> Bool
can_finish_split zone_colors other_locs piece_map count start_loc end_loc =
    let
        -- TODO: pass in move_type when we have StartSplit
        --       (it's essentially ignored now, but that
        --       may change)
        move_type =
            WithCard "7"

        move =
            ( move_type, start_loc, end_loc )

        modified_piece_map =
            move_piece move piece_map

        can_go other_loc =
            get_can_go_n_spaces modified_piece_map other_loc zone_colors count

        other_movable_locs =
            other_locs |> Set.toList |> List.filter can_go
    in
    List.length other_movable_locs > 0


get_moves_for_jack : FindLocParams -> List Move
get_moves_for_jack params =
    let
        piece_map =
            params.piece_map

        start_loc =
            params.loc

        piece_color =
            get_the_piece piece_map start_loc

        forward_moves =
            end_locations params
                |> Set.toList
                |> List.map (\end_loc -> ( WithCard "J", start_loc, end_loc ))

        trade_moves =
            if is_normal_loc start_loc then
                swappable_locs piece_map piece_color
                    |> Set.toList
                    |> List.map (\end_loc -> ( JackTrade, start_loc, end_loc ))

            else
                []
    in
    List.concat [ forward_moves, trade_moves ]


get_moves_for_seven : FindLocParams -> List Move
get_moves_for_seven params =
    let
        piece_map =
            params.piece_map

        loc =
            params.loc

        zone_colors =
            params.zone_colors

        piece_color =
            get_the_piece piece_map loc

        get_locs move_count =
            end_locations
                { params
                    | moves_left = move_count
                }
                |> Set.toList

        full_moves =
            get_locs 7
                |> List.map (\end_loc -> ( WithCard "7", loc, end_loc ))

        other_locs =
            other_mobile_pieces piece_map piece_color loc
    in
    if Set.size other_locs == 0 then
        full_moves

    else
        let
            prev_loc =
                loc

            get_partial_moves move_count =
                let
                    other_count =
                        7 - move_count

                    move_type =
                        StartSplit move_count
                in
                get_locs move_count
                    |> List.filter (can_finish_split zone_colors other_locs piece_map other_count prev_loc)
                    |> List.map (\end_loc -> ( move_type, loc, end_loc ))

            partial_moves =
                List.range 1 6
                    |> List.map get_partial_moves
                    |> List.concat
        in
        partial_moves ++ full_moves


end_locations : FindLocParams -> Set.Set PieceLocation
end_locations params =
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
                    end_locations
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
    if is_holding_pen_id id then
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
                next_ids_in_zone id piece_color zone_color
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
    if is_holding_pen_id id then
        Set.empty

    else if is_base_id id then
        Set.empty

    else if id == "R4" then
        filter [ ( prev_color, "FT" ) ]

    else
        let
            prev_id =
                prev_id_in_zone id
        in
        [ ( zone_color, prev_id ) ]
            |> filter


get_card_for_play_type : PlayType -> Card
get_card_for_play_type play_type =
    case play_type of
        PlayCard card ->
            card

        FinishSeven _ ->
            "7"


get_card_for_move_type : MoveType -> Card
get_card_for_move_type move_type =
    case move_type of
        WithCard card ->
            card

        Reverse card ->
            card

        StartSplit _ ->
            "7"

        FinishSplit _ _ ->
            "7"

        JackTrade ->
            "J"


move_count_for_move_type : MoveType -> String -> Int
move_count_for_move_type move_type id =
    case move_type of
        WithCard card ->
            move_count_for_card card id

        Reverse card ->
            move_count_for_card card id

        StartSplit count ->
            count

        FinishSplit count _ ->
            count

        JackTrade ->
            -- we never call this for J trades
            0


is_loc_free : PieceDict -> Color -> PieceLocation -> Bool
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
