module Player exposing
    ( activate_card
    , begin_turn
    , config_players
    , end_locs_for_player
    , finish_card
    , finish_move
    , get_playable_cards
    , get_player
    , get_player_move_type
    , get_start_location
    , maybe_replenish_hand
    , replenish_hand
    , set_start_location
    , set_turn
    , start_locs_for_player
    , update_active_player
    )

import Config
    exposing
        ( full_deck
        , starting_hand
        )
import Dict
import LegalMove
    exposing
        ( get_card_for_move_type
        , get_card_for_play_type
        , get_moves_for_cards
        , get_moves_for_move_type
        )
import List.Extra
import Random
import Set
import Type
    exposing
        ( Card
        , Color
        , Model
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , PlayerDict
        , Turn(..)
        )


get_playable_cards : Player -> Set.Set Card
get_playable_cards player =
    case player.turn of
        TurnNeedCard info ->
            info.moves
                |> List.map (\( move_type, _, _ ) -> move_type)
                |> List.map get_card_for_move_type
                |> Set.fromList

        _ ->
            Set.empty


get_player_move_type : Player -> PieceLocation -> Maybe MoveType
get_player_move_type player end_loc =
    case player.turn of
        TurnNeedEndLoc info ->
            let
                moves =
                    info.moves
                        |> List.filter (\( _, _, end ) -> end == end_loc)

                move_types =
                    moves
                        |> List.map (\( move_type, _, _ ) -> move_type)

                the_move_type =
                    case List.length move_types of
                        1 ->
                            List.head move_types

                        2 ->
                            -- The only way we'd have two possible ways
                            -- to move the same piece between the same two
                            -- locations is via a jack (we're either moving
                            -- forward one or trading).  For now we just
                            -- arbitrarily pick the first option, which is
                            -- gonna be to move forward (and trade, not kill,
                            -- if there happens to be a piece there).
                            --
                            -- TODO: make this way more rigourous (and let user
                            --       choose)
                            List.head move_types

                        _ ->
                            -- programming error
                            Nothing
            in
            the_move_type

        _ ->
            -- programming error
            Nothing


config_player : Player
config_player =
    let
        -- starting_hand is usually empty, unless
        -- we turn on dev_hack
        original_setup =
            { deck = full_deck
            , hand = starting_hand
            , discard_pile = []
            , turn = TurnBegin
            }
    in
    original_setup


config_players : List Color -> PlayerDict
config_players zone_colors =
    let
        config_one color =
            Dict.insert color config_player

        dct =
            Dict.empty
    in
    List.foldl config_one dct zone_colors


get_player : PlayerDict -> Color -> Player
get_player players color =
    -- The "Maybe" is just to satisfy the compiler
    Dict.get color players
        |> Maybe.withDefault config_player


is_move_again_card : Card -> Bool
is_move_again_card card =
    List.member card [ "A", "K", "Q", "J", "joker", "6" ]


get_moves_for_player : Player -> PieceDict -> List Color -> Color -> List Move
get_moves_for_player player piece_map zone_colors active_color =
    let
        cards =
            Set.fromList player.hand
    in
    get_moves_for_cards cards piece_map zone_colors active_color


turn_need_card : Player -> PieceDict -> List Color -> Color -> Turn
turn_need_card player piece_map zone_colors active_color =
    let
        moves =
            get_moves_for_player player piece_map zone_colors active_color
    in
    TurnNeedCard
        { moves = moves
        }


maybe_finish_turn : Card -> Player -> PieceDict -> List Color -> Color -> Turn
maybe_finish_turn card player piece_map zone_colors active_color =
    if is_move_again_card card then
        turn_need_card player piece_map zone_colors active_color

    else
        TurnDone


maybe_replenish_hand : Color -> Model -> Model
maybe_replenish_hand active_color model =
    let
        player =
            get_player model.players active_color
    in
    if List.length player.hand > 0 then
        model

    else
        replenish_hand active_color model


set_turn_to_need_card : Color -> Model -> Model
set_turn_to_need_card active_color model =
    update_active_player
        (\player ->
            let
                piece_map =
                    model.piece_map

                zone_colors =
                    model.zone_colors

                new_turn =
                    turn_need_card player piece_map zone_colors active_color
            in
            { player
                | turn = new_turn
            }
        )
        model


finish_seven_split : PieceDict -> List Color -> Color -> Int -> PieceLocation -> Turn
finish_seven_split piece_map zone_colors active_color distance end_loc =
    let
        move_count =
            7 - distance

        -- We exclude the piece we just moved from the subsequent split.  We
        -- keep track of the piece by its location.
        exclude_loc =
            end_loc

        play_type =
            FinishSeven move_count

        move_type =
            FinishSplit move_count exclude_loc

        moves =
            get_moves_for_move_type move_type piece_map zone_colors active_color

        start_locs =
            moves
                |> List.map (\( _, start, _ ) -> start)
                |> Set.fromList
    in
    TurnNeedStartLoc
        { play_type = play_type
        , moves = moves
        , start_locs = start_locs
        }


finish_move : PieceDict -> List Color -> Color -> Move -> Player -> Player
finish_move piece_map zone_colors active_color move player =
    let
        ( move_type, _, end_loc ) =
            move

        card =
            get_card_for_move_type move_type

        turn =
            case move_type of
                StartSplit distance ->
                    finish_seven_split piece_map zone_colors active_color distance end_loc

                _ ->
                    maybe_finish_turn card player piece_map zone_colors active_color
    in
    { player | turn = turn }


set_start_location : PieceLocation -> Player -> Player
set_start_location start_loc player =
    case player.turn of
        TurnNeedStartLoc info ->
            let
                play_type =
                    info.play_type

                moves =
                    info.moves
                        |> List.filter (\( _, start, _ ) -> start == start_loc)

                end_locs =
                    moves
                        |> List.map (\( _, _, end ) -> end)
                        |> Set.fromList

                turn =
                    TurnNeedEndLoc
                        { play_type = play_type
                        , moves = moves
                        , start_location = start_loc
                        , end_locs = end_locs
                        }
            in
            { player | turn = turn }

        _ ->
            player


get_start_location : Player -> Maybe PieceLocation
get_start_location player =
    case player.turn of
        TurnNeedEndLoc info ->
            Just info.start_location

        _ ->
            Nothing


update_active_player : (Player -> Player) -> Model -> Model
update_active_player f model =
    let
        active_color =
            model.get_active_color model.zone_colors

        players =
            update_player model.players active_color f
    in
    { model | players = players }


update_player : PlayerDict -> Color -> (Player -> Player) -> PlayerDict
update_player players color f =
    let
        player =
            get_player players color

        new_player =
            f player
    in
    Dict.insert color new_player players


set_turn : Color -> Turn -> PlayerDict -> PlayerDict
set_turn color turn players =
    update_player
        players
        color
        (\player ->
            { player
                | turn = turn
            }
        )


activate_card : Int -> Player -> Player
activate_card idx player =
    case List.Extra.getAt idx player.hand of
        Nothing ->
            -- some kind of programming error or race
            -- condition must have happened
            player

        Just active_card ->
            case player.turn of
                TurnNeedCard info ->
                    let
                        new_hand =
                            List.Extra.removeAt idx player.hand

                        good_move move =
                            let
                                ( move_type, _, _ ) =
                                    move
                            in
                            get_card_for_move_type move_type == active_card

                        moves =
                            info.moves
                                |> List.filter good_move

                        start_locs =
                            moves
                                |> List.map (\( _, start, _ ) -> start)
                                |> Set.fromList

                        turn =
                            TurnNeedStartLoc
                                { play_type = PlayCard active_card
                                , moves = moves
                                , start_locs = start_locs
                                }
                    in
                    { player
                        | hand = new_hand
                        , turn = turn
                    }

                _ ->
                    -- programming error, we should only move to this
                    -- state from TurnNeedCard
                    player


maybe_replenish_deck : List Card -> List Card
maybe_replenish_deck deck =
    case List.length deck of
        0 ->
            full_deck

        _ ->
            deck


replenish_hand : Color -> Model -> Model
replenish_hand active_color model =
    let
        players =
            model.players

        active_player =
            get_player model.players active_color
    in
    if List.length active_player.hand >= 5 then
        -- The length of the hand should never actually
        -- exceed 5, but we want to prevent infinite loops
        -- for dev_hack kind of stuff.
        model

    else
        let
            ( idx, seed ) =
                get_card_idx active_player model.seed

            players_ =
                update_player players active_color (draw_card idx)

            model_ =
                { model
                    | players = players_
                    , seed = seed
                }
        in
        replenish_hand active_color model_


begin_turn : Color -> Model -> Model
begin_turn active_color model =
    model
        |> replenish_hand active_color
        |> set_turn_to_need_card active_color


get_card_idx : Player -> Random.Seed -> ( Int, Random.Seed )
get_card_idx player seed =
    let
        deckCount =
            List.length player.deck

        max =
            deckCount - 1
    in
    Random.step (Random.int 0 max) seed


draw_card : Int -> Player -> Player
draw_card idx player =
    case List.Extra.getAt idx player.deck of
        Nothing ->
            -- this should never happen...idx should always be valid
            player

        Just card ->
            let
                hand =
                    List.append player.hand [ card ]

                deck =
                    List.Extra.removeAt idx player.deck
            in
            { player
                | deck = maybe_replenish_deck deck
                , hand = hand
            }


finish_card : Color -> Model -> Model
finish_card active_color model =
    let
        players =
            update_player
                model.players
                active_color
                (\player ->
                    let
                        possibly_finish_turn play_type =
                            let
                                card =
                                    get_card_for_play_type play_type
                            in
                            maybe_finish_turn card player model.piece_map model.zone_colors active_color

                        turn =
                            case player.turn of
                                TurnNeedStartLoc info ->
                                    -- This is broken--we should really have a separate state
                                    -- for when we're discarding.  Right now players can play a card
                                    -- without actually finishing the move (even when a valid move
                                    -- does exist).
                                    possibly_finish_turn info.play_type

                                TurnNeedEndLoc info ->
                                    possibly_finish_turn info.play_type

                                other ->
                                    other
                    in
                    { player | turn = turn }
                )

        model_ =
            { model
                | players = players
            }
    in
    model_ |> maybe_replenish_hand active_color


start_locs_for_player : Player -> Set.Set PieceLocation
start_locs_for_player player =
    case player.turn of
        TurnNeedStartLoc info ->
            info.start_locs

        _ ->
            Set.empty


end_locs_for_player : Player -> Set.Set PieceLocation
end_locs_for_player player =
    case player.turn of
        TurnNeedEndLoc info ->
            info.end_locs

        _ ->
            Set.empty
