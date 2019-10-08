module Player exposing
    ( activate_card
    , begin_turn
    , config_players
    , end_locs_for_player
    , finish_card
    , finish_move
    , get_card_for_play_type
    , get_player
    , get_player_cards
    , get_start_location
    , maybe_replenish_hand
    , player_played_jack
    , replenish_hand
    , set_start_location
    , set_turn
    , start_locs_for_player
    , update_active_player
    )

import Config
    exposing
        ( full_deck
        )
import Dict
import LegalMove
    exposing
        ( distance
        , get_moves_for_cards
        , get_moves_for_move_type
        )
import List.Extra
import Random
import Set
import Type
    exposing
        ( Card
        , CardStartEnd
        , Color
        , Model
        , MoveType(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , PlayerDict
        , Turn(..)
        )


get_card_for_play_type : PlayType -> Card
get_card_for_play_type play_type =
    case play_type of
        UsingCard card ->
            card

        FinishingSplit _ ->
            "7"


get_active_card : Player -> Maybe Card
get_active_card player =
    case player.turn of
        TurnNeedStartLoc info ->
            Just (get_card_for_play_type info.play_type)

        TurnNeedEndLoc info ->
            Just (get_card_for_play_type info.play_type)

        _ ->
            Nothing


get_player_cards : Player -> Set.Set Card
get_player_cards player =
    let
        cards =
            case get_active_card player of
                Just card ->
                    card :: player.hand

                Nothing ->
                    player.hand
    in
    cards |> Set.fromList


player_played_jack : Player -> Bool
player_played_jack player =
    case get_active_card player of
        Just card ->
            card == "J"

        Nothing ->
            False


config_player : Player
config_player =
    let
        original_setup =
            { deck = full_deck
            , hand = []
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


get_moves_for_player : Player -> PieceDict -> List Color -> Color -> Set.Set CardStartEnd
get_moves_for_player player piece_map zone_colors active_color =
    let
        cards =
            get_player_cards player
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


maybe_finish_turn : PlayType -> Player -> PieceDict -> List Color -> Color -> Turn
maybe_finish_turn play_type player piece_map zone_colors active_color =
    let
        card =
            get_card_for_play_type play_type
    in
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


maybe_finish_seven : PieceDict -> List Color -> Color -> PieceLocation -> PieceLocation -> Turn
maybe_finish_seven piece_map zone_colors active_color start_loc end_loc =
    let
        distance_moved =
            distance zone_colors active_color start_loc end_loc
    in
    if distance_moved < 7 then
        let
            move_count =
                7 - distance_moved

            play_type =
                FinishingSplit move_count

            -- We exclude the piece we just moved from the subsequent split.  We
            -- keep track of the piece by its location.
            exclude_loc =
                end_loc

            move_type =
                FinishSplit move_count exclude_loc

            moves =
                get_moves_for_move_type move_type piece_map zone_colors active_color

            start_locs =
                moves
                    |> Set.map (\( start, _ ) -> start)
        in
        TurnNeedStartLoc
            { play_type = play_type
            , moves = moves
            , start_locs = start_locs
            }

    else
        TurnDone


finish_move : PieceDict -> List Color -> Color -> PieceLocation -> PieceLocation -> Player -> Player
finish_move piece_map zone_colors active_color start_loc end_loc player =
    case player.turn of
        TurnNeedEndLoc info ->
            let
                play_type =
                    info.play_type

                turn =
                    case play_type of
                        UsingCard "7" ->
                            maybe_finish_seven piece_map zone_colors active_color start_loc end_loc

                        _ ->
                            maybe_finish_turn play_type player piece_map zone_colors active_color
            in
            { player | turn = turn }

        _ ->
            player


set_start_location : PieceLocation -> Player -> Player
set_start_location start_loc player =
    case player.turn of
        TurnNeedStartLoc info ->
            let
                end_locs =
                    info.moves
                        |> Set.filter (\( start, _ ) -> start == start_loc)
                        |> Set.map (\( _, end ) -> end)

                turn =
                    TurnNeedEndLoc
                        { play_type = info.play_type
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

                        moves =
                            info.moves
                                |> Set.filter (\( card, _, _ ) -> card == active_card)
                                |> Set.map (\( _, start, end ) -> ( start, end ))

                        start_locs =
                            moves
                                |> Set.map (\( start, _ ) -> start)

                        turn =
                            TurnNeedStartLoc
                                { play_type = UsingCard active_card
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
    if List.length active_player.hand == 5 then
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
                            maybe_finish_turn play_type player model.piece_map model.zone_colors active_color

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
