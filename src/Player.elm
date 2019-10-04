module Player exposing
    ( activate_card
    , can_player_start_move_here
    , config_players
    , finish_card
    , finish_move
    , get_active_location
    , get_player
    , get_player_cards
    , maybe_replenish_hand
    , reachable_locs_for_player
    , ready_to_play
    , replenish_hand
    , set_active_location
    , set_move_error
    , set_turn
    , update_player
    )

import Deck exposing (full_deck)
import Dict
import LegalMove
    exposing
        ( distance
        , get_reachable_locs
        )
import List.Extra
import Piece
    exposing
        ( get_piece
        )
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
        , Player
        , PlayerDict
        , Turn(..)
        , TurnCardInfo
        )


get_player_cards : Player -> Set.Set Card
get_player_cards player =
    let
        cards =
            case player.turn of
                TurnCard info ->
                    player.hand ++ [ info.active_card ]

                other ->
                    player.hand
    in
    cards |> Set.fromList


config_player : Color -> Color -> Player
config_player active_color color =
    let
        turn =
            if active_color == color then
                TurnInProgress

            else
                TurnIdle

        original_setup =
            { deck = full_deck
            , hand = []
            , discard_pile = []
            , turn = turn
            }
    in
    original_setup


config_players : Color -> List Color -> PlayerDict
config_players active_color zone_colors =
    let
        config_one color =
            Dict.insert color (config_player active_color color)

        dct =
            Dict.empty
    in
    List.foldl config_one dct zone_colors


get_player : PlayerDict -> Color -> Player
get_player players color =
    -- The "Maybe" is just to satisfy the compiler
    Dict.get color players
        |> Maybe.withDefault (config_player "bogus" "bogus")


ready_to_play : Player -> Bool
ready_to_play player =
    case player.turn of
        TurnCard info ->
            info.active_location == Nothing

        other ->
            False


set_move_error : String -> Player -> Player
set_move_error error player =
    case player.turn of
        TurnCard info ->
            let
                turn =
                    TurnCard
                        { info
                            | active_location = Nothing
                            , move_error = Just error
                        }
            in
            { player | turn = turn }

        other ->
            player


is_move_again_card : Card -> Bool
is_move_again_card card =
    List.member card [ "A", "K", "Q", "J", "joker", "6" ]


maybe_finish_turn : TurnCardInfo -> Turn
maybe_finish_turn info =
    if is_move_again_card info.active_card then
        TurnInProgress

    else
        TurnDone


maybe_finish_card : TurnCardInfo -> Turn
maybe_finish_card info =
    let
        max_moves =
            if info.active_card == "7" then
                if info.distance_moved == 7 then
                    1

                else
                    2

            else
                1
    in
    if info.num_moves == max_moves then
        maybe_finish_turn info

    else
        TurnCard info


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


finish_move : List Color -> Color -> PieceLocation -> PieceLocation -> Player -> Player
finish_move zone_colors active_color start_loc end_loc player =
    case player.turn of
        TurnCard info ->
            let
                distance_moved =
                    if info.active_card == "4" then
                        -4

                    else if info.active_card == "J" then
                        -- when using J to swap pieces, the distance
                        -- concept will become irrelevant
                        0

                    else
                        distance zone_colors active_color start_loc end_loc

                turn =
                    maybe_finish_card
                        { info
                            | active_location = Nothing
                            , move_error = Nothing
                            , num_moves = info.num_moves + 1
                            , distance_moved = distance_moved
                        }
            in
            { player | turn = turn }

        other ->
            player


set_active_location : PieceLocation -> Player -> Player
set_active_location loc player =
    case player.turn of
        TurnCard info ->
            let
                turn =
                    TurnCard
                        { info
                            | active_location = Just loc
                            , move_error = Nothing
                        }
            in
            { player | turn = turn }

        other ->
            player


get_active_location : Player -> Maybe PieceLocation
get_active_location player =
    case player.turn of
        TurnCard info ->
            info.active_location

        other ->
            Nothing


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
    let
        active_card =
            List.Extra.getAt idx player.hand
                |> Maybe.withDefault "bogus"

        new_hand =
            List.Extra.removeAt idx player.hand

        turn =
            TurnCard
                { active_card = active_card
                , active_location = Nothing
                , move_error = Nothing
                , num_moves = 0
                , distance_moved = 0
                }
    in
    { player
        | turn = turn
        , hand = new_hand
    }


maybe_replenish_deck : List Card -> List Card
maybe_replenish_deck deck =
    case List.length deck of
        0 ->
            full_deck

        other ->
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
    let
        card =
            case List.Extra.getAt idx player.deck of
                Nothing ->
                    "bogus"

                Just card_ ->
                    card_

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
                        turn =
                            case player.turn of
                                TurnCard info ->
                                    maybe_finish_turn info

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


can_player_start_move_here : Player -> Color -> PieceDict -> PieceLocation -> Bool
can_player_start_move_here player player_color piece_map loc =
    let
        piece_color =
            get_piece piece_map loc
    in
    case piece_color of
        Nothing ->
            False

        Just piece_color_ ->
            if piece_color_ == player_color then
                case player.turn of
                    TurnCard _ ->
                        True

                    other ->
                        False

            else
                False


reachable_locs_for_player : Player -> PieceDict -> List Color -> Set.Set CardStartEnd -> Set.Set PieceLocation
reachable_locs_for_player active_player piece_map zone_colors moves =
    case active_player.turn of
        TurnCard info ->
            let
                loc =
                    info.active_location

                active_card =
                    info.active_card
            in
            case loc of
                Just start_loc ->
                    if info.num_moves == 0 then
                        moves
                            |> Set.filter (\( card, start, end ) -> card == active_card)
                            |> Set.filter (\( card, start, end ) -> start == start_loc)
                            |> Set.map (\( card, start, end ) -> end)

                    else
                        let
                            move_count =
                                7 - info.distance_moved
                        in
                        get_reachable_locs (ForceCount move_count) piece_map zone_colors start_loc

                Nothing ->
                    Set.empty

        other ->
            Set.empty
