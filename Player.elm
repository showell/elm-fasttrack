module Player
    exposing
        ( config_players
        , player_view
        , draw_card_cmd
        , draw_card
        , activate_card
        , finish_card
        , set_turn
        , can_player_move
        , get_active_square
        , set_active_square
        , clear_active_square
        , set_move_error
        , update_player
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Random
import Msg
    exposing
        ( Msg(..)
        )
import Type
    exposing
        ( Color
        , Card
        , TurnCardInfo
        , Turn(..)
        , PieceLocation
        , Player
        , PlayerDict
        )
import Deck exposing (full_deck)
import List.Extra


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


draw_card_cmd : PlayerDict -> Color -> Cmd Msg
draw_card_cmd players color =
    let
        player =
            get_player players color

        deckCount =
            List.length player.deck

        max =
            deckCount - 1
    in
        Random.generate (DrawCardResult color) (Random.int 0 max)


set_move_error : String -> Player -> Player
set_move_error error player =
    case player.turn of
        TurnCard info ->
            let
                turn =
                    TurnCard
                        { info
                            | active_square = Nothing
                            , move_error = Just error
                        }
            in
                { player | turn = turn }

        other ->
            player


clear_active_square : Player -> Player
clear_active_square player =
    case player.turn of
        TurnCard info ->
            let
                turn =
                    TurnCard
                        { info
                            | active_square = Nothing
                            , move_error = Nothing
                        }
            in
                { player | turn = turn }

        other ->
            player


set_active_square : PieceLocation -> Player -> Player
set_active_square square player =
    case player.turn of
        TurnCard info ->
            let
                turn =
                    TurnCard
                        { info
                            | active_square = Just square
                            , move_error = Nothing
                        }
            in
                { player | turn = turn }

        other ->
            player


get_active_square : PlayerDict -> Color -> Maybe PieceLocation
get_active_square players color =
    let
        player =
            get_player players color
    in
        case player.turn of
            TurnCard info ->
                info.active_square

            other ->
                Nothing


can_player_move : PlayerDict -> Color -> Bool
can_player_move players color =
    let
        player =
            get_player players color
    in
        case player.turn of
            TurnCard _ ->
                True

            other ->
                False


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
                , active_square = Nothing
                , move_error = Nothing
                }
    in
        { player
            | turn = turn
            , hand = new_hand
        }


maybe_replenish : List Card -> List Card
maybe_replenish deck =
    case List.length deck of
        0 ->
            full_deck

        other ->
            deck


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
            | deck = maybe_replenish deck
            , hand = hand
        }


finish_card : Player -> Player
finish_card player =
    { player | turn = TurnInProgress }



-- VIEW


card_css : Color -> List (Attribute Msg)
card_css color =
    [ style "border-color" color
    , style "background" "white"
    , style "color" color
    , style "padding" "4px"
    , style "margin" "3px"
    , style "font-size" "110%"
    , style "min-width" "30px"
    ]


view_hand_card : Color -> Player -> Int -> Card -> Html Msg
view_hand_card color player idx card =
    let
        css =
            card_css color

        attrs =
            case player.turn of
                TurnInProgress ->
                    [ onClick (ActivateCard color idx) ]

                other ->
                    [ disabled True ]
    in
        button
            (attrs ++ css)
            [ Html.text card ]


deck_view : Player -> Color -> Html Msg
deck_view player color =
    let
        deckCount =
            List.length player.deck

        handCount =
            List.length player.hand

        title_ =
            (String.fromInt deckCount) ++ " cards left"

        attrs =
            if (handCount < 5) && (player.turn == TurnInProgress) then
                [ onClick (DrawCard color) ]
            else
                [ disabled True ]

        css =
            card_css color
    in
        button
            (attrs ++ css ++ [ title title_ ])
            [ Html.text "Deck" ]


player_view : PlayerDict -> Color -> Html Msg
player_view players color =
    let
        player =
            get_player players color

        deck =
            deck_view player color

        hand_cards =
            List.indexedMap (view_hand_card color player) player.hand

        hand =
            span [] hand_cards

        active_card =
            case player.turn of
                TurnCard turn_info ->
                    active_card_view turn_info color

                other ->
                    div [] [ Html.text "click a card below" ]
    in
        div []
            [ active_card
            , span [] [ deck, hand ]
            ]


active_card_view : TurnCardInfo -> Color -> Html Msg
active_card_view turn_info color =
    case turn_info.active_square of
        Just _ ->
            div []
                [ Html.text ("play now: " ++ turn_info.active_card)
                , div [] [ Html.text "Click a square to end move." ]
                , hr [] []
                ]

        Nothing ->
            let
                finish_button =
                    button
                        [ onClick (FinishCard color) ]
                        [ Html.text "Done" ]

                move_error =
                    case turn_info.move_error of
                        Just error ->
                            div [] [ Html.text error ]

                        Nothing ->
                            div [] []
            in
                div []
                    [ Html.text ("play now: " ++ turn_info.active_card)
                    , div [] [ Html.text "Click a piece to start move." ]
                    , div [] [ finish_button ]
                    , move_error
                    , hr [] []
                    ]
