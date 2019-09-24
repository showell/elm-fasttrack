module Player
    exposing
        ( config_players
        , player_view
        , draw_card_cmd
        , draw_card
        , activate_card
        , finish_card
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
        , PlayerCards
        , PlayerDict
        )
import Deck exposing (full_deck)
import List.Extra


config_player : PlayerCards
config_player =
    let
        original_setup =
            { deck = full_deck
            , hand = []
            , active_card = Nothing
            , discard_pile = []
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


get_player : PlayerDict -> Color -> PlayerCards
get_player players color =
    -- The "Maybe" is just to satisfy the compiler
    Dict.get color players
        |> Maybe.withDefault config_player


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


update_player : PlayerDict -> Color -> (PlayerCards -> PlayerCards) -> PlayerDict
update_player players color f =
    let
        player =
            get_player players color

        new_player =
            f player
    in
        Dict.insert color new_player players


activate_card : PlayerDict -> Color -> Int -> PlayerDict
activate_card players color idx =
    update_player
        players
        color
        (\player ->
            let
                active_card =
                    List.Extra.getAt idx player.hand

                new_hand =
                    List.Extra.removeAt idx player.hand
            in
                { player
                    | active_card = active_card
                    , hand = new_hand
                }
        )


maybe_replenish : List Card -> List Card
maybe_replenish deck =
    case List.length deck of
        0 ->
            full_deck

        other ->
            deck


draw_card : PlayerDict -> Color -> Int -> PlayerDict
draw_card players color idx =
    update_player
        players
        color
        (\player ->
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
        )


finish_card : PlayerDict -> Color -> PlayerDict
finish_card players color =
    update_player
        players
        color
        (\player ->
            { player
                | active_card = Nothing
            }
        )



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


view_hand_card : Color -> PlayerCards -> Int -> Card -> Html Msg
view_hand_card color player idx card =
    let
        css =
            card_css color

        attrs =
            case player.active_card of
                Nothing ->
                    [ onClick (ActivateCard color idx) ]

                other ->
                    [ disabled True ]
    in
        button
            (attrs ++ css)
            [ Html.text card ]


deck_view : PlayerCards -> Color -> Html Msg
deck_view player color =
    let
        deckCount =
            List.length player.deck

        handCount =
            List.length player.hand

        title_ =
            (String.fromInt deckCount) ++ " cards left"

        attrs =
            if (handCount < 5) && (player.active_card == Nothing) then
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

        finish_button =
            button
                [ onClick (FinishCard color) ]
                [ Html.text "Done" ]

        active_card =
            case player.active_card of
                Nothing ->
                    div [] [ Html.text "click a card below" ]

                Just active_card_ ->
                    div []
                        [ Html.text ("play now: " ++ active_card_)
                        , div [] [ finish_button ]
                        , hr [] []
                        ]
    in
        div []
            [ active_card
            , span [] [ deck, hand ]
            ]
