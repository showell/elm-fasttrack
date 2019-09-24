module Card
    exposing
        ( AllCards
        , config_all_cards
        , card_view
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
        )
import Deck exposing (full_deck)
import List.Extra


type alias PlayerCards =
    { deck : List Card
    , hand : List Card
    , active_card : Maybe Card
    , discard_pile : List Card
    }


type alias AllCards =
    Dict.Dict Color PlayerCards


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


config_all_cards : List Color -> AllCards
config_all_cards zone_colors =
    let
        config_one color =
            Dict.insert color config_player

        dct =
            Dict.empty
    in
        List.foldl config_one dct zone_colors


get_player : AllCards -> Color -> PlayerCards
get_player all_cards color =
    -- The "Maybe" is just to satisfy the compiler
    Dict.get color all_cards
        |> Maybe.withDefault config_player


draw_card_cmd : AllCards -> Color -> Cmd Msg
draw_card_cmd all_cards color =
    let
        player =
            get_player all_cards color

        deckCount =
            List.length player.deck

        max =
            deckCount - 1
    in
        Random.generate (DrawCardResult color) (Random.int 0 max)


update_player : AllCards -> Color -> (PlayerCards -> PlayerCards) -> AllCards
update_player all_cards color f =
    let
        player =
            get_player all_cards color

        new_player =
            f player
    in
        Dict.insert color new_player all_cards


activate_card : AllCards -> Color -> Int -> AllCards
activate_card all_cards color idx =
    update_player
        all_cards
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

draw_card : AllCards -> Color -> Int -> AllCards
draw_card all_cards color idx =
    update_player
        all_cards
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


finish_card : AllCards -> Color -> AllCards
finish_card all_cards color =
    update_player
        all_cards
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
        css = card_css color

        attrs =
            case player.active_card of
                Nothing ->
                    [ onClick (ActivateCard color idx) ]

                other ->
                    [ disabled True ]

    in
        button
            (attrs ++ css)
            [ Html.text card]

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

        css = card_css color
    in
        button
            ( attrs ++ css ++ [title title_] )
            [ Html.text "Deck" ]

card_view : AllCards -> Color -> Html Msg
card_view all_cards color =
    let
        player =
            get_player all_cards color

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
                        , div [] [finish_button]
                        , hr [] []
                        ]
    in
        div []
            [ active_card
            , span [] [deck, hand]
            ]
