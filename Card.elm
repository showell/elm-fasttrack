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
import Config
    exposing
        ( zone_colors
        )
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
import ListExtra


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


config_all_cards : AllCards
config_all_cards =
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
                    ListExtra.getAt idx player.hand

                new_hand =
                    ListExtra.removeAt idx player.hand
            in
                { player
                    | active_card = active_card
                    , hand = new_hand
                }
        )


draw_card : AllCards -> Color -> Int -> AllCards
draw_card all_cards color idx =
    update_player
        all_cards
        color
        (\player ->
            let
                card =
                    case ListExtra.getAt idx player.deck of
                        Nothing ->
                            "bogus"

                        Just card_ ->
                            card_

                hand =
                    List.append player.hand [ card ]

                new_deck =
                    ListExtra.removeAt idx player.deck
            in
                { player
                    | deck = new_deck
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


view_hand_card : Color -> PlayerCards -> Int -> Card -> Html Msg
view_hand_card color player idx card =
    case player.active_card of
        Nothing ->
            button
                [ onClick (ActivateCard color idx) ]
                [ Html.text card ]

        other ->
            button
                [ disabled True ]
                [ Html.text card ]


deck_view : PlayerCards -> Color -> Html Msg
deck_view player color =
    let
        deckCount =
            List.length player.deck

        handCount =
            List.length player.hand

        buttonText =
            "Deck (" ++ (toString deckCount) ++ ")"
    in
        if (handCount < 5) && (player.active_card == Nothing) then
            button
                [ onClick (DrawCard color) ]
                [ Html.text buttonText ]
        else
            button
                [ disabled True ]
                [ Html.text buttonText ]


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
            div [] hand_cards

        finish_button =
            button
                [ onClick (FinishCard color) ]
                [ Html.text "Done" ]

        active_card =
            case player.active_card of
                Nothing ->
                    div [] [ Html.text "click a card above" ]

                Just active_card_ ->
                    div []
                        [ Html.text ("play now: " ++ active_card_)
                        , Html.text " "
                        , finish_button
                        ]
    in
        div []
            [ deck
            , hand
            , active_card
            ]
