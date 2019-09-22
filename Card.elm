module Card
    exposing
        ( AllCards
        , config_all_cards
        , card_view
        , draw_card_cmd
        , draw_card
        )

import Html exposing (..)
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
        )
import ListExtra

type alias Card =
    String


type alias PlayerCards =
    { deck : List Card
    , hand: List Card
    , active_card : Maybe Card
    , get_out_pile : List Card
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
            , get_out_pile = []
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


card_view : AllCards -> Color -> Html Msg
card_view all_cards color =
    let
        player =
            get_player all_cards color

        deckCount =
            List.length player.deck

        buttonText =
            "Deck (" ++ (toString deckCount) ++ ")"

        hand_card card =
            button [] [Html.text card]

        hand = div [] (List.map hand_card player.hand)
    in
        div []
            [ button [ onClick (DrawCard color) ] [ Html.text buttonText]
            , hand
            ]

draw_card_cmd: AllCards -> Color -> Cmd Msg
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

draw_card: AllCards -> Color -> Int -> AllCards
draw_card all_cards color idx =
    let
        player =
            get_player all_cards color

        card =
            case ListExtra.getAt idx player.deck of
                Nothing ->
                    "bogus"
                Just card_ ->
                    card_

        hand = List.append player.hand [card]

        new_deck = ListExtra.removeAt idx player.deck

        new_player =
            { player
            | deck = new_deck
            , hand = hand
            }
    in
        Dict.insert color new_player all_cards

-- We ignore suits in FastTrack, since they don't affect game play.
-- Also, we never truly shuffle the deck; instead, we remove cards
-- from random positions in the remaining deck when players draw
-- cards.


full_deck : List Card
full_deck =
    [ "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    , "10"
    , "J"
    , "Q"
    , "K"
    , "A"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    , "10"
    , "J"
    , "Q"
    , "K"
    , "A"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    , "10"
    , "J"
    , "Q"
    , "K"
    , "A"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    , "10"
    , "J"
    , "Q"
    , "K"
    , "A"
    , "joker"
    , "joker"
    ]
