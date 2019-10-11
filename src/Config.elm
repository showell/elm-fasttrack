module Config exposing
    ( config_locations
    , full_deck
    , get_zone_colors
    , gutter_size
    , holding_pen_locations
    , is_base_id
    , is_holding_pen_id
    , move_count_for_card
    , next_ids_in_zone
    , num_credits_to_get_out
    , prev_id_in_zone
    , square_size
    )

import Set
import Type
    exposing
        ( Card
        , Color
        , Location
        )


num_credits_to_get_out : Int
num_credits_to_get_out =
    5


gutter_size : Float
gutter_size =
    4.0


square_size : Float
square_size =
    26.0


get_zone_colors : Int -> List String
get_zone_colors num_players =
    List.take num_players [ "red", "blue", "green", "purple", "aqua", "brown" ]


base_locations : List String
base_locations =
    [ "B1", "B2", "B3", "B4" ]


is_base_id : String -> Bool
is_base_id id =
    Set.member id (Set.fromList base_locations)


holding_pen_locations : List String
holding_pen_locations =
    [ "HP1", "HP2", "HP3", "HP4" ]


is_holding_pen_id : String -> Bool
is_holding_pen_id id =
    Set.member id (Set.fromList holding_pen_locations)


config_locations : List Location
config_locations =
    [ -- holding pen
      { x = -4.2
      , y = 1.7
      , id = "HP1"
      }
    , { x = -3.2
      , y = 1.7
      , id = "HP2"
      }
    , { x = -4.2
      , y = 0.7
      , id = "HP3"
      }
    , { x = -3.2
      , y = 0.7
      , id = "HP4"
      }
    , -- base
      { x = 0
      , y = 1
      , id = "B1"
      }
    , { x = 0
      , y = 2
      , id = "B2"
      }
    , { x = 0
      , y = 3
      , id = "B3"
      }
    , { x = 0
      , y = 4
      , id = "B4"
      }
    , -- bottom
      { x = -1
      , y = 0
      , id = "HH"
      }
    , { x = 0
      , y = 0
      , id = "DS"
      }
    , { x = 1
      , y = 0
      , id = "BR"
      }
    , -- left
      { x = -2
      , y = 0
      , id = "L0"
      }
    , { x = -2
      , y = 1
      , id = "L1"
      }
    , { x = -2
      , y = 2
      , id = "L2"
      }
    , { x = -2
      , y = 3
      , id = "L3"
      }
    , { x = -2
      , y = 4
      , id = "L4"
      }
    , { x = -2
      , y = 5
      , id = "FT"
      }
    , -- right
      { x = 2
      , y = 0
      , id = "R0"
      }
    , { x = 2
      , y = 1
      , id = "R1"
      }
    , { x = 2
      , y = 2
      , id = "R2"
      }
    , { x = 2
      , y = 3
      , id = "R3"
      }
    , { x = 2
      , y = 4
      , id = "R4"
      }
    ]


move_count_for_card : Card -> String -> Int
move_count_for_card active_card id =
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
            if is_holding_pen_id id then
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

        "joker" ->
            1

        _ ->
            0


next_ids_in_zone : String -> Color -> Color -> List String
next_ids_in_zone id piece_color zone_color =
    -- Note, this handle neither holding pen locations nor
    -- fast track locations--the caller handles those
    -- special cases.
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


prev_id_in_zone : String -> String
prev_id_in_zone id =
    -- This only handles cases where there is an obvious previous
    -- location to move to.  The caller handles R4, plus holding pen
    -- locations and base locations.
    if id == "HH" then
        "DS"

    else if id == "L0" then
        "HH"

    else if id == "L1" then
        "L0"

    else if id == "L2" then
        "L1"

    else if id == "L3" then
        "L2"

    else if id == "L4" then
        "L3"

    else if id == "FT" then
        "L4"

    else if id == "R3" then
        "R4"

    else if id == "R2" then
        "R3"

    else if id == "R1" then
        "R2"

    else if id == "R0" then
        "R1"

    else if id == "BR" then
        "R0"

    else if id == "DS" then
        "BR"

    else
        "bogus"



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
