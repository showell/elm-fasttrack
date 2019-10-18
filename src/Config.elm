module Config exposing
    ( cardValue
    , configLocations
    , fullDeck
    , gutterSize
    , hintForCard
    , holdingPenLocations
    , isBaseId
    , isHoldingPenId
    , isMoveAgainCard
    , moveCountForCard
    , nextIdsInZone
    , numCreditsToGetOut
    , prevIdInZone
    , squareSize
    )

import Set
import Type
    exposing
        ( Card
        , Color
        , Location
        )


numCreditsToGetOut : Int
numCreditsToGetOut =
    5


gutterSize : Float
gutterSize =
    4.0


squareSize : Float
squareSize =
    26.0


baseLocations : List String
baseLocations =
    [ "B1", "B2", "B3", "B4" ]


isBaseId : String -> Bool
isBaseId id =
    Set.member id (Set.fromList baseLocations)


holdingPenLocations : List String
holdingPenLocations =
    [ "HP1", "HP2", "HP3", "HP4" ]


isHoldingPenId : String -> Bool
isHoldingPenId id =
    Set.member id (Set.fromList holdingPenLocations)


isMoveAgainCard : Card -> Bool
isMoveAgainCard card =
    List.member card [ "A", "K", "Q", "J", "joker", "6" ]


configLocations : List Location
configLocations =
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


cardValue : Card -> Int
cardValue card =
    -- This is just for sorting cards in the cheat sheet.
    case card of
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
            11

        "Q" ->
            12

        "K" ->
            13

        "joker" ->
            14

        _ ->
            0


hintForCard : Card -> String
hintForCard card =
    case card of
        "A" ->
            "move 1 or get out"

        "2" ->
            "move 2"

        "3" ->
            "move 3"

        "4" ->
            "move backward 4"

        "5" ->
            "move 5"

        "6" ->
            "move 6 or get out"

        "7" ->
            "move 7 or split"

        "8" ->
            "move 8"

        "9" ->
            "move 9"

        "10" ->
            "move 10"

        "J" ->
            "move 1 or trade pieces"

        "Q" ->
            "move 1"

        "K" ->
            "move 1"

        "joker" ->
            "move 1 or get out"

        _ ->
            ""


moveCountForCard : Card -> String -> Int
moveCountForCard activeCard id =
    case activeCard of
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
            if isHoldingPenId id then
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


nextIdsInZone : String -> Color -> Color -> List String
nextIdsInZone id pieceColor zoneColor =
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
        if zoneColor == pieceColor then
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


prevIdInZone : String -> String
prevIdInZone id =
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


fullDeck : List Card
fullDeck =
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
