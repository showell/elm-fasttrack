module Type
    exposing
        ( SquareKind(..)
        , Square
        , SquareKey
        , Color
        , Card
        , PieceDict
        , Turn(..)
        , Player
        , PlayerDict
        , Model
        )

import Dict


type alias Color =
    String


type alias Card =
    String


type SquareKind
    = HoldingPen
    | Normal
    | FastTrack
    | DoorStep
    | HideyHole
    | Base


type alias Square =
    { x : Float
    , y : Float
    , kind : SquareKind
    , id : String
    }


type alias SquareKey =
    { zone_color : String
    , id : String

    -- kind is actually not an attribute, not part of the key
    , kind : SquareKind
    }


type alias ZonePieceDict =
    Dict.Dict String Color


type alias PieceDict =
    Dict.Dict Color ZonePieceDict


type Turn
    = TurnIdle
    | TurnInProgress
    | TurnCard
        { active_card : Card
        }
    | TurnDone


type alias Player =
    { deck : List Card
    , hand : List Card
    , discard_pile : List Card
    , turn : Turn
    }


type alias PlayerDict =
    Dict.Dict Color Player


type alias Model =
    { zone_colors : List Color
    , piece_map : PieceDict
    , status : String
    , active_square : Maybe SquareKey
    , players : PlayerDict
    }
