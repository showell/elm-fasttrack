module Type
    exposing
        ( SquareKind(..)
        , Square
        , Color
        , Card
        , PieceLocation
        , PieceDict
        , TurnCardInfo
        , Turn(..)
        , Player
        , PlayerDict
        , UpdatePlayerFunc
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


type alias PieceLocation =
    ( Color, String )


type alias PieceDict =
    Dict.Dict PieceLocation Color


type alias TurnCardInfo =
    { active_card : Card
    , active_square : Maybe PieceLocation
    , move_error : Maybe String
    }


type Turn
    = TurnIdle
    | TurnInProgress
    | TurnCard TurnCardInfo
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
    , players : PlayerDict
    }


type alias UpdatePlayerFunc =
    (Player -> Player) -> Model
