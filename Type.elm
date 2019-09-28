module Type
    exposing
        ( SquareKind(..)
        , Turn(..)
        , AppState(..)
        , Square
        , Color
        , Card
        , PieceLocation
        , PieceDict
        , TurnCardInfo
        , Player
        , PlayerDict
        , UpdatePlayerFunc
        , Model
        )

import Dict
import Random


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
    , num_moves : Int
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


type AppState
    = Loading
    | Ready


type alias Model =
    { zone_colors : List Color
    , piece_map : PieceDict
    , players : PlayerDict
    , seed : Random.Seed
    , state : AppState
    }


type alias UpdatePlayerFunc =
    (Player -> Player) -> Model
