module Type exposing
    ( AppState(..)
    , Card
    , CardStartEnd
    , Color
    , FindLocParams
    , Location
    , Model
    , Move
    , MoveType(..)
    , Msg(..)
    , PieceDict
    , PieceLocation
    , Player
    , PlayerDict
    , Turn(..)
    , TurnCardInfo
    , UpdatePlayerFunc
    )

import Dict
import Random
import Time


type alias Color =
    String


type alias Card =
    String


type alias Location =
    { x : Float
    , y : Float
    , id : String
    }


type alias PieceLocation =
    ( Color, String )


type alias PieceDict =
    Dict.Dict PieceLocation Color


type alias Move =
    { start : PieceLocation
    , end : PieceLocation
    , want_trade : Bool
    }


type MoveType
    = WithCard Card
    | ForceReverse Card
    | ForceCount Int


type alias TurnCardInfo =
    { active_card : Card
    , active_location : Maybe PieceLocation
    , num_moves : Int
    , distance_moved : Int
    }


type Turn
    = TurnIdle
    | TurnNeedCard
    | TurnCard TurnCardInfo
    | TurnDone


type alias Player =
    { deck : List Card
    , hand : List Card
    , discard_pile : List Card
    , turn : Turn
    }


type alias CardStartEnd =
    ( Card, PieceLocation, PieceLocation )


type alias PlayerDict =
    Dict.Dict Color Player


type AppState
    = Loading
    | Ready


type alias FindLocParams =
    { can_fast_track : Bool
    , can_leave_pen : Bool
    , reverse_mode : Bool
    , moves_left : Int
    , loc : PieceLocation
    , piece_color : Color
    , piece_map : PieceDict
    , zone_colors : List Color
    }


type alias Model =
    { zone_colors : List Color
    , piece_map : PieceDict
    , players : PlayerDict
    , seed : Random.Seed
    , state : AppState
    , get_active_color : List Color -> Color
    }


type alias UpdatePlayerFunc =
    (Player -> Player) -> Model


type Msg
    = ClickLocation PieceLocation
    | ReplenishHand
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
    | LoadGame Time.Posix
    | NewSeed Time.Posix
