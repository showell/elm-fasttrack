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
    , TurnNeedEndLocInfo
    , TurnNeedStartLocInfo
    )

import Dict
import Random
import Set
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


type alias CardStartEnd =
    ( Card, PieceLocation, PieceLocation )


type alias PieceDict =
    Dict.Dict PieceLocation Color


type alias Move =
    { start : PieceLocation
    , end : PieceLocation
    , want_trade : Bool
    }


type MoveType
    = WithCard Card
    | Reverse Card
    | FinishSplit Int PieceLocation


type alias TurnNeedCardInfo =
    { moves : Set.Set CardStartEnd
    }


type alias TurnNeedStartLocInfo =
    { move_type : MoveType
    , moves : Set.Set ( PieceLocation, PieceLocation )
    , start_locs : Set.Set PieceLocation
    }


type alias TurnNeedEndLocInfo =
    { move_type : MoveType
    , start_location : PieceLocation
    , end_locs : Set.Set PieceLocation
    }


type Turn
    = TurnIdle
    | TurnBegin
    | TurnNeedCard TurnNeedCardInfo
    | TurnNeedStartLoc TurnNeedStartLocInfo
    | TurnNeedEndLoc TurnNeedEndLocInfo
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


type Msg
    = ReplenishHand
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
    | SetEndLocation PieceLocation
    | SetStartLocation PieceLocation
    | LoadGame Time.Posix
    | NewSeed Time.Posix
