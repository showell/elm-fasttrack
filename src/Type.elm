module Type exposing
    ( AppState(..)
    , Card
    , Color
    , FindLocParams
    , Location
    , Model
    , Move
    , MoveType(..)
    , Msg(..)
    , PieceDict
    , PieceLocation
    , PlayType(..)
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


type alias PieceDict =
    Dict.Dict PieceLocation Color



{--
    Why do we have PlayType and MoveType, instead of just a single
    type?

    Let's say you play a J or a 7, and then you click the starting
    location for the move.  There not only may be multiple moves
    available, but there may be multiple **types** of moves
    available.

    FWIW it's not impossible to unify these types--I had a version
    basically working with a single MoveType--but I think having
    two separate concepts ultimately is cleaner.

--}


type PlayType
    = PlayCard Card
    | FinishSeven Int


type MoveType
    = WithCard Card
    | Reverse Card
    | FinishSplit Int PieceLocation
    | JackTrade


type alias Move =
    ( MoveType, PieceLocation, PieceLocation )


type alias TurnNeedCardInfo =
    { moves : List Move
    }


type alias TurnNeedStartLocInfo =
    { play_type : PlayType
    , moves : List Move
    , start_locs : Set.Set PieceLocation
    }


type alias TurnNeedEndLocInfo =
    { play_type : PlayType
    , start_location : PieceLocation
    , moves : List Move
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
