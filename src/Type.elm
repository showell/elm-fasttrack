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
    | StartSplit Int
    | FinishSplit Int PieceLocation
    | JackTrade
    | ComeOutWithCredits


type alias Move =
    ( MoveType, PieceLocation, PieceLocation )


type alias TurnNeedCardInfo =
    { moves : List Move
    }


type alias TurnNeedStartLocInfo =
    { playType : PlayType
    , moves : List Move
    , startLocs : Set.Set PieceLocation
    }


type alias TurnNeedEndLocInfo =
    { playType : PlayType
    , startLocation : PieceLocation
    , moves : List Move
    , endLocs : Set.Set PieceLocation
    }


type Turn
    = TurnIdle
    | TurnBegin
    | TurnNeedCard TurnNeedCardInfo
    | TurnNeedStartLoc TurnNeedStartLocInfo
    | TurnNeedEndLoc TurnNeedEndLocInfo
    | TurnNeedDiscard
    | TurnNeedCover
    | TurnDone


type alias Player =
    { deck : List Card
    , hand : List Card
    , getOutCredits : Int
    , turn : Turn
    }


type alias PlayerDict =
    Dict.Dict Color Player


type AppState
    = Loading
    | Ready


type alias FindLocParams =
    { canFastTrack : Bool
    , canLeavePen : Bool
    , canLeaveBullsEye : Bool
    , reverseMode : Bool
    , pieceColor : Color
    , pieceMap : PieceDict
    , zoneColors : List Color
    }


type alias Model =
    { zoneColors : List Color
    , pieceMap : PieceDict
    , players : PlayerDict
    , seed : Random.Seed
    , state : AppState
    , getActiveColor : List Color -> Color
    }


type Msg
    = ActivateCard Color Int
    | DiscardCard Color Int
    | CoverCard Color Int
    | RotateBoard
    | SetEndLocation PieceLocation
    | SetStartLocation PieceLocation
    | LoadGame Time.Posix
