module Type exposing
    ( AppState(..)
    , Card
    , Color
    , FindLocParams
    , Game
    , GameMsg(..)
    , Location
    , Model
    , Move
    , MoveFlavor(..)
    , MoveType(..)
    , Msg(..)
    , PieceLocation
    , PieceMap
    , PlayType(..)
    , Player
    , PlayerDict
    , Turn(..)
    , TurnNeedEndLocInfo
    , TurnNeedStartLocInfo
    , Zone(..)
    )

import AssocList
import AssocSet as Set exposing (Set)
import Dict exposing (Dict)
import History exposing (History)
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


type Zone
    = NormalColor String
    | BullsEyeZone


type alias PieceLocation =
    ( Zone, String )


type alias PieceMap =
    AssocList.Dict PieceLocation Color



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


type MoveFlavor
    = RegularMove
    | TradePieces


type alias Move =
    ( MoveType, PieceLocation, PieceLocation )


type alias TurnNeedCardInfo =
    { moves : List Move
    }


type alias TurnNeedStartLocInfo =
    { playType : PlayType
    , moves : List Move
    , startLocs : Set PieceLocation
    }


type alias TurnNeedEndLocInfo =
    { playType : PlayType
    , startLocation : PieceLocation
    , moves : List Move
    , endLocs : Set PieceLocation
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
    , color : Color
    }


type alias PlayerDict =
    Dict Int Player


type AppState
    = Loading
    | Ready


type alias FindLocParams =
    { canFastTrack : Bool
    , canLeavePen : Bool
    , canLeaveBullsEye : Bool
    , reverseMode : Bool
    , pieceColor : Color
    , pieceMap : PieceMap
    , zoneColors : List Color
    }


type alias Game =
    { zoneColors : List Color
    , pieceMap : PieceMap
    , players : PlayerDict
    , seed : Random.Seed
    , activePlayerIdx : Int
    , numPlayers : Int
    }


type alias Model =
    { game : Maybe Game
    , history : History Game
    }


type GameMsg
    = ActivateCard Int
    | DiscardCard Int
    | CoverCard Int
    | RotateBoard
    | SetEndLocation PieceLocation
    | SetStartLocation PieceLocation
    | UndoAction


type Msg
    = BeginGame Time.Posix
    | UpdateGame GameMsg
