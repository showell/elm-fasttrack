module Type exposing
    ( SquareKind(..)
    , Square
    , Zone
    , SquareKey
    )

type SquareKind
    = HoldingPen
    | Normal
    | FastTrack
    | DoorStep
    | HideyHole
    | Base

type alias Square =
    { x : Float, y : Float, kind: SquareKind, id: String }

type alias Zone =
    { squares: List Square
    , color: String
    , angle: Float
    }

type alias SquareKey =
    { zone_color : String
    , id: String
    -- kind is actually not an attribute, not part of the key
    , kind: SquareKind
    }

