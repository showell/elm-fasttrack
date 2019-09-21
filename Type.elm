module Type exposing
    ( SquareKind(..)
    , Square
    , Zone
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

