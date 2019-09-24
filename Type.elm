module Type
    exposing
        ( SquareKind(..)
        , Square
        , SquareKey
        , Color
        , Card
        )


-- TODO: use this more places (even though it
-- doesn't really guarantee much type safety, it
-- at least documents our intent)
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
