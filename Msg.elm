module Msg
    exposing
        ( Msg(..)
        )

import Time
import Type
    exposing
        ( PieceLocation
        , Color
        , Player
        )


type Msg
    = ClickSquare PieceLocation
    | ReplenishHand
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
    | LoadGame Time.Posix
    | NewSeed Time.Posix
