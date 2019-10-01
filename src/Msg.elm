module Msg exposing (Msg(..))

import Time
import Type
    exposing
        ( Color
        , PieceLocation
        , Player
        )


type Msg
    = ClickLocation PieceLocation
    | ReplenishHand
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
    | LoadGame Time.Posix
    | NewSeed Time.Posix