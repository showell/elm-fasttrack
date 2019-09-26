module Msg
    exposing
        ( Msg(..)
        )

import Type
    exposing
        ( PieceLocation
        , Color
        , Player
        )


type Msg
    = ClickSquare PieceLocation
    | DrawCard
    | DrawCardResult Int
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
