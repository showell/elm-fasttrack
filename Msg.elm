module Msg
    exposing
        ( Msg(..)
        )

import Type
    exposing
        ( PieceLocation
        , Color
        )


type Msg
    = ClickSquare PieceLocation
    | DrawCard Color
    | DrawCardResult Color Int
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
