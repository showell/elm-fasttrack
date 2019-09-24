module Msg
    exposing
        ( Msg(..)
        )

import Type
    exposing
        ( SquareKey
        , Color
        )


type Msg
    = ClickSquare SquareKey
    | DrawCard Color
    | DrawCardResult Color Int
    | ActivateCard Color Int
    | FinishCard Color
    | RotateBoard
