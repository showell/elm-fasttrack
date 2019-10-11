module Setup exposing
    ( starting_hand
    , starting_locations
    )

import Config
    exposing
        ( holding_pen_locations
        )
import Type
    exposing
        ( Card
        , Color
        , PieceLocation
        )



{--
This dev_hack code is for developers...if you turn it on,
it sets up pieces in "interesting" configurations to
facilitate manual testing of scenarios like splitting
sevens, trading jacks, and being forced to go in
reverse.

Long term we might want to expose this in some kind
of tutorial/demo thingy.
--}


type InitSetup
    = Normal
    | ForcedToReverse


init_setup : InitSetup
init_setup =
    Normal


starting_locations : Color -> List PieceLocation
starting_locations color =
    let
        in_my_zone ids =
            ids
                |> List.map (\id -> ( color, id ))
    in
    case init_setup of
        ForcedToReverse ->
            [ "HP1", "B1", "B3", "R0" ]
                |> in_my_zone

        _ ->
            holding_pen_locations
                |> in_my_zone


starting_hand : Color -> List Card
starting_hand color =
    case init_setup of
        ForcedToReverse ->
            [ "7", "8", "10", "9", "9" ]

        _ ->
            []
