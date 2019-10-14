module Setup exposing
    ( startingHand
    , startingLocations
    )

import Config
    exposing
        ( holdingPenLocations
        )
import Type
    exposing
        ( Card
        , Color
        , PieceLocation
        )



{--
This devHack code is for developers...if you turn it on,
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
    | Discard
    | Cover
    | BullsEye


initSetup : InitSetup
initSetup =
    Normal


startingLocations : Color -> List PieceLocation
startingLocations color =
    let
        inMyZone ids =
            ids
                |> List.map (\id -> ( color, id ))
    in
    case initSetup of
        ForcedToReverse ->
            [ "HP1", "B1", "B3", "R0" ]
                |> inMyZone

        Cover ->
            [ "HP1", "HP2", "HP3", "B2" ]
                |> inMyZone

        _ ->
            holdingPenLocations
                |> inMyZone


startingHand : Color -> List Card
startingHand color =
    case initSetup of
        ForcedToReverse ->
            [ "7", "8", "10", "9", "9" ]

        Cover ->
            [ "K", "Q", "Q", "Q", "2" ]

        Discard ->
            [ "K", "Q", "Q", "Q", "8" ]

        BullsEye ->
            [ "A", "6", "Q", "8", "9" ]

        _ ->
            []
