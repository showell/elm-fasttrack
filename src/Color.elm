module Color exposing
    ( getActiveColor
    , getZoneColors
    , nextZoneColor
    , prevZoneColor
    , rotateList
    )

import List.Extra
import Type
    exposing
        ( Color
        )


getZoneColors : Int -> List String
getZoneColors numPlayers =
    List.take numPlayers [ "red", "blue", "green", "purple", "aqua", "brown" ]


getActiveColor : List Color -> Color
getActiveColor zoneColors =
    -- appease compiler with Maybe
    List.head zoneColors
        |> Maybe.withDefault "bogus"


rotateList : Int -> List a -> List a
rotateList idx lst =
    List.drop idx lst ++ List.take idx lst


nextZoneColor : Color -> List Color -> Color
nextZoneColor color zoneColors =
    let
        idx =
            List.Extra.elemIndex color zoneColors
                |> Maybe.withDefault -1

        len =
            List.length zoneColors

        nextIdx =
            (idx + 1) |> modBy len
    in
    List.Extra.getAt nextIdx zoneColors
        |> Maybe.withDefault "bogus"


prevZoneColor : Color -> List Color -> Color
prevZoneColor color zoneColors =
    let
        idx =
            List.Extra.elemIndex color zoneColors
                |> Maybe.withDefault 1

        len =
            List.length zoneColors

        nextIdx =
            (idx - 1) |> modBy len
    in
    List.Extra.getAt nextIdx zoneColors
        |> Maybe.withDefault "bogus"
