-- see https://github.com/elm-community/list-extra/blob/master/src/List/Extra.elm

module ListExtra
    exposing
        ( getAt
        , removeAt
        )

getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs

removeAt : Int -> List a -> List a
removeAt index l =
    if index < 0 then
        l

    else
        let
            head =
                List.take index l

            tail =
                List.drop index l |> List.tail
        in
        case tail of
            Nothing ->
                l

            Just t ->
                List.append head t
