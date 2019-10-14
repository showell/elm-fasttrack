module Graph exposing
    ( canTravelNEdges
    , getNodesNEdgesAway
    )


getNodesNEdgesAway : (a -> List a) -> Int -> a -> List a
getNodesNEdgesAway getNeighbors n node =
    if n == 0 then
        [ node ]

    else
        getNeighbors node
            |> List.map (getNodesNEdgesAway getNeighbors (n - 1))
            |> List.concat


canTravelNEdges : (a -> List a) -> Int -> a -> Bool
canTravelNEdges getNeighbors n node =
    if n <= 0 then
        True

    else
        List.any (canTravelNEdges getNeighbors (n - 1)) (getNeighbors node)
