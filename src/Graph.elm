module Graph exposing
    ( canTravelNEdges
    , getFinalStates
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


getFinalStates : (state -> List state) -> (state -> Bool) -> state -> List state
getFinalStates neighborStates isFinal startState =
    {--
        This is basically getting leaf nodes of a directed graph,
        where each node is a "state" that is either final
        (i.e. no edges) or has neighbors.  We depend on
        caller to pass us sensible functions that implicitly
        represent a tree of states (i.e. finite, no cycles).
    --}
    if isFinal startState then
        [ startState ]

    else
        neighborStates startState
            |> List.map (getFinalStates neighborStates isFinal)
            |> List.concat
