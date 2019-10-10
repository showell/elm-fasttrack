module Graph exposing
    ( can_travel_n_edges
    , get_nodes_n_edges_away
    )


get_nodes_n_edges_away : (a -> List a) -> Int -> a -> List a
get_nodes_n_edges_away get_neighbors n node =
    if n == 0 then
        [ node ]

    else
        get_neighbors node
            |> List.map (get_nodes_n_edges_away get_neighbors (n - 1))
            |> List.concat


can_travel_n_edges : (a -> List a) -> Int -> a -> Bool
can_travel_n_edges get_neighbors n node =
    if n <= 0 then
        True

    else
        List.any (can_travel_n_edges get_neighbors (n - 1)) (get_neighbors node)
