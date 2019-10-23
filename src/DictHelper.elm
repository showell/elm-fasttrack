module DictHelper exposing (..)

import NewDict exposing (..)


type alias Color =
    String


type alias InternalNode k v =
    { k : k
    , v : v
    , color : Color
    , path : String
    }


type ShapeTree
    = Node String ShapeTree ShapeTree
    | Empty


type StatsTree
    = StatsTree
        { depth : Int
        , blackDepth : Int
        , size : Int
        , color : Color
        , left : Maybe StatsTree
        , right : Maybe StatsTree
        , sig : String
        }


emptyStatsTree : StatsTree
emptyStatsTree =
    StatsTree
        { depth = 0
        , blackDepth = 0
        , size = 0
        , color = "_"
        , left = Nothing
        , right = Nothing
        , sig = "_"
        }


listToStats : List comparable -> StatsTree
listToStats lst =
    lst
        |> List.map (\k -> ( k, "" ))
        |> fromList
        |> toInternalRepresentation
        |> toShapeTree
        |> shapeToStats


shapeToStats : ShapeTree -> StatsTree
shapeToStats shapeTree =
    case shapeTree of
        Empty ->
            emptyStatsTree

        Node color lShape rShape ->
            let
                box stats =
                    Just (StatsTree stats)

                unbox (StatsTree stats) =
                    stats

                left =
                    shapeToStats lShape |> unbox

                right =
                    shapeToStats rShape |> unbox

                depth =
                    1 + min left.depth right.depth

                blackDepth =
                    1 + left.blackDepth

                size =
                    1 + left.size + right.size

                sig =
                    color
                        ++ " ("
                        ++ left.sig
                        ++ ", "
                        ++ right.sig
                        ++ ")"
            in
            StatsTree
                { depth = depth
                , blackDepth = blackDepth
                , size = size
                , color = color
                , left = box left
                , right = box right
                , sig = sig
                }


toShapeTree : List (InternalNode k v) -> ShapeTree
toShapeTree internals =
    let
        top_ =
            internals
                |> List.filter (\node -> String.left 1 node.path == "")
                |> List.head
    in
    case top_ of
        Nothing ->
            Empty

        Just top ->
            let
                slicePath internalNode =
                    { internalNode
                        | path = String.dropLeft 1 internalNode.path
                    }

                getSubTree dir =
                    internals
                        |> List.filter (\node -> String.left 1 node.path == dir)
                        |> List.map slicePath
                        |> toShapeTree
            in
            Node
                top.color
                (getSubTree "l")
                (getSubTree "r")


show =
    let
        internals =
            [ 0, 1, 2, 3, 4 ]
                |> listToStats

        x =
            Debug.log "repr" internals
    in
    "see debugger"
