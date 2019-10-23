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


type DescribeTree
    = Broken
    | Nada
    | Tree1 Int
    | Tree2 Int Int
    | Tree3 Int Int Int


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


unboxStats (StatsTree stats) =
    stats


listToDescription lst =
    let
        stats =
            listToStats lst |> unboxStats

        subTree f tree =
            tree
                |> f
                |> Maybe.withDefault emptyStatsTree
                |> unboxStats

        lColor =
            stats.left
                |> Maybe.map unboxStats
                |> Maybe.map .color

        rColor =
            stats.right
                |> Maybe.map unboxStats
                |> Maybe.map .color

    in
    case ( lColor, rColor ) of
        ( Nothing, Nothing ) ->
            Nada

        ( Just "B", Nothing ) ->
            Tree1 stats.size

        ( Just "B", Just "B" ) ->
            let
                lSize =
                    stats
                        |> subTree .left
                        |> .size

                rSize =
                    stats
                        |> subTree .right
                        |> .size
            in
            Tree2 lSize rSize

        ( Just "R", Just "B" ) ->
            let
                llSize =
                    stats
                        |> subTree .left
                        |> subTree .left
                        |> .size
                lrSize =
                    stats
                        |> subTree .left
                        |> subTree .right
                        |> .size

                rSize =
                    stats
                        |> subTree .right
                        |> .size
            in
            Tree3 llSize lrSize rSize

        _ ->
            Broken


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

                left =
                    shapeToStats lShape |> unboxStats

                right =
                    shapeToStats rShape |> unboxStats

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
                |> listToDescription

        x =
            Debug.log "repr" internals
    in
    "see debugger"
