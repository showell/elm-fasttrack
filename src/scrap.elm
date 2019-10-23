module Main exposing (Tree, TreeCollection, analyzeDict, cartesianMap, combineTwo, dictString, expandTrees, expandTwo, filterTrees, firstLine, letterFromIdx, permuteList, show, show1, show2, smallTrees, uniqueReverseMap)


firstLine s =
    s
        |> String.split "\n"
        |> List.head
        |> Maybe.withDefault ""


dictString : Dict String String -> Dict k v -> String
dictString aliases dct =
    let
        toRepr =
            internalRepresentationOfTheUnderlyingRedBlackTreeForTestingPurposes

        depth repr =
            case repr of
                InternalDictRepr Nothing ->
                    0

                InternalDictRepr (Just ( color, _, ( left, right ) )) ->
                    let
                        delta =
                            if color == "B" then
                                1

                            else
                                0
                    in
                    delta + depth left

        size repr =
            case repr of
                InternalDictRepr Nothing ->
                    0

                InternalDictRepr (Just ( color, _, ( left, right ) )) ->
                    1 + size left + size right

        remapRed s =
            if String.left 1 s == "R" then
                let
                    s2 =
                        "B" ++ String.dropLeft 1 s
                in
                case get s2 aliases of
                    Just v ->
                        "R" ++ v

                    Nothing ->
                        s

            else
                s

        remap s =
            s

        -- get s aliases
        --    |> Maybe.withDefault (remapRed s)
        reprToString repr =
            let
                helper color left right =
                    let
                        l =
                            left |> reprToString |> remap |> firstLine

                        r =
                            right |> reprToString |> remap |> firstLine

                        cnt =
                            size repr |> String.fromInt

                        dd =
                            depth repr |> String.fromInt
                    in
                    dd
                        ++ " "
                        ++ cnt
                        ++ " "
                        ++ color
                        ++ " ("
                        ++ l
                        ++ ", "
                        ++ r
                        ++ ")"
                        ++ "\n"
                        ++ l
                        ++ "\n"
                        ++ r
            in
            case repr of
                InternalDictRepr (Just ( color, _, ( left, right ) )) ->
                    case ( left, right ) of
                        ( InternalDictRepr Nothing, InternalDictRepr Nothing ) ->
                            color

                        ( InternalDictRepr Nothing, InternalDictRepr (Just ( "R", _, _ )) ) ->
                            -- Red is never a right child
                            "ERROR"

                        ( InternalDictRepr (Just ( "R", _, ( _, _ ) )), InternalDictRepr Nothing ) ->
                            if color == "R" then
                                "ERROR"

                            else
                                helper color left right

                        _ ->
                            helper color left right

                InternalDictRepr Nothing ->
                    "_"
    in
    dct |> toRepr |> reprToString


uniqueReverseMap : List ( input, comparable ) -> Dict comparable input
uniqueReverseMap tupList =
    -- Take a list of tuples and do a reverse mapping of
    -- output back to input, taking only the first example
    -- for each group of inputs that all have the same output.
    -- We use this to compress the list of things to test
    -- after each successive generation of bigger and bigger dicts.
    -- It helps us avoid combinatorial explosion.
    let
        f d candidates =
            case candidates of
                [] ->
                    d

                ( input, output ) :: rest ->
                    case get output d of
                        Just _ ->
                            -- we already have an example
                            -- for this particular representation
                            d

                        Nothing ->
                            f (insert output input d) rest
    in
    f empty tupList


permuteList : List String -> List (List String)
permuteList lst =
    let
        baseList =
            lst
                |> List.map (\s -> s ++ "1")

        successors =
            lst
                |> List.map (\s -> s ++ "2")

        newElements =
            "a" :: successors

        newLists =
            newElements
                |> List.map (\elem -> baseList ++ [ elem ])
    in
    newLists


letterFromIdx : Int -> String
letterFromIdx idx =
    List.Extra.getAt idx [ "a", "b", "c", "d", "e", "f" ]
        |> Maybe.withDefault "?"


analyzeDict : Int -> DictGeneration
analyzeDict maxN =
    let
        toReprTuple aliases lst =
            let
                dictRepr =
                    lst
                        |> List.map (\k -> ( k, "" ))
                        |> fromList
                        |> dictString aliases
            in
            ( lst, dictRepr )

        makeGeneration : DictGeneration -> SingleGenerationResult
        makeGeneration params =
            let
                aliases =
                    params.aliases

                n =
                    params.n

                prevLists =
                    params.interestingLists

                newLists =
                    prevLists
                        |> List.map permuteList
                        |> List.concat

                prefix =
                    "T" ++ String.fromInt n

                reprTuples =
                    newLists
                        |> List.map (toReprTuple aliases)

                reverseMap =
                    reprTuples
                        |> uniqueReverseMap

                interestingLists =
                    values reverseMap

                uniqueStrings =
                    reverseMap
                        |> keys
                        |> List.sort

                makeId idx =
                    prefix ++ letterFromIdx idx

                makeTup idx s =
                    ( s, makeId idx )

                newAliasTuples =
                    uniqueStrings
                        |> List.indexedMap makeTup

                humanOutput =
                    "\n\n==============\n"
                        ++ String.fromInt n
                        ++ " nodes:\n"
                        ++ (newAliasTuples
                                |> List.map (\( val, name ) -> "\n\n" ++ name ++ " =\n" ++ val)
                                |> List.foldr (++) ""
                           )
            in
            { humanOutput = humanOutput
            , aliases = fromList newAliasTuples
            , interestingLists = interestingLists
            }

        accum : DictGeneration -> DictGeneration
        accum params =
            if params.n > maxN then
                params

            else
                let
                    newTrees =
                        makeGeneration params

                    updatedHumanOutput =
                        params.humanOutput ++ newTrees.humanOutput

                    updatedAliases =
                        union
                            params.aliases
                            newTrees.aliases
                in
                accum
                    { humanOutput = updatedHumanOutput
                    , aliases = updatedAliases
                    , interestingLists = newTrees.interestingLists
                    , n = params.n + 1
                    }

        result =
            accum
                { interestingLists = [ [ "b" ] ]
                , aliases = empty
                , n = 2
                , humanOutput = ""
                }
    in
    result


type alias Tree =
    { count : Int
    , depth : Int
    , color : String
    , l : String
    , r : String
    , lColor : String
    , rColor : String
    , sig : String
    }


type alias TreeCollection =
    { trees : List Tree
    }


smallTrees : TreeCollection
smallTrees =
    { trees =
        [ { count = 1
          , depth = 1
          , color = "B"
          , l = "_"
          , r = "_"
          , lColor = "_"
          , rColor = "_"
          , sig = "B"
          }
        , { count = 2
          , depth = 1
          , color = "B"
          , l = "R"
          , r = "_"
          , lColor = "R"
          , rColor = "_"
          , sig = "BR_"
          }
        ]
    }


combineTwo : String -> Tree -> Tree -> Maybe Tree
combineTwo color left right =
    if left.depth /= right.depth then
        Nothing

    else if color == "R" && right.lColor == "R" then
        Nothing

    else if color == "B" && right.count > left.count then
        Nothing

    else if left.sig == "B (BR_, BR_)" || right.sig == "B (BR_, BR_)" then
        Nothing

    else
        let
            sig =
                color ++ " (" ++ left.sig ++ ", " ++ right.sig ++ ")"

            count =
                left.count + right.count + 1

            depth =
                if color == "B" then
                    left.depth + 1

                else
                    left.depth
        in
        Just
            { count = count
            , depth = depth
            , sig = sig
            , color = color
            , l = left.sig
            , r = right.sig
            , lColor = left.color
            , rColor = right.color
            }


cartesianMap : (a -> a -> b) -> List a -> List a -> List b
cartesianMap f lst1 lst2 =
    let
        expand elem =
            lst2 |> List.map (f elem)
    in
    lst1
        |> List.map expand
        |> List.concat


filterTrees : List (Maybe Tree) -> List Tree
filterTrees trees =
    trees
        |> List.filterMap identity
        |> List.filter (\t -> t.count < 12)


expandTwo : TreeCollection -> TreeCollection
expandTwo p =
    let
        f rb lst1 lst2 =
            cartesianMap (combineTwo rb) lst1 lst2
                |> filterTrees

        oldTrees =
            p.trees

        newBlack =
            f "B" oldTrees oldTrees

        newMixed =
            f "B" (f "R" oldTrees oldTrees) oldTrees
    in
    { trees = newBlack ++ newMixed
    }


expandTrees ( all, p ) =
    let
        newParams =
            expandTwo p

        newAll =
            all ++ newParams.trees
    in
    ( newAll, newParams )


show1 =
    let
        trees =
            ( [], smallTrees )
                |> expandTrees
                |> expandTrees
                |> expandTrees
                |> expandTrees
                |> expandTrees
                |> Tuple.first
                |> List.sortBy (\t -> t.count)
                |> List.map (\t -> ( t.count, t.l, t.r ))

        x =
            trees
                |> List.reverse
                |> List.map (Debug.log "tree")
    in
    Html.div [] []


show2 =
    analyzeDict 150
        |> (\params -> Html.pre [] [ Html.text params.humanOutput ])


show =
    let
        x =
            show1
    in
    show2
