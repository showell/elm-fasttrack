module Player exposing
    ( activateCard
    , beginTurn
    , clearCredits
    , configPlayers
    , coverCard
    , discardCard
    , endLocsForPlayer
    , finishMove
    , getPlayableCards
    , getPlayer
    , getPlayerMoveType
    , getStartLocation
    , maybeReplenishHand
    , replenishHand
    , setStartLocation
    , setTurn
    , startLocsForPlayer
    , updateActivePlayer
    )

import Config
    exposing
        ( fullDeck
        )
import Dict
import LegalMove
    exposing
        ( getCardForMoveType
        , getMovesForCards
        , getMovesForMoveType
        )
import List.Extra
import Random
import Set
import Setup
    exposing
        ( startingHand
        )
import Type
    exposing
        ( Card
        , Color
        , Model
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , PlayerDict
        , Turn(..)
        )


getPlayableCards : Player -> Set.Set Card
getPlayableCards player =
    case player.turn of
        TurnNeedCard info ->
            info.moves
                |> List.map (\( moveType, _, _ ) -> moveType)
                |> List.map getCardForMoveType
                |> Set.fromList

        _ ->
            Set.empty


getPlayerMoveType : Player -> PieceLocation -> Maybe MoveType
getPlayerMoveType player endLoc =
    case player.turn of
        TurnNeedEndLoc info ->
            let
                moves =
                    info.moves
                        |> List.filter (\( _, _, end ) -> end == endLoc)

                moveTypes =
                    moves
                        |> List.map (\( moveType, _, _ ) -> moveType)

                theMoveType =
                    case List.length moveTypes of
                        1 ->
                            List.head moveTypes

                        2 ->
                            -- The only way we'd have two possible ways
                            -- to move the same piece between the same two
                            -- locations is via a jack (we're either moving
                            -- forward one or trading).  For now we just
                            -- arbitrarily pick the first option, which is
                            -- gonna be to move forward (and trade, not kill,
                            -- if there happens to be a piece there).
                            --
                            -- TODO: make this way more rigourous (and let user
                            --       choose)
                            List.head moveTypes

                        _ ->
                            -- programming error
                            Nothing
            in
            theMoveType

        _ ->
            -- programming error
            Nothing


configPlayer : Color -> Player
configPlayer color =
    let
        -- startingHand is usually empty, unless
        -- we turn on devHack
        originalSetup =
            { deck = fullDeck
            , hand = startingHand color
            , getOutCredits = 0
            , turn = TurnBegin
            }
    in
    originalSetup


configPlayers : List Color -> PlayerDict
configPlayers zoneColors =
    let
        configOne color =
            Dict.insert color (configPlayer color)

        dct =
            Dict.empty
    in
    List.foldl configOne dct zoneColors


getPlayer : PlayerDict -> Color -> Player
getPlayer players color =
    -- The "Maybe" is just to satisfy the compiler
    Dict.get color players
        |> Maybe.withDefault (configPlayer color)


isMoveAgainCard : Card -> Bool
isMoveAgainCard card =
    List.member card [ "A", "K", "Q", "J", "joker", "6" ]


getMovesForPlayer : Player -> PieceDict -> List Color -> Color -> List Move
getMovesForPlayer player pieceMap zoneColors activeColor =
    let
        cards =
            Set.fromList player.hand
    in
    getMovesForCards cards pieceMap zoneColors activeColor


turnNeedCard : PieceDict -> List Color -> Color -> Player -> Player
turnNeedCard pieceMap zoneColors activeColor player =
    let
        moves =
            getMovesForPlayer player pieceMap zoneColors activeColor
    in
    if List.length moves == 0 then
        let
            turn_ =
                case player.turn of
                    TurnBegin ->
                        TurnNeedDiscard

                    _ ->
                        TurnNeedCover
        in
        { player
            | turn = turn_
        }

    else
        { player
            | turn =
                TurnNeedCard
                    { moves = moves
                    }
            , getOutCredits = 0
        }


maybeFinishTurn : Card -> PieceDict -> List Color -> Color -> Player -> Player
maybeFinishTurn card pieceMap zoneColors activeColor player =
    if isMoveAgainCard card then
        player
            |> turnNeedCard pieceMap zoneColors activeColor

    else
        { player
            | turn = TurnDone
        }


maybeReplenishHand : Color -> Model -> Model
maybeReplenishHand activeColor model =
    let
        player =
            getPlayer model.players activeColor
    in
    if List.length player.hand > 0 then
        model

    else
        replenishHand activeColor model


setTurnToNeedCard : Color -> Model -> Model
setTurnToNeedCard activeColor model =
    updateActivePlayer
        (\player ->
            let
                pieceMap =
                    model.pieceMap

                zoneColors =
                    model.zoneColors
            in
            player
                |> turnNeedCard pieceMap zoneColors activeColor
        )
        model


finishSevenSplit : PieceDict -> List Color -> Color -> Int -> PieceLocation -> Turn
finishSevenSplit pieceMap zoneColors activeColor distance endLoc =
    let
        moveCount =
            7 - distance

        -- We exclude the piece we just moved from the subsequent split.  We
        -- keep track of the piece by its location.
        excludeLoc =
            endLoc

        playType =
            FinishSeven moveCount

        moveType =
            FinishSplit moveCount excludeLoc

        moves =
            getMovesForMoveType moveType pieceMap zoneColors activeColor

        startLocs =
            moves
                |> List.map (\( _, start, _ ) -> start)
                |> Set.fromList
    in
    TurnNeedStartLoc
        { playType = playType
        , moves = moves
        , startLocs = startLocs
        }


clearCredits : Player -> Player
clearCredits player =
    let
        newTurn =
            case player.turn of
                TurnNeedDiscard ->
                    TurnNeedCover

                other ->
                    other
    in
    { player
        | getOutCredits = 0
        , turn = newTurn
    }


finishMove : PieceDict -> List Color -> Color -> Move -> Player -> Player
finishMove pieceMap zoneColors activeColor move player =
    let
        ( moveType, _, endLoc ) =
            move

        card =
            getCardForMoveType moveType
    in
    case moveType of
        StartSplit distance ->
            let
                turn =
                    finishSevenSplit pieceMap zoneColors activeColor distance endLoc
            in
            { player | turn = turn }

        _ ->
            player
                |> maybeFinishTurn card pieceMap zoneColors activeColor


setStartLocation : PieceLocation -> Player -> Player
setStartLocation startLoc player =
    case player.turn of
        TurnNeedStartLoc info ->
            let
                playType =
                    info.playType

                moves =
                    info.moves
                        |> List.filter (\( _, start, _ ) -> start == startLoc)

                endLocs =
                    moves
                        |> List.map (\( _, _, end ) -> end)
                        |> Set.fromList

                turn =
                    TurnNeedEndLoc
                        { playType = playType
                        , moves = moves
                        , startLocation = startLoc
                        , endLocs = endLocs
                        }
            in
            { player | turn = turn }

        _ ->
            player


getStartLocation : Player -> Maybe PieceLocation
getStartLocation player =
    case player.turn of
        TurnNeedEndLoc info ->
            Just info.startLocation

        _ ->
            Nothing


updateActivePlayer : (Player -> Player) -> Model -> Model
updateActivePlayer f model =
    let
        activeColor =
            model.getActiveColor model.zoneColors

        players =
            updatePlayer model.players activeColor f
    in
    { model | players = players }


updatePlayer : PlayerDict -> Color -> (Player -> Player) -> PlayerDict
updatePlayer players color f =
    let
        player =
            getPlayer players color

        newPlayer =
            f player
    in
    Dict.insert color newPlayer players


setTurn : Color -> Turn -> PlayerDict -> PlayerDict
setTurn color turn players =
    updatePlayer
        players
        color
        (\player ->
            { player
                | turn = turn
            }
        )


discardCard : Int -> Player -> Player
discardCard idx player =
    case List.Extra.getAt idx player.hand of
        Nothing ->
            -- some kind of programming error or race
            -- condition must have happened
            player

        Just card ->
            case player.turn of
                TurnNeedDiscard ->
                    let
                        newHand =
                            List.Extra.removeAt idx player.hand

                        turn =
                            if isMoveAgainCard card then
                                -- we can rely on the caller to move us to
                                -- TurnNeedCover if we have enough credits to get
                                -- out of the pen
                                TurnNeedDiscard

                            else
                                TurnDone

                        credits =
                            player.getOutCredits + 1
                    in
                    { player
                        | hand = newHand
                        , turn = turn
                        , getOutCredits = credits
                    }

                _ ->
                    -- programming error
                    player


coverCard : Int -> Player -> Player
coverCard idx player =
    case List.Extra.getAt idx player.hand of
        Nothing ->
            -- some kind of programming error or race
            -- condition must have happened
            player

        Just card ->
            case player.turn of
                TurnNeedCover ->
                    let
                        newHand =
                            List.Extra.removeAt idx player.hand

                        turn =
                            if isMoveAgainCard card then
                                TurnNeedCover

                            else
                                TurnDone
                    in
                    { player
                        | hand = newHand
                        , turn = turn
                    }

                _ ->
                    -- programming error
                    player


activateCard : Int -> Player -> Player
activateCard idx player =
    case List.Extra.getAt idx player.hand of
        Nothing ->
            -- some kind of programming error or race
            -- condition must have happened
            player

        Just activeCard ->
            case player.turn of
                TurnNeedCard info ->
                    let
                        newHand =
                            List.Extra.removeAt idx player.hand

                        goodMove move =
                            let
                                ( moveType, _, _ ) =
                                    move
                            in
                            getCardForMoveType moveType == activeCard

                        moves =
                            info.moves
                                |> List.filter goodMove

                        startLocs =
                            moves
                                |> List.map (\( _, start, _ ) -> start)
                                |> Set.fromList

                        turn =
                            TurnNeedStartLoc
                                { playType = PlayCard activeCard
                                , moves = moves
                                , startLocs = startLocs
                                }
                    in
                    { player
                        | hand = newHand
                        , turn = turn
                    }

                _ ->
                    -- programming error, we should only move to this
                    -- state from TurnNeedCard
                    player


maybeReplenishDeck : List Card -> List Card
maybeReplenishDeck deck =
    case List.length deck of
        0 ->
            fullDeck

        _ ->
            deck


replenishHand : Color -> Model -> Model
replenishHand activeColor model =
    let
        players =
            model.players

        activePlayer =
            getPlayer model.players activeColor
    in
    if List.length activePlayer.hand >= 5 then
        -- The length of the hand should never actually
        -- exceed 5, but we want to prevent infinite loops
        -- for devHack kind of stuff.
        model

    else
        let
            ( idx, seed ) =
                getCardIdx activePlayer model.seed

            players_ =
                updatePlayer players activeColor (drawCard idx)

            model_ =
                { model
                    | players = players_
                    , seed = seed
                }
        in
        replenishHand activeColor model_


beginTurn : Color -> Model -> Model
beginTurn activeColor model =
    model
        |> replenishHand activeColor
        |> setTurnToNeedCard activeColor


getCardIdx : Player -> Random.Seed -> ( Int, Random.Seed )
getCardIdx player seed =
    let
        deckCount =
            List.length player.deck

        max =
            deckCount - 1
    in
    Random.step (Random.int 0 max) seed


drawCard : Int -> Player -> Player
drawCard idx player =
    case List.Extra.getAt idx player.deck of
        Nothing ->
            -- this should never happen...idx should always be valid
            player

        Just card ->
            let
                hand =
                    List.append player.hand [ card ]

                deck =
                    List.Extra.removeAt idx player.deck
            in
            { player
                | deck = maybeReplenishDeck deck
                , hand = hand
            }


startLocsForPlayer : Player -> Set.Set PieceLocation
startLocsForPlayer player =
    case player.turn of
        TurnNeedStartLoc info ->
            info.startLocs

        _ ->
            Set.empty


endLocsForPlayer : Player -> Set.Set PieceLocation
endLocsForPlayer player =
    case player.turn of
        TurnNeedEndLoc info ->
            info.endLocs

        _ ->
            Set.empty
