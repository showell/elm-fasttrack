module Player exposing
    ( activateCard
    , clearCredits
    , configPlayers
    , coverCard
    , discardCard
    , endLocsForPlayer
    , ensureHandNotEmpty
    , finishMove
    , getActivePlayer
    , getActivePlayerColor
    , getPlayableCards
    , getPlayer
    , getPlayerMoveType
    , getStartLocation
    , replenishHand
    , setStartLocation
    , setTurn
    , setTurnToNeedCard
    , startLocsForPlayer
    , updateActivePlayer
    )

import Config
    exposing
        ( fullDeck
        , isMoveAgainCard
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
import Set exposing (Set)
import Setup
    exposing
        ( startingHand
        )
import Type
    exposing
        ( Card
        , Color
        , Game
        , Move
        , MoveType(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , PlayerDict
        , Turn(..)
        )


getPlayableCards : Player -> Set Card
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
            , color = color
            }
    in
    originalSetup


configPlayers : List Color -> PlayerDict
configPlayers zoneColors =
    let
        lst =
            zoneColors
                |> List.indexedMap
                    (\idx color ->
                        ( idx, configPlayer color )
                    )
    in
    Dict.fromList lst


getPlayer : PlayerDict -> Int -> Player
getPlayer players idx =
    -- The "Maybe" is just to satisfy the compiler
    Dict.get idx players
        |> Maybe.withDefault (configPlayer "bogus")


getActivePlayer : Game -> Player
getActivePlayer game =
    getPlayer game.players game.activePlayerIdx


getActivePlayerColor : Game -> Color
getActivePlayerColor game =
    (getActivePlayer game).color


getMovesForPlayer : Player -> PieceDict -> List Color -> List Move
getMovesForPlayer player pieceMap zoneColors =
    let
        activeColor =
            player.color

        cards =
            Set.fromList player.hand
    in
    getMovesForCards cards pieceMap zoneColors activeColor


turnNeedCard : PieceDict -> List Color -> Player -> Player
turnNeedCard pieceMap zoneColors player =
    let
        moves =
            getMovesForPlayer player pieceMap zoneColors
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


maybeFinishTurn : Card -> PieceDict -> List Color -> Player -> Player
maybeFinishTurn card pieceMap zoneColors player =
    if isMoveAgainCard card then
        player
            |> turnNeedCard pieceMap zoneColors

    else
        { player
            | turn = TurnDone
        }


ensureHandNotEmpty : Game -> Game
ensureHandNotEmpty game =
    let
        activeIdx =
            game.activePlayerIdx

        player =
            getPlayer game.players activeIdx
    in
    if List.length player.hand > 0 then
        game

    else
        replenishHand game


setTurnToNeedCard : Game -> Game
setTurnToNeedCard game =
    updateActivePlayer
        (\player ->
            let
                pieceMap =
                    game.pieceMap

                zoneColors =
                    game.zoneColors
            in
            player
                |> turnNeedCard pieceMap zoneColors
        )
        game


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


finishMove : PieceDict -> List Color -> Move -> Player -> Player
finishMove pieceMap zoneColors move player =
    let
        activeColor =
            player.color

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
                |> maybeFinishTurn card pieceMap zoneColors


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


updateActivePlayer : (Player -> Player) -> Game -> Game
updateActivePlayer f game =
    let
        activeIdx =
            game.activePlayerIdx

        players =
            updatePlayer game.players activeIdx f
    in
    { game | players = players }


updatePlayer : PlayerDict -> Int -> (Player -> Player) -> PlayerDict
updatePlayer players idx f =
    let
        player =
            getPlayer players idx

        newPlayer =
            f player
    in
    Dict.insert idx newPlayer players


setTurn : Int -> Turn -> PlayerDict -> PlayerDict
setTurn idx turn players =
    updatePlayer
        players
        idx
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


replenishHand : Game -> Game
replenishHand game =
    let
        activeIdx =
            game.activePlayerIdx

        players =
            game.players

        activePlayer =
            getPlayer game.players activeIdx
    in
    if List.length activePlayer.hand >= 5 then
        -- The length of the hand should never actually
        -- exceed 5, but we want to prevent infinite loops
        -- for devHack kind of stuff.
        game

    else
        let
            ( idx, seed ) =
                getCardIdx activePlayer game.seed

            players_ =
                updatePlayer players activeIdx (drawCard idx)

            game_ =
                { game
                    | players = players_
                    , seed = seed
                }
        in
        replenishHand game_


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


startLocsForPlayer : Player -> Set PieceLocation
startLocsForPlayer player =
    case player.turn of
        TurnNeedStartLoc info ->
            info.startLocs

        _ ->
            Set.empty


endLocsForPlayer : Player -> Set PieceLocation
endLocsForPlayer player =
    case player.turn of
        TurnNeedEndLoc info ->
            info.endLocs

        _ ->
            Set.empty
