module View exposing (gameView)

import AssocSet as Set exposing (Set)
import Color
    exposing
        ( rotateList
        )
import Config
    exposing
        ( cardValue
        , configLocations
        , gutterSize
        , hintForCard
        , isBaseId
        , isHoldingPenId
        , squareSize
        )
import Html
    exposing
        ( Html
        , b
        , br
        , button
        , div
        , hr
        , span
        )
import Html.Attributes
    exposing
        ( disabled
        , style
        )
import Html.Events
    exposing
        ( onClick
        )
import LegalMove
    exposing
        ( getCardForPlayType
        )
import Piece
    exposing
        ( getPiece
        )
import Player
    exposing
        ( endLocsForPlayer
        , getPlayableCards
        , getPlayer
        , getStartLocation
        , startLocsForPlayer
        )
import Polygon
    exposing
        ( getCenterOffset
        , makePolygon
        )
import Svg
    exposing
        ( circle
        , g
        , rect
        , svg
        )
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , height
        , r
        , rx
        , stroke
        , transform
        , width
        , x
        , y
        )
import Type
    exposing
        ( AppState(..)
        , Card
        , Color
        , Game
        , GameMsg(..)
        , Location
        , MoveType(..)
        , PieceLocation
        , PieceMap
        , PlayType(..)
        , Player
        , Turn(..)
        , Zone(..)
        )



-- VIEW


gameView : Game -> Bool -> Html GameMsg
gameView game showUndoButton =
    let
        pieceMap =
            game.pieceMap

        players =
            game.players

        activePlayerIdx =
            game.activePlayerIdx

        activePlayer =
            getPlayer players activePlayerIdx

        activeColor =
            activePlayer.color

        zoneColors =
            game.zoneColors
                |> rotateList activePlayerIdx

        board =
            div
                []
                [ boardView pieceMap zoneColors activePlayer activeColor ]

        undoButton =
            if showUndoButton then
                [ button [ onClick UndoAction ] [ Html.text "oops" ] ]

            else
                []

        playerConsole =
            playerView activePlayer activeColor undoButton

        cheatSheet =
            cheatSheetView activePlayer

        leftSide =
            div []
                [ board
                , hr [] []
                , playerConsole
                ]

        rightSide =
            cheatSheet
    in
    div [ style "display" "flex", style "flex-direction" "row" ]
        [ div [] [ leftSide ]
        , div [] [ rightSide ]
        ]


boardView : PieceMap -> List Color -> Player -> Color -> Html GameMsg
boardView pieceMap zoneColors activePlayer activeColor =
    let
        startLocation =
            getStartLocation activePlayer

        startLocs =
            startLocsForPlayer activePlayer

        endLocs =
            endLocsForPlayer activePlayer

        normalZones =
            zoneColors
                |> List.map NormalColor

        zoneLocations =
            List.map (zoneView pieceMap startLocs endLocs activeColor startLocation) normalZones
                |> makePolygon panelWidth panelHeight

        sideCount =
            List.length zoneColors

        centerOffset =
            getCenterOffset sideCount panelWidth panelHeight

        boardSize =
            String.fromFloat (2 * centerOffset + 3 * squareSize)

        bullsEye =
            bullsEyeView pieceMap startLocs endLocs activeColor startLocation centerOffset

        allLocations =
            [ zoneLocations, bullsEye ]
                |> g []
                |> nudge
    in
    svg
        [ width boardSize, height boardSize ]
        [ allLocations ]


panelWidth : Float
panelWidth =
    4 * squareSize


panelHeight : Float
panelHeight =
    5 * squareSize


nudge : Svg.Svg GameMsg -> Svg.Svg GameMsg
nudge board =
    let
        offset =
            String.fromFloat squareSize

        translate =
            "translate(" ++ offset ++ " " ++ offset ++ ")"
    in
    g [ transform translate ] [ board ]


zoneView : PieceMap -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Zone -> Html GameMsg
zoneView pieceMap startLocs endLocs activeColor startLocation zone =
    let
        locations =
            configLocations

        drawnLocations =
            List.map (locationView pieceMap zone startLocs endLocs activeColor startLocation) locations
    in
    g [] drawnLocations


locationView : PieceMap -> Zone -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Location -> Html GameMsg
locationView pieceMap zone startLocs endLocs activeColor selectedLocation locationInfo =
    let
        cx_ =
            locationInfo.x * squareSize

        cy_ =
            panelHeight - (locationInfo.y * squareSize)

        id =
            locationInfo.id
    in
    drawLocationAtCoords pieceMap zone id startLocs endLocs activeColor selectedLocation cx_ cy_


bullsEyeView : PieceMap -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Float -> Html GameMsg
bullsEyeView pieceMap startLocs endLocs activeColor selectedLocation centerOffset =
    let
        cx_ =
            centerOffset

        cy_ =
            centerOffset

        zone =
            BullsEyeZone

        id =
            "bullseye"
    in
    drawLocationAtCoords pieceMap zone id startLocs endLocs activeColor selectedLocation cx_ cy_


drawLocationAtCoords : PieceMap -> Zone -> String -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Float -> Float -> Html GameMsg
drawLocationAtCoords pieceMap zone id startLocs endLocs activeColor selectedLocation cx_ cy_ =
    let
        pieceLocation =
            ( zone, id )

        zoneColor =
            case zone of
                BullsEyeZone ->
                    "black"

                NormalColor color ->
                    color

        w =
            squareSize - gutterSize

        myPiece =
            getPiece pieceMap pieceLocation

        isMe =
            myPiece == Just activeColor

        isSelectedPiece =
            case
                selectedLocation
            of
                Just location ->
                    pieceLocation == location

                Nothing ->
                    False

        isStartLoc =
            Set.member pieceLocation startLocs

        isReachable =
            Set.member pieceLocation endLocs

        fillColor =
            if isSelectedPiece then
                "lightblue"

            else if isStartLoc then
                "lightcyan"

            else if isReachable then
                "lightgreen"

            else if isMe then
                "mintcream"

            else
                "white"

        strokeColor =
            if isStartLoc then
                "black"

            else
                zoneColor

        isRect =
            isHoldingPenId id || isBaseId id

        locHandlers =
            if isStartLoc then
                [ onClick (SetStartLocation pieceLocation)
                ]

            else if isReachable then
                [ onClick (SetEndLocation pieceLocation)
                ]

            else
                []

        sLocation =
            if isRect then
                let
                    h =
                        w

                    xpos =
                        cx_ - w / 2

                    ypos =
                        cy_ - h / 2
                in
                rect
                    ([ x (String.fromFloat xpos)
                     , y (String.fromFloat ypos)
                     , fill fillColor
                     , stroke strokeColor
                     , width (String.fromFloat w)
                     , height (String.fromFloat h)
                     , rx "2"
                     ]
                        ++ locHandlers
                    )
                    []

            else
                let
                    radius =
                        w / 2
                in
                circle
                    ([ cx (String.fromFloat cx_)
                     , cy (String.fromFloat cy_)
                     , fill fillColor
                     , stroke strokeColor
                     , r (String.fromFloat radius)
                     ]
                        ++ locHandlers
                    )
                    []

        sPieces =
            case myPiece of
                Just pieceColor ->
                    [ pieceView pieceColor isSelectedPiece isStartLoc cx_ cy_ locHandlers ]

                Nothing ->
                    []

        contents =
            sLocation :: sPieces
    in
    g [] contents


pieceView : Color -> Bool -> Bool -> Float -> Float -> List (Svg.Attribute GameMsg) -> Html GameMsg
pieceView color isSelectedPiece isStartLoc cx_ cy_ handlers =
    let
        radius =
            if isSelectedPiece then
                "7"

            else if isStartLoc then
                "6"

            else
                "4"

        attrs =
            [ cx (String.fromFloat cx_)
            , cy (String.fromFloat cy_)
            , fill color
            , stroke color
            , r radius
            ]
    in
    circle
        (attrs ++ handlers)
        []


cheatSheetCards : Player -> Set Card
cheatSheetCards player =
    case player.turn of
        TurnNeedCard _ ->
            getPlayableCards player

        TurnNeedStartLoc info ->
            getCardForPlayType info.playType
                |> Set.singleton

        _ ->
            Set.empty


playerView : Player -> Color -> List (Html GameMsg) -> Html GameMsg
playerView player color undoButton =
    let
        playableCards =
            getPlayableCards player

        handCards =
            List.indexedMap (handCardView color player playableCards) player.hand

        rotateButton =
            rotateButtonView player

        hand =
            span [] (handCards ++ undoButton ++ rotateButton)

        console =
            case player.turn of
                TurnNeedDiscard ->
                    div []
                        [ Html.text "click a card to discard"
                        ]

                TurnNeedCover ->
                    div []
                        [ Html.text "click a card to cover"
                        ]

                TurnNeedCard _ ->
                    playerNeedCard

                TurnNeedStartLoc turnInfo ->
                    playerNeedStart turnInfo.playType color

                TurnNeedEndLoc turnInfo ->
                    playerNeedEnd turnInfo.playType color

                TurnDone ->
                    div
                        []
                        [ Html.text "ok, now hit 'done' if you're happy"
                        ]

                _ ->
                    div [] []
    in
    div []
        [ hand
        , creditsView player
        , console
        ]


type CardAction
    = CanActivate
    | CanDiscard
    | CanCover
    | Ignore


handCardView : Color -> Player -> Set Card -> Int -> Card -> Html GameMsg
handCardView color player playableCards idx card =
    let
        action =
            case player.turn of
                TurnNeedCard _ ->
                    -- ideally, we should put playableCards on TurnNeedCard
                    if Set.member card playableCards then
                        CanActivate

                    else
                        Ignore

                TurnNeedDiscard ->
                    CanDiscard

                TurnNeedCover ->
                    CanCover

                _ ->
                    Ignore

        css =
            case action of
                CanActivate ->
                    cardCss color color

                CanDiscard ->
                    cardCss "gray" color

                CanCover ->
                    cardCss "gray" color

                Ignore ->
                    cardCss "gray" "gray"

        attrs =
            case action of
                CanActivate ->
                    [ onClick (ActivateCard idx) ]

                CanDiscard ->
                    [ onClick (DiscardCard idx) ]

                CanCover ->
                    [ onClick (CoverCard idx) ]

                Ignore ->
                    [ disabled True ]
    in
    button
        (attrs ++ css)
        [ Html.text card ]


playerNeedCard : Html GameMsg
playerNeedCard =
    let
        instructions =
            div [] [ Html.text "click a card above" ]
    in
    div [] [ instructions ]


cheatSheetView : Player -> Html GameMsg
cheatSheetView player =
    let
        cards =
            cheatSheetCards player
    in
    if Set.isEmpty cards then
        div [] []

    else
        let
            title =
                b [] [ Html.text "Cheat sheet:" ]

            cardHint card =
                let
                    hint =
                        card ++ " - " ++ hintForCard card
                in
                div [] [ Html.text hint ]

            cardHints =
                cards
                    |> Set.toList
                    |> List.sortBy cardValue
                    |> List.map cardHint
                    |> div []
        in
        div []
            [ br [] []
            , title
            , cardHints
            ]


playerNeedStart : PlayType -> Color -> Html GameMsg
playerNeedStart playType color =
    let
        instructions =
            case playType of
                PlayCard _ ->
                    "click a piece to start move"

                FinishSeven count ->
                    "click a piece to finish split (moving " ++ String.fromInt count ++ ")"

        activeCard =
            getCardForPlayType playType
    in
    div []
        [ activeCardView activeCard color instructions
        ]


playerNeedEnd : PlayType -> Color -> Html GameMsg
playerNeedEnd playType color =
    let
        activeCard =
            getCardForPlayType playType

        instructions =
            "now click piece's end location"
    in
    div []
        [ activeCardView activeCard color instructions
        ]


activeCardView : Card -> Color -> String -> Html GameMsg
activeCardView activeCard color instructions =
    let
        css =
            [ style "color" color
            , style "padding" "4px"
            , style "margin" "5px"
            , style "font-size" "110%"
            ]

        card =
            b css [ Html.text activeCard ]
    in
    span [] [ card, Html.text instructions ]


cardCss : Color -> Color -> List (Html.Attribute GameMsg)
cardCss borderColor color =
    [ style "border-color" borderColor
    , style "color" color
    , style "background" "white"
    , style "padding" "4px"
    , style "margin" "3px"
    , style "font-size" "110%"
    , style "min-width" "30px"
    ]


creditsView : Player -> Html GameMsg
creditsView player =
    if player.getOutCredits > 0 then
        let
            credits =
                String.fromInt player.getOutCredits
        in
        div []
            [ Html.text "You have "
            , b [] [ Html.text credits ]
            , Html.text " credits (you need 5 to get out)"
            ]

    else
        span [] []


rotateButtonView : Player -> List (Html GameMsg)
rotateButtonView player =
    case
        player.turn
    of
        TurnDone ->
            [ button
                [ onClick RotateBoard
                , style "background" "lightgreen"
                ]
                [ Html.text "done" ]
            ]

        _ ->
            []
