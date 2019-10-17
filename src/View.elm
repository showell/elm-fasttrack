module View exposing (gameView)

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
import Set exposing (Set)
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
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , Turn(..)
        )



-- VIEW


gameView : Game -> List (Html GameMsg)
gameView game =
    let
        pieceMap =
            game.pieceMap

        zoneColors =
            game.zoneColors

        players =
            game.players

        activeColor =
            game.activeColor

        activePlayer =
            getPlayer players activeColor

        board =
            div
                []
                [ boardView pieceMap zoneColors activePlayer activeColor ]

        playerConsole =
            playerView activePlayer activeColor
    in
    [ board
    , hr [] []
    , playerConsole
    ]


boardView : PieceDict -> List Color -> Player -> Color -> Html GameMsg
boardView pieceMap zoneColors activePlayer activeColor =
    let
        startLocation =
            getStartLocation activePlayer

        startLocs =
            startLocsForPlayer activePlayer

        endLocs =
            endLocsForPlayer activePlayer

        zoneLocations =
            List.map (zoneView pieceMap startLocs endLocs activeColor startLocation) zoneColors
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


zoneView : PieceDict -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Color -> Html GameMsg
zoneView pieceMap startLocs endLocs activeColor startLocation zoneColor =
    let
        locations =
            configLocations

        drawnLocations =
            List.map (locationView pieceMap zoneColor startLocs endLocs activeColor startLocation) locations
    in
    g [] drawnLocations


locationView : PieceDict -> Color -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Location -> Html GameMsg
locationView pieceMap zoneColor startLocs endLocs activeColor selectedLocation locationInfo =
    let
        cx_ =
            locationInfo.x * squareSize

        cy_ =
            panelHeight - (locationInfo.y * squareSize)

        id =
            locationInfo.id
    in
    drawLocationAtCoords pieceMap zoneColor id startLocs endLocs activeColor selectedLocation cx_ cy_


bullsEyeView : PieceDict -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Float -> Html GameMsg
bullsEyeView pieceMap startLocs endLocs activeColor selectedLocation centerOffset =
    let
        cx_ =
            centerOffset

        cy_ =
            centerOffset

        zoneColor =
            "black"

        id =
            "bullseye"
    in
    drawLocationAtCoords pieceMap zoneColor id startLocs endLocs activeColor selectedLocation cx_ cy_


drawLocationAtCoords : PieceDict -> Color -> String -> Set PieceLocation -> Set PieceLocation -> Color -> Maybe PieceLocation -> Float -> Float -> Html GameMsg
drawLocationAtCoords pieceMap zoneColor id startLocs endLocs activeColor selectedLocation cx_ cy_ =
    let
        pieceLocation =
            ( zoneColor, id )

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


playerView : Player -> Color -> Html GameMsg
playerView player color =
    let
        playableCards =
            getPlayableCards player

        handCards =
            List.indexedMap (handCardView color player playableCards) player.hand

        hand =
            span [] handCards

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
                    playerNeedCard playableCards

                TurnNeedStartLoc turnInfo ->
                    playerNeedStart turnInfo.playType color

                TurnNeedEndLoc turnInfo ->
                    playerNeedEnd turnInfo.playType color

                TurnDone ->
                    div
                        []
                        [ Html.text "ok, now finish your turn"
                        , rotateButton
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


playerNeedCard : Set Card -> Html GameMsg
playerNeedCard playableCards =
    let
        instructions =
            div [] [ Html.text "click a card above" ]
    in
    div [] [ instructions, cheatSheet playableCards ]


cheatSheet : Set Card -> Html GameMsg
cheatSheet cards =
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
        , cheatSheet (Set.fromList [ activeCard ])
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


rotateButton : Html GameMsg
rotateButton =
    div
        []
        [ button
            [ onClick RotateBoard ]
            [ Html.text "Finish Turn" ]
        ]
