module View exposing (view)

import Browser
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
import Set
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
        , Location
        , Model
        , MoveType(..)
        , Msg(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , Turn(..)
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.state of
        Loading ->
            -- The load should basically happen instantly, so this
            -- is just defensive against race conditions.  Of course,
            -- this may change in the future if we do things like
            -- connect to a server.
            { title = "Fast Track"
            , body = [ Html.text "loading..." ]
            }

        Ready ->
            normalView model


normalView : Model -> Browser.Document Msg
normalView model =
    let
        pieceMap =
            model.pieceMap

        zoneColors =
            model.zoneColors

        players =
            model.players

        activeColor =
            model.getActiveColor zoneColors

        activePlayer =
            getPlayer players activeColor

        board =
            div
                []
                [ boardView pieceMap zoneColors activePlayer activeColor ]

        playerConsole =
            playerView activePlayer activeColor

        body =
            [ board
            , hr [] []
            , playerConsole
            ]
    in
    { title = "Fast Track"
    , body = body
    }


boardView : PieceDict -> List Color -> Player -> Color -> Html Msg
boardView pieceMap zoneColors activePlayer activeColor =
    let
        startLocation =
            getStartLocation activePlayer

        startLocs =
            startLocsForPlayer activePlayer

        endLocs =
            endLocsForPlayer activePlayer

        content =
            List.map (zoneView pieceMap startLocs endLocs activeColor startLocation) zoneColors
                |> makePolygon panelWidth panelHeight
                |> nudge

        sideCount =
            List.length zoneColors

        centerOffset =
            getCenterOffset sideCount panelWidth panelHeight

        boardSize =
            String.fromFloat (2 * centerOffset + 3 * squareSize)
    in
    svg
        [ width boardSize, height boardSize ]
        [ content ]


panelWidth : Float
panelWidth =
    4 * squareSize


panelHeight : Float
panelHeight =
    5 * squareSize


nudge : Svg.Svg Msg -> Svg.Svg Msg
nudge board =
    let
        offset =
            String.fromFloat squareSize

        translate =
            "translate(" ++ offset ++ " " ++ offset ++ ")"
    in
    g [ transform translate ] [ board ]


zoneView : PieceDict -> Set.Set PieceLocation -> Set.Set PieceLocation -> Color -> Maybe PieceLocation -> Color -> Html Msg
zoneView pieceMap startLocs endLocs activeColor startLocation zoneColor =
    let
        locations =
            configLocations

        drawnLocations =
            List.map (locationView pieceMap zoneColor startLocs endLocs activeColor startLocation) locations
    in
    g [] drawnLocations


locationView : PieceDict -> String -> Set.Set PieceLocation -> Set.Set PieceLocation -> Color -> Maybe PieceLocation -> Location -> Html Msg
locationView pieceMap zoneColor startLocs endLocs activeColor selectedLocation locationInfo =
    let
        id =
            locationInfo.id

        pieceLocation =
            ( zoneColor, id )

        w =
            squareSize - gutterSize

        h =
            squareSize - gutterSize

        radius =
            w / 2

        cx_ =
            locationInfo.x * squareSize

        cy_ =
            panelHeight - (locationInfo.y * squareSize)

        xpos =
            cx_ - w / 2

        ypos =
            cy_ - h / 2

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


pieceView : Color -> Bool -> Bool -> Float -> Float -> List (Svg.Attribute Msg) -> Html Msg
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


playerView : Player -> Color -> Html Msg
playerView player color =
    let
        playableCards =
            getPlayableCards player

        handCards =
            List.indexedMap (handCardView color player playableCards) player.hand

        hand =
            span [] handCards

        deck =
            deckView player color

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
        [ span [] [ hand, deck ]
        , creditsView player
        , console
        ]


type CardAction
    = CanActivate
    | CanDiscard
    | CanCover
    | Ignore


handCardView : Color -> Player -> Set.Set Card -> Int -> Card -> Html Msg
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
                    [ onClick (ActivateCard color idx) ]

                CanDiscard ->
                    [ onClick (DiscardCard color idx) ]

                CanCover ->
                    [ onClick (CoverCard color idx) ]

                Ignore ->
                    [ disabled True ]
    in
    button
        (attrs ++ css)
        [ Html.text card ]


playerNeedCard : Set.Set Card -> Html Msg
playerNeedCard playableCards =
    let
        instructions =
            div [] [ Html.text "click a card above" ]
    in
    div [] [ instructions, cheatSheet playableCards ]


cheatSheet : Set.Set Card -> Html Msg
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


playerNeedStart : PlayType -> Color -> Html Msg
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


playerNeedEnd : PlayType -> Color -> Html Msg
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


activeCardView : Card -> Color -> String -> Html Msg
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


deckView : Player -> Color -> Html Msg
deckView player color =
    let
        handCount =
            List.length player.hand
    in
    if (player.turn == TurnDone) && (handCount < 5) then
        let
            css =
                cardCss color color

            attrs =
                [ onClick ReplenishHand ]
        in
        button
            (attrs ++ css)
            [ Html.text "Deck" ]

    else
        span [] []


cardCss : Color -> Color -> List (Html.Attribute Msg)
cardCss borderColor color =
    [ style "border-color" borderColor
    , style "color" color
    , style "background" "white"
    , style "padding" "4px"
    , style "margin" "3px"
    , style "font-size" "110%"
    , style "min-width" "30px"
    ]


creditsView : Player -> Html Msg
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


rotateButton : Html Msg
rotateButton =
    div
        []
        [ button
            [ onClick RotateBoard ]
            [ Html.text "Finish Turn" ]
        ]
