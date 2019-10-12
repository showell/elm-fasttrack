module View exposing (view)

import Browser
import Config
    exposing
        ( config_locations
        , gutter_size
        , is_base_id
        , is_holding_pen_id
        , square_size
        )
import Html
    exposing
        ( Html
        , b
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
        ( get_card_for_play_type
        )
import Piece
    exposing
        ( get_piece
        )
import Player
    exposing
        ( end_locs_for_player
        , get_playable_cards
        , get_player
        , get_start_location
        , start_locs_for_player
        )
import Polygon
    exposing
        ( get_center_offset
        , make_polygon
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
            normal_view model


normal_view : Model -> Browser.Document Msg
normal_view model =
    let
        piece_map =
            model.piece_map

        zone_colors =
            model.zone_colors

        players =
            model.players

        active_color =
            model.get_active_color zone_colors

        active_player =
            get_player players active_color

        board =
            div
                []
                [ board_view piece_map zone_colors active_player active_color ]

        player_console =
            player_view active_player active_color

        body =
            [ board
            , hr [] []
            , player_console
            ]
    in
    { title = "Fast Track"
    , body = body
    }


board_view : PieceDict -> List Color -> Player -> Color -> Html Msg
board_view piece_map zone_colors active_player active_color =
    let
        start_location =
            get_start_location active_player

        start_locs =
            start_locs_for_player active_player

        end_locs =
            end_locs_for_player active_player

        content =
            List.map (zone_view piece_map start_locs end_locs active_color start_location) zone_colors
                |> make_polygon panel_width panel_height
                |> nudge

        side_count =
            List.length zone_colors

        center_offset =
            get_center_offset side_count panel_width panel_height

        board_size =
            String.fromFloat (2 * center_offset + 3 * square_size)
    in
    svg
        [ width board_size, height board_size ]
        [ content ]


panel_width : Float
panel_width =
    4 * square_size


panel_height : Float
panel_height =
    5 * square_size


nudge : Svg.Svg Msg -> Svg.Svg Msg
nudge board =
    let
        offset =
            String.fromFloat square_size

        translate =
            "translate(" ++ offset ++ " " ++ offset ++ ")"
    in
    g [ transform translate ] [ board ]


zone_view : PieceDict -> Set.Set PieceLocation -> Set.Set PieceLocation -> Color -> Maybe PieceLocation -> Color -> Html Msg
zone_view piece_map start_locs end_locs active_color start_location zone_color =
    let
        locations =
            config_locations

        drawn_locations =
            List.map (location_view piece_map zone_color start_locs end_locs active_color start_location) locations
    in
    g [] drawn_locations


location_view : PieceDict -> String -> Set.Set PieceLocation -> Set.Set PieceLocation -> Color -> Maybe PieceLocation -> Location -> Html Msg
location_view piece_map zone_color start_locs end_locs active_color selected_location location_info =
    let
        id =
            location_info.id

        piece_location =
            ( zone_color, id )

        w =
            square_size - gutter_size

        h =
            square_size - gutter_size

        radius =
            w / 2

        cx_ =
            location_info.x * square_size

        cy_ =
            panel_height - (location_info.y * square_size)

        xpos =
            cx_ - w / 2

        ypos =
            cy_ - h / 2

        my_piece =
            get_piece piece_map piece_location

        is_me =
            my_piece == Just active_color

        is_selected_piece =
            case
                selected_location
            of
                Just location ->
                    piece_location == location

                Nothing ->
                    False

        is_start_loc =
            Set.member piece_location start_locs

        is_reachable =
            Set.member piece_location end_locs

        fill_color =
            if is_selected_piece then
                "lightblue"

            else if is_start_loc then
                "lightcyan"

            else if is_reachable then
                "lightgreen"

            else if is_me then
                "mintcream"

            else
                "white"

        stroke_color =
            if is_start_loc then
                "black"

            else
                zone_color

        is_rect =
            is_holding_pen_id id || is_base_id id

        loc_handlers =
            if is_start_loc then
                [ onClick (SetStartLocation piece_location)
                ]

            else if is_reachable then
                [ onClick (SetEndLocation piece_location)
                ]

            else
                []

        s_location =
            if is_rect then
                rect
                    ([ x (String.fromFloat xpos)
                     , y (String.fromFloat ypos)
                     , fill fill_color
                     , stroke stroke_color
                     , width (String.fromFloat w)
                     , height (String.fromFloat h)
                     , rx "2"
                     ]
                        ++ loc_handlers
                    )
                    []

            else
                circle
                    ([ cx (String.fromFloat cx_)
                     , cy (String.fromFloat cy_)
                     , fill fill_color
                     , stroke stroke_color
                     , r (String.fromFloat radius)
                     ]
                        ++ loc_handlers
                    )
                    []

        s_pieces =
            case my_piece of
                Just piece_color ->
                    [ piece_view piece_color is_selected_piece is_start_loc cx_ cy_ loc_handlers ]

                Nothing ->
                    []

        contents =
            s_location :: s_pieces
    in
    g [] contents


piece_view : Color -> Bool -> Bool -> Float -> Float -> List (Svg.Attribute Msg) -> Html Msg
piece_view color is_selected_piece is_start_loc cx_ cy_ handlers =
    let
        radius =
            if is_selected_piece then
                "7"

            else if is_start_loc then
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


player_view : Player -> Color -> Html Msg
player_view player color =
    let
        playable_cards =
            get_playable_cards player

        hand_cards =
            List.indexedMap (hand_card_view color player playable_cards) player.hand

        hand =
            span [] hand_cards

        deck =
            deck_view player color

        console =
            case player.turn of
                TurnNeedDiscard ->
                    div [] [ Html.text "click a card to discard" ]

                TurnNeedCard _ ->
                    div [] [ Html.text "click a card above" ]

                TurnNeedStartLoc turn_info ->
                    player_need_start turn_info.play_type color

                TurnNeedEndLoc turn_info ->
                    player_need_end turn_info.play_type color

                TurnDone ->
                    div
                        []
                        [ Html.text "ok, now finish your turn"
                        , rotate_button
                        ]

                _ ->
                    div [] []
    in
    div []
        [ span [] [ hand, deck ]
        , console
        ]


type CardAction
    = CanActivate
    | CanDiscard
    | Ignore


hand_card_view : Color -> Player -> Set.Set Card -> Int -> Card -> Html Msg
hand_card_view color player playable_cards idx card =
    let
        action =
            case player.turn of
                TurnNeedCard _ ->
                    -- ideally, we should put playable_cards on TurnNeedCard
                    if Set.member card playable_cards then
                        CanActivate

                    else
                        Ignore

                TurnNeedDiscard ->
                    CanDiscard

                _ ->
                    Ignore

        css =
            case action of
                CanActivate ->
                    card_css color color

                CanDiscard ->
                    card_css "gray" color

                Ignore ->
                    card_css "gray" "gray"

        attrs =
            case action of
                CanActivate ->
                    [ onClick (ActivateCard color idx) ]

                CanDiscard ->
                    [ onClick (DiscardCard color idx) ]

                Ignore ->
                    [ disabled True ]
    in
    button
        (attrs ++ css)
        [ Html.text card ]


player_need_start : PlayType -> Color -> Html Msg
player_need_start play_type color =
    let
        instructions =
            case play_type of
                PlayCard _ ->
                    "click a piece to start move"

                FinishSeven count ->
                    "click a piece to finish split (moving " ++ String.fromInt count ++ ")"

        active_card =
            get_card_for_play_type play_type
    in
    div []
        [ active_card_view active_card color instructions
        ]


player_need_end : PlayType -> Color -> Html Msg
player_need_end play_type color =
    let
        active_card =
            get_card_for_play_type play_type

        instructions =
            "now click piece's end location"
    in
    div []
        [ active_card_view active_card color instructions
        ]


active_card_view : Card -> Color -> String -> Html Msg
active_card_view active_card color instructions =
    let
        css =
            [ style "color" color
            , style "padding" "4px"
            , style "margin" "5px"
            , style "font-size" "110%"
            ]

        card =
            b css [ Html.text active_card ]
    in
    span [] [ card, Html.text instructions ]


deck_view : Player -> Color -> Html Msg
deck_view player color =
    let
        handCount =
            List.length player.hand
    in
    if (player.turn == TurnDone) && (handCount < 5) then
        let
            css =
                card_css color color

            attrs =
                [ onClick ReplenishHand ]
        in
        button
            (attrs ++ css)
            [ Html.text "Deck" ]

    else
        span [] []


card_css : Color -> Color -> List (Html.Attribute Msg)
card_css border_color color =
    [ style "border-color" border_color
    , style "color" color
    , style "background" "white"
    , style "padding" "4px"
    , style "margin" "3px"
    , style "font-size" "110%"
    , style "min-width" "30px"
    ]


rotate_button : Html Msg
rotate_button =
    div
        []
        [ button
            [ onClick RotateBoard ]
            [ Html.text "Finish Turn" ]
        ]
