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
        ( get_moves_for_player
        )
import Piece
    exposing
        ( get_piece
        )
import Player
    exposing
        ( end_locs_for_player
        , get_card_for_play_type
        , get_player
        , get_player_cards
        , get_start_location
        , start_locs_for_player
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
        , CardStartEnd
        , Color
        , Location
        , Model
        , Msg(..)
        , PieceDict
        , PieceLocation
        , PlayType(..)
        , Player
        , PlayerDict
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

        cards =
            get_player_cards active_player

        moves =
            get_moves_for_player cards piece_map zone_colors active_color

        playable_cards =
            moves
                |> Set.map (\( card, _, _ ) -> card)

        board =
            div
                []
                [ board_view piece_map zone_colors players active_color moves ]

        player_console =
            player_view players active_color playable_cards

        body =
            [ board
            , hr [] []
            , player_console
            ]
    in
    { title = "Fast Track"
    , body = body
    }


board_view : PieceDict -> List Color -> PlayerDict -> Color -> Set.Set CardStartEnd -> Html Msg
board_view piece_map zone_colors players active_color moves =
    let
        active_player =
            get_player players active_color

        start_location =
            get_start_location active_player

        playable_locs =
            start_locs_for_player active_player piece_map zone_colors moves active_color

        reachable_locs =
            end_locs_for_player active_player piece_map zone_colors moves

        content =
            List.map (draw_zone piece_map playable_locs reachable_locs active_color start_location zone_colors) zone_colors

        board_size =
            String.fromFloat (2 * get_zone_height zone_colors + 3 * square_size)
    in
    svg
        [ width board_size, height board_size ]
        content


zone_index : Color -> List Color -> Int
zone_index x lst =
    case lst of
        [] ->
            -- should never happen, just appease compiler
            -1

        first :: rest ->
            if first == x then
                0

            else
                1 + zone_index x rest


get_angle : List Color -> Float
get_angle zone_colors =
    360.0 / toFloat (List.length zone_colors)


get_zone_height : List Color -> Float
get_zone_height zone_colors =
    let
        angle =
            get_angle zone_colors

        half_angle =
            angle / 2 |> degrees

        num_squares =
            5 + (2.0 / tan half_angle)
    in
    num_squares * square_size


draw_zone : PieceDict -> Set.Set PieceLocation -> Set.Set PieceLocation -> Color -> Maybe PieceLocation -> List Color -> Color -> Html Msg
draw_zone piece_map playable_locs reachable_locs active_color start_location zone_colors zone_color =
    let
        locations =
            config_locations

        idx =
            zone_index zone_color zone_colors

        angle =
            toFloat idx * get_angle zone_colors

        zone_height =
            get_zone_height zone_colors

        color =
            zone_color

        center =
            String.fromFloat (zone_height + square_size)

        translate =
            "translate(" ++ center ++ " " ++ center ++ ")"

        rotate =
            "rotate(" ++ String.fromFloat angle ++ ")"

        transform_ =
            translate ++ " " ++ rotate

        drawn_locations =
            List.map (location_view zone_height piece_map color playable_locs reachable_locs active_color start_location) locations
    in
    g [ transform transform_ ] drawn_locations


is_start_location : PieceLocation -> Maybe PieceLocation -> Bool
is_start_location loc start_location =
    case start_location of
        Nothing ->
            False

        Just active ->
            loc == active


location_view : Float -> PieceDict -> String -> Set.Set PieceLocation -> Set.Set PieceLocation -> Color -> Maybe PieceLocation -> Location -> Html Msg
location_view zone_height piece_map zone_color playable_locs reachable_locs active_color start_location location_info =
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
            zone_height - (location_info.y * square_size)

        xpos =
            cx_ - w / 2

        ypos =
            cy_ - h / 2

        my_piece =
            get_piece piece_map piece_location

        is_me =
            my_piece == Just active_color

        is_active =
            is_start_location piece_location start_location

        is_playable =
            Set.member piece_location playable_locs

        is_reachable =
            Set.member piece_location reachable_locs

        fill_color =
            if is_active then
                "lightblue"

            else if is_playable then
                "lightcyan"

            else if is_reachable then
                "lightgreen"

            else if is_me then
                "mintcream"

            else
                "white"

        stroke_color =
            if is_playable then
                "black"

            else
                zone_color

        is_rect =
            is_holding_pen_id id || is_base_id id

        loc_handlers =
            if is_playable then
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
                    [ piece_view piece_color is_active is_playable cx_ cy_ loc_handlers ]

                Nothing ->
                    []

        contents =
            s_location :: s_pieces
    in
    g [] contents


piece_view : Color -> Bool -> Bool -> Float -> Float -> List (Svg.Attribute Msg) -> Html Msg
piece_view color is_active is_playable cx_ cy_ handlers =
    let
        radius =
            if is_active then
                "7"

            else if is_playable then
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


card_css : Color -> List (Html.Attribute Msg)
card_css color =
    [ style "border-color" color
    , style "background" "white"
    , style "color" color
    , style "padding" "4px"
    , style "margin" "3px"
    , style "font-size" "110%"
    , style "min-width" "30px"
    ]


view_hand_card : Color -> Player -> Set.Set Card -> Int -> Card -> Html Msg
view_hand_card color player playable_cards idx card =
    let
        enabled =
            player.turn == TurnNeedCard && Set.member card playable_cards

        border_color =
            if enabled then
                color

            else
                "gray"

        css =
            card_css border_color

        attrs =
            case player.turn of
                TurnNeedCard ->
                    [ onClick (ActivateCard color idx) ]

                _ ->
                    [ disabled True ]
    in
    button
        (attrs ++ css)
        [ Html.text card ]


deck_view : Player -> Color -> Html Msg
deck_view player color =
    let
        handCount =
            List.length player.hand
    in
    if (player.turn == TurnDone) && (handCount < 5) then
        let
            css =
                card_css color

            attrs =
                [ onClick ReplenishHand ]
        in
        button
            (attrs ++ css)
            [ Html.text "Deck" ]

    else
        span [] []


player_view : PlayerDict -> Color -> Set.Set Card -> Html Msg
player_view players color playable_cards =
    let
        player =
            get_player players color

        deck =
            deck_view player color

        hand_cards =
            List.indexedMap (view_hand_card color player playable_cards) player.hand

        hand =
            span [] hand_cards

        console =
            case player.turn of
                TurnNeedCard ->
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


rotate_button : Html Msg
rotate_button =
    div
        []
        [ button
            [ onClick RotateBoard ]
            [ Html.text "Finish Turn" ]
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


player_need_start : PlayType -> Color -> Html Msg
player_need_start play_type color =
    let
        active_card =
            get_card_for_play_type play_type

        -- We will get rid of this once we have a new state for discards.
        finish_button =
            button
                [ onClick (FinishCard color) ]
                [ Html.text "Done" ]
    in
    div []
        [ active_card_view active_card color "click a piece to start move"
        , div [] [ finish_button ]
        ]


player_need_end : PlayType -> Color -> Html Msg
player_need_end play_type color =
    let
        active_card =
            get_card_for_play_type play_type
    in
    div []
        [ active_card_view active_card color "now click piece's new location"
        ]
